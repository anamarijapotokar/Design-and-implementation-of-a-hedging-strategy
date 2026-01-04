library(readr)
library(dplyr)
library(lubridate)
library(tibble)
# PART 5: Hedging with futures

# Assumptions / units:
# - copper.csv spot prices look like USD per metric ton (values ~ 8,000–10,000)
# - copper_futures.csv (HG=F) is typically USD per pound
#   => convert futures to USD per metric ton via lb_per_ton
lb_per_ton = 2204.62262185
contract_size_lb  = 25000 # 25k pounds of copper per contract
contract_size_ton = contract_size_lb / lb_per_ton # the same contract but expressed in metric tons bc thats the metric in copper.csv

futures = read_csv(
  "copper_futures.csv",
  skip = 3,
  col_names = c("Date","Close","High","Low","Open","Volume"),
  show_col_types = FALSE
) %>%
  mutate(
    Date = as.Date(Date),
    Close = as.numeric(Close),
    Fut_USD_per_ton = Close * lb_per_ton
  ) %>%
  select(Date, Fut_USD_per_ton)

if (!exists("final_data2")) stop("final_data2 not found. Run source('ziva.r') first (and ensure it finishes without errors).")

if (is.character(final_data2$Date)) {
  final_data2 = final_data2 %>% mutate(Date = dmy(Date))
} else if (!inherits(final_data2$Date, "Date")) {
  final_data2 = final_data2 %>% mutate(Date = as.Date(Date))
}

# 2) Join futures onto your scenario table
df_hedge = final_data2 %>%
  inner_join(futures, by = "Date") %>%
  arrange(Date)

# 3) Compute changes for hedge ratio estimation
# Material_cost_t is already in USD (Price_USD * k_factor)
df_hedge = df_hedge %>%
  mutate(
    dCM = Material_cost_t - lag(Material_cost_t),     # USD change in material costs
    dF  = Fut_USD_per_ton - lag(Fut_USD_per_ton)      # USD/ton change in futures price
  ) %>%
  filter(!is.na(dCM), !is.na(dF))

# 4) Optimal hedge ratio (minimum-variance hedge)
# h* = Cov(dCM, dF) / Var(dF)
h_star_ton = cov(df_hedge$dCM, df_hedge$dF) / var(df_hedge$dF)

# number of futures contracts (rough implementation size)
N_contracts = h_star_ton / contract_size_ton

cat("\n--- Hedge ratio using actual copper futures ---\n")
cat("Optimal hedge quantity h* (metric tons):", round(h_star_ton, 2), "\n")
cat("Contract size (metric tons):", round(contract_size_ton, 4), "\n")
cat("Approx. number of contracts:", round(N_contracts, 0), "\n")

# 5) Apply hedge P&L to EBIT
# Company is effectively LONG copper (it must buy it). To hedge rising prices:
# take a LONG futures position -> P&L_t = h* * dF_t  (positive when futures rise)
df_hedge = df_hedge %>%
  mutate(
    hedgePnL = h_star_ton * dF,
    EBIT_a_hedged         = EBIT_a         + hedgePnL,
    EBIT_b_rho_1_hedged   = EBIT_b_rho_1   + hedgePnL,
    EBIT_b_rho_2_hedged   = EBIT_b_rho_2   + hedgePnL
  )

ebit_var_etl = function(ebit_vec, alpha = 0.05) {
  ebit_vec = ebit_vec[!is.na(ebit_vec)]
  EBIT_VaR = as.numeric(quantile(ebit_vec, probs = alpha))
  EBIT_ETL = mean(ebit_vec[ebit_vec <= EBIT_VaR], na.rm = TRUE)
  tibble(alpha = alpha, EBIT_VaR = EBIT_VaR, EBIT_ETL = EBIT_ETL)
}

alpha = 0.05

risk_unhedged = bind_rows(
  ebit_var_etl(df_hedge$EBIT_a, alpha)       %>% mutate(case = "a) fixed sales",        strategy = "unhedged"),
  ebit_var_etl(df_hedge$EBIT_b_rho_1, alpha) %>% mutate(case = "b) rho = 0.74",         strategy = "unhedged"),
  ebit_var_etl(df_hedge$EBIT_b_rho_2, alpha) %>% mutate(case = "b) rho = 0.92",         strategy = "unhedged")
)

risk_hedged_fut = bind_rows(
  ebit_var_etl(df_hedge$EBIT_a_hedged, alpha)       %>% mutate(case = "a) fixed sales", strategy = "hedged (copper futures)"),
  ebit_var_etl(df_hedge$EBIT_b_rho_1_hedged, alpha) %>% mutate(case = "b) rho = 0.74",  strategy = "hedged (copper futures)"),
  ebit_var_etl(df_hedge$EBIT_b_rho_2_hedged, alpha) %>% mutate(case = "b) rho = 0.92",  strategy = "hedged (copper futures)")
)

risk_part5_futures = bind_rows(risk_unhedged, risk_hedged_fut) %>%
  mutate(across(c(EBIT_VaR, EBIT_ETL), ~ .x/1e6)) %>%  # show in USD millions
  select(case, strategy, alpha, EBIT_VaR, EBIT_ETL)

cat("\n--- VaR/ETL comparison (USD millions) ---\n")
print(risk_part5_futures)

# PART 5 second part: Derivative with corr = 0.75 to CM

# Create a synthetic derivative price series with corr ~ 0.75 to dCM
# then compute hedge ratio again and re-run VaR/ETL.
set.seed(42)
rho_deriv = 0.75

# epsilon with same sd as dCM so the constructed series has theoretical corr rho
eps = rnorm(nrow(df_hedge), mean = 0, sd = sd(df_hedge$dCM))
dD = rho_deriv * df_hedge$dCM + sqrt(1 - rho_deriv^2) * eps

# Build a "derivative price" path (level doesn't matter for hedge ratio, but we create it anyway)
D0 = first(df_hedge$Fut_USD_per_ton)  # anchor at futures level (USD/ton)
D_price = D0 + cumsum(dD / h_star_ton)  # scale so changes are roughly comparable in USD/ton

df_hedge = df_hedge %>%
  mutate(
    Deriv_USD_per_ton = D_price,
    dDeriv = Deriv_USD_per_ton - lag(Deriv_USD_per_ton)
  ) %>%
  filter(!is.na(dDeriv))

# Optimal hedge ratio vs synthetic derivative
h_star_deriv_ton = cov(df_hedge$dCM, df_hedge$dDeriv) / var(df_hedge$dDeriv)
N_contracts_deriv = h_star_deriv_ton / contract_size_ton

cat("\n--- Hedge ratio using synthetic derivative (corr ~ 0.75 to dCM) ---\n")
cat("Sample corr(dCM, dDeriv):", round(cor(df_hedge$dCM, df_hedge$dDeriv), 3), "\n")
cat("Optimal hedge quantity h* (metric tons):", round(h_star_deriv_ton, 2), "\n")
cat("Approx. number of contracts:", round(N_contracts_deriv, 0), "\n")

df_hedge = df_hedge %>%
  mutate(
    hedgePnL_deriv = h_star_deriv_ton * dDeriv,
    EBIT_a_hedged_deriv       = EBIT_a       + hedgePnL_deriv,
    EBIT_b_rho_1_hedged_deriv = EBIT_b_rho_1 + hedgePnL_deriv,
    EBIT_b_rho_2_hedged_deriv = EBIT_b_rho_2 + hedgePnL_deriv
  )

risk_hedged_deriv = bind_rows(
  ebit_var_etl(df_hedge$EBIT_a_hedged_deriv, alpha)       %>% mutate(case = "a) fixed sales", strategy = "hedged (deriv corr~0.75)"),
  ebit_var_etl(df_hedge$EBIT_b_rho_1_hedged_deriv, alpha) %>% mutate(case = "b) rho = 0.74",  strategy = "hedged (deriv corr~0.75)"),
  ebit_var_etl(df_hedge$EBIT_b_rho_2_hedged_deriv, alpha) %>% mutate(case = "b) rho = 0.92",  strategy = "hedged (deriv corr~0.75)")
)

risk_part5_deriv = bind_rows(risk_unhedged, risk_hedged_deriv) %>%
  mutate(across(c(EBIT_VaR, EBIT_ETL), ~ .x/1e6)) %>%
  select(case, strategy, alpha, EBIT_VaR, EBIT_ETL)

cat("\n--- VaR/ETL comparison with synthetic derivative (USD millions) ---\n")
print(risk_part5_deriv)


# PART 6: Correlation sensitivity (derivative instrument)

# We vary corr between cost of materials and derivative instrument: 0.60, 0.75, 0.90 and re-compute optimal hedge ratio + VaR/ETL on EBIT for a) and b).

rho_set = c(0.60, 0.75, 0.90)

# Safety checks
if (!exists("final_data2")) stop("final_data2 not found. Source ziva.r first.")
if (!exists("futures"))    stop("futures not found. Run Part 5 futures import first.")
if (!exists("ebit_var_etl")) stop("ebit_var_etl() not found. Keep the Part 5 function definition.")

# avoid shit from p5 filtering
df_base = final_data2 %>%
  inner_join(futures, by = "Date") %>%
  arrange(Date) %>%
  mutate(
    dCM = Material_cost_t - lag(Material_cost_t),   # USD change in material costs
    dF  = Fut_USD_per_ton - lag(Fut_USD_per_ton)    # USD/ton change in futures (used only for scaling)
  ) %>%
  filter(!is.na(dCM), !is.na(dF))

alpha = 0.05

# Unhedged risk
risk_unhedged_base = bind_rows(
  ebit_var_etl(df_base$EBIT_a, alpha) %>% 
  mutate(case = "a) fixed sales", strategy = "unhedged"),
  ebit_var_etl(df_base$EBIT_b_rho_1, alpha) %>%
  mutate(case = "b) rho = 0.74",  strategy = "unhedged"),
  ebit_var_etl(df_base$EBIT_b_rho_2, alpha) %>% 
  mutate(case = "b) rho = 0.92",  strategy = "unhedged")
) %>%
  mutate(rho_deriv = NA_real_)

# Create synthetic derivative *changes* dDeriv with (approximately exact) sample corr = rho to dCM
# We standardize dCM, orthogonalize noise to dCM, then mix.
make_dDeriv = function(dCM, sd_target, rho, seed = 42) {
  set.seed(seed + round(rho * 1000))

  zCM = as.numeric(scale(dCM, center = TRUE, scale = TRUE))

  eps = rnorm(length(zCM))
  # Remove projection of eps on zCM => sample covariance ~ 0
  eps_orth = eps - (sum(eps * zCM) / sum(zCM^2)) * zCM
  eps_orth = eps_orth / sd(eps_orth)

  zDer = rho * zCM + sqrt(1 - rho^2) * eps_orth
  dDeriv = zDer * sd_target   # scale to USD/ton magnitude comparable to futures moves

  return(dDeriv)
}

# We scale derivative changes to have volatility similar to futures (USD/ton), so hedge ratios are comparable across rho.
sd_target = sd(df_base$dF)

# Loop over rho values
risk_hedged_list = list()
eff_list = list()

for (rho in rho_set) {

  dDeriv = make_dDeriv(df_base$dCM, sd_target, rho, seed = 42)

  # Hedge ratio (tons): h* = Cov(dCM, dDeriv) / Var(dDeriv)
  h_star_ton = cov(df_base$dCM, dDeriv) / var(dDeriv)

  # Hedge effectiveness on material-cost changes:
  # residual = dCM - h*dDeriv  (min-variance residual)
  resid = df_base$dCM - h_star_ton * dDeriv
  hedge_eff = 1 - var(resid) / var(df_base$dCM)

  # Apply hedge P&L to EBIT
  hedgePnL = h_star_ton * dDeriv

  EBIT_a_h = df_base$EBIT_a + hedgePnL
  EBIT_b_074_h = df_base$EBIT_b_rho_1 + hedgePnL
  EBIT_b_092_h = df_base$EBIT_b_rho_2 + hedgePnL

  risk_hedged = bind_rows(
    ebit_var_etl(EBIT_a_h, alpha) %>% 
    mutate(case = "a) fixed sales"),
    ebit_var_etl(EBIT_b_074_h, alpha) %>% 
    mutate(case = "b) rho = 0.74"),
    ebit_var_etl(EBIT_b_092_h, alpha) %>% 
    mutate(case = "b) rho = 0.92")
  ) %>%
    mutate(
      strategy  = paste0("hedged (deriv corr~", rho, ")"),
      rho_deriv = rho
    )

  risk_hedged_list[[as.character(rho)]] = risk_hedged

  eff_list[[as.character(rho)]] = tibble(
    rho_deriv = rho,
    sample_corr = cor(df_base$dCM, dDeriv),
    h_star_ton = h_star_ton,
    hedge_effectiveness = hedge_eff
  )
}

risk_part6 = bind_rows(risk_unhedged_base, bind_rows(risk_hedged_list)) %>%
  mutate(across(c(EBIT_VaR, EBIT_ETL), ~ .x / 1e6)) %>%   # USD millions
  select(case, strategy, rho_deriv, alpha, EBIT_VaR, EBIT_ETL)

eff_part6 = bind_rows(eff_list) %>%
  mutate(
    h_star_ton = as.numeric(h_star_ton),
    hedge_effectiveness = as.numeric(hedge_effectiveness)
  )

cat("\n--- PART 6: VaR/ETL vs derivative correlation (USD millions) ---\n")
print(risk_part6)

cat("\n--- PART 6: Hedge efficiency diagnostics (cost-change variance reduction) ---\n")
print(eff_part6)