library(readr)
library(dplyr)
library(lubridate)
library(tibble)

# -----------------------------
# PART 5: Hedging with futures
# -----------------------------
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

# reuse your risk function (if not in environment, re-define it)
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

# -------------------------------------------------------
# PART 5 (second part): Derivative with corr = 0.75 to CM
# -------------------------------------------------------
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


