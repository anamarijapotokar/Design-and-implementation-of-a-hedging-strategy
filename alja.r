library(readr)
library(dplyr)
library(tibble)
library(readxl)
library(zoo)

source("ziva.r")
source("ana.r")


# PART 7: Transition to a EU based company

S_0_EUR <- 920000000
OH_EUR <- 77000000

material_cost_0_EUR <- S_0_EUR * 0.69

fx_data <- read_csv("fx_rates_monthly.csv", show_col_types = FALSE)

copper_price_eur <- copper_data$Price_USD / fx_data$FX_Rate

copper_eur <- as.data.frame(copper_price_eur)
colnames(copper_eur) <- paste0("Price_EUR")
copper_eur$Date <- fx_data$Date
copper_eur <- copper_eur %>% select(Date, everything())

# P_C_0 is the last closing price from the data (initial copper price)
P_C_0_EUR <- last(copper_eur$Price_EUR)
print(paste("Current Copper price (EUR):", format(P_C_0_EUR)))

FX_0 <- last(fx_data$FX_Rate)
print(paste("Current FX rate:", format(FX_0)))

k_factor_eur <- material_cost_0_EUR / P_C_0_EUR

copper_eur <- copper_eur %>%
  mutate(
    Material_cost_t_EUR = Price_EUR * k_factor_eur,  # CM_t = P_C,t * k
    Log_Returns_Mat = c(NA, diff(log(Material_cost_t_EUR))),
    Log_Returns_Price = c(NA, diff(log(Price_EUR)))
    ) %>%
  filter(!is.na(Log_Returns_Mat)) #%>%
  #select(Date, Material_cost_t, Log_Returns_mat)


# 2.

log_returns_mat_eur <- copper_eur$Log_Returns_Mat
N_a <- length(log_returns_mat_eur)

set.seed(42)
epsilon_a <- rnorm(N_a, mean = 0, sd = sd(log_returns_mat_eur)) 

sales_returns_eur <- data.frame(Date = copper_eur$Date)

# a. Non-correlated sales - fixed sales

sales_returns_eur <- sales_returns_eur %>%
  mutate(
    R_Sales_eur_a = 0  # No returns since sales are fixed
  )

# b. Correlated sales (rho_1, rho_2)

rho_1 <- 0.74  # correlation 1
rho_2 <- 0.92  # correlation 2

# (funkcija iz ziva.r)

sales_returns_eur <- sales_returns_eur %>%
  mutate(
    R_Sales_eur_b1 = calculate_correlated_returns(log_returns_mat_eur, epsilon_a, rho_1),
    R_Sales_eur_b2 = calculate_correlated_returns(log_returns_mat_eur, epsilon_a, rho_2)
  )

# sales (funkcija iz ziva.r)

sales_eur <- data.frame(Date = copper_eur$Date)

sales_eur <- sales_eur %>%
  mutate(
    Sales_eur_a = S_0_EUR,  # Fixed sales
    Sales_eur_b1 = returns_to_price_R(S_0_EUR, sales_returns_eur$R_Sales_eur_b1),
    Sales_eur_b2 = returns_to_price_R(S_0_EUR, sales_returns_eur$R_Sales_eur_b2)
  )

final_data_eur <- copper_eur %>%
left_join(sales_eur, by = "Date")


# 3. EBIT

final_data2_eur <- final_data_eur %>%
  mutate(
    EBIT_eur_a = Sales_eur_a - Material_cost_t_EUR - 0.06*Sales_eur_a - 0.14*Sales_eur_a - OH_EUR,
    EBIT_eur_b1 = Sales_eur_b1 - Material_cost_t_EUR - 0.06*Sales_eur_b1 - 0.14*Sales_eur_b1 - OH_EUR,
    EBIT_eur_b2 = Sales_eur_b2 - Material_cost_t_EUR - 0.06*Sales_eur_b2 - 0.14*Sales_eur_b2 - OH_EUR
  )

EBIT_eur <- final_data2_eur %>%
  summarise(
    EBIT_eur_a = mean(EBIT_eur_a, na.rm = TRUE),
    EBIT_eur_b1 = mean(EBIT_eur_b1, na.rm = TRUE), 
    EBIT_eur_b2 = mean(EBIT_eur_b2, na.rm = TRUE)
  )

OC_ratio <- 0.06
OC_0_EUR <- OC_ratio * S_0_EUR
DL_ratio <- 0.14
DL_0_EUR <- DL_ratio * S_0_EUR

EBIT_0_EUR <- S_0_EUR - material_cost_0_EUR - OC_0_EUR - DL_0_EUR - OH_EUR

EBIT_eur_a_last <- tail(final_data2_eur$EBIT_eur_a, 1)  # should equal EBIT_0 (≈24.2m)
EBIT_eur_b1_last <- tail(final_data2_eur$EBIT_eur_b1, 1) 
EBIT_eur_b2_last <- tail(final_data2_eur$EBIT_eur_b2, 1) 

print(paste("EBIT (EUR):"))
print(paste(EBIT_eur))


# 4.

risk_table_eur <- bind_rows(
  ebit_var_etl(final_data2_eur$EBIT_eur_a, alpha)  %>% mutate(case = "a) fixed sales"),
  ebit_var_etl(final_data2_eur$EBIT_eur_b1, alpha) %>% mutate(case = "b) rho = 0.74"),
  ebit_var_etl(final_data2_eur$EBIT_eur_b2, alpha) %>% mutate(case = "b) rho = 0.92")
) %>% select(case, alpha, EBIT_VaR, EBIT_ETL)


risk_table_m_eur <- risk_table_eur %>%
  mutate(across(c(EBIT_VaR, EBIT_ETL), ~ .x/1e6))


# 5.

df_hedge_eur <- final_data2_eur %>%
  inner_join(futures, by = "Date") %>%
  inner_join(fx_data, by = "Date") %>%
  arrange(Date)

df_hedge_eur <- df_hedge_eur %>%
  mutate(
    # Futures price in EUR terms
    Fut_EUR_per_ton = Fut_USD_per_ton / FX_Rate,
    
    # Changes in EUR (The "pain" and the "medicine" must be in the same currency)
    dCM_eur = Material_cost_t_EUR - lag(Material_cost_t_EUR),
    dF_eur  = Fut_EUR_per_ton - lag(Fut_EUR_per_ton)
  ) %>%
  filter(!is.na(dCM_eur), !is.na(dF_eur))

h_star_ton_eur <- cov(df_hedge_eur$dCM_eur, df_hedge_eur$dF_eur) / var(df_hedge_eur$dF_eur)

N_contracts_eur = h_star_ton_eur / contract_size_ton

cat("\n--- Hedge ratio using actual copper futures (EUR) ---\n")
cat("Optimal hedge quantity h* (metric tons):", round(h_star_ton_eur, 2), "\n")
cat("Contract size (metric tons):", round(contract_size_ton, 4), "\n")
cat("Approx. number of contracts:", round(N_contracts_eur, 0), "\n")

df_hedge_eur <- df_hedge_eur %>%
  mutate(
    hedgePnL_eur = h_star_ton_eur * dF_eur,
    EBIT_eur_a_hedged  = EBIT_eur_a  + hedgePnL_eur,
    EBIT_eur_b1_hedged = EBIT_eur_b1 + hedgePnL_eur,
    EBIT_eur_b2_hedged = EBIT_eur_b2 + hedgePnL_eur
  )

alpha <- 0.05

risk_unhedged_eur <- bind_rows(
  ebit_var_etl(df_hedge_eur$EBIT_eur_a, alpha)       %>% mutate(case = "a) fixed sales",        strategy = "unhedged"),
  ebit_var_etl(df_hedge_eur$EBIT_eur_b1, alpha) %>% mutate(case = "b) rho = 0.74",         strategy = "unhedged"),
  ebit_var_etl(df_hedge_eur$EBIT_eur_b2, alpha) %>% mutate(case = "b) rho = 0.92",         strategy = "unhedged")
)

risk_hedged_fut_eur <- bind_rows(
  ebit_var_etl(df_hedge_eur$EBIT_eur_a_hedged, alpha)       %>% mutate(case = "a) fixed sales", strategy = "hedged (copper futures)"),
  ebit_var_etl(df_hedge_eur$EBIT_eur_b1_hedged, alpha) %>% mutate(case = "b) rho = 0.74",  strategy = "hedged (copper futures)"),
  ebit_var_etl(df_hedge_eur$EBIT_eur_b2_hedged, alpha) %>% mutate(case = "b) rho = 0.92",  strategy = "hedged (copper futures)")
)

risk_part5_futures_eur <- bind_rows(risk_unhedged_eur, risk_hedged_fut_eur) %>%
  mutate(across(c(EBIT_VaR, EBIT_ETL), ~ .x/1e6)) %>%  # show in EUR millions
  select(case, strategy, alpha, EBIT_VaR, EBIT_ETL)

cat("\n--- VaR/ETL comparison (EUR millions) ---\n")
print(risk_part5_futures_eur)


eps_eur = rnorm(nrow(df_hedge_eur), mean = 0, sd = sd(df_hedge_eur$dCM_eur))
dD_eur = rho_deriv * df_hedge_eur$dCM_eur + sqrt(1 - rho_deriv^2) * eps_eur

D0_eur = first(df_hedge_eur$Fut_EUR_per_ton)  
D_price_eur = D0_eur + cumsum(dD_eur / h_star_ton_eur)

df_hedge_eur = df_hedge_eur %>%
  mutate(
    Deriv_EUR_per_ton = D_price_eur,
    dDeriv_eur = Deriv_EUR_per_ton - lag(Deriv_EUR_per_ton)
  ) %>%
  filter(!is.na(dDeriv_eur))

# Optimal hedge ratio vs synthetic derivative
h_star_deriv_ton_eur = cov(df_hedge_eur$dCM_eur, df_hedge_eur$dDeriv_eur) / var(df_hedge_eur$dDeriv_eur)
N_contracts_deriv_eur = h_star_deriv_ton_eur / contract_size_ton

cat("\n--- Hedge ratio using synthetic derivative (corr ~ 0.75 to dCM) (EUR) ---\n")
cat("Sample corr(dCM, dDeriv):", round(cor(df_hedge_eur$dCM_eur, df_hedge_eur$dDeriv_eur), 3), "\n")
cat("Optimal hedge quantity h* (metric tons):", round(h_star_deriv_ton_eur, 2), "\n")
cat("Approx. number of contracts:", round(N_contracts_deriv_eur, 0), "\n")

df_hedge_eur = df_hedge_eur %>%
  mutate(
    hedgePnL_deriv_eur = h_star_deriv_ton_eur * dDeriv_eur,
    EBIT_eur_a_hedged_deriv       = EBIT_eur_a       + hedgePnL_deriv_eur,
    EBIT_eur_b1_hedged_deriv = EBIT_eur_b1 + hedgePnL_deriv_eur,
    EBIT_eur_b2_hedged_deriv = EBIT_eur_b2 + hedgePnL_deriv_eur
  )

risk_hedged_deriv_eur = bind_rows(
  ebit_var_etl(df_hedge_eur$EBIT_eur_a_hedged_deriv, alpha)       %>% mutate(case = "a) fixed sales", strategy = "hedged (deriv corr~0.75)"),
  ebit_var_etl(df_hedge_eur$EBIT_eur_b1_hedged_deriv, alpha) %>% mutate(case = "b) rho = 0.74",  strategy = "hedged (deriv corr~0.75)"),
  ebit_var_etl(df_hedge_eur$EBIT_eur_b2_hedged_deriv, alpha) %>% mutate(case = "b) rho = 0.92",  strategy = "hedged (deriv corr~0.75)")
)

risk_part5_deriv_eur = bind_rows(risk_unhedged_eur, risk_hedged_deriv_eur) %>%
  mutate(across(c(EBIT_VaR, EBIT_ETL), ~ .x/1e6)) %>%
  select(case, strategy, alpha, EBIT_VaR, EBIT_ETL)

cat("\n--- VaR/ETL comparison with synthetic derivative (EUR millions) ---\n")
print(risk_part5_deriv_eur)


# 6.

df_base_eur <- df_hedge_eur %>%
  mutate(
    # Futures price in EUR terms
    Fut_EUR_per_ton = Fut_USD_per_ton / FX_Rate,
    
    # Changes in EUR (The "pain" and the "medicine" must be in the same currency)
    dCM_eur = Material_cost_t_EUR - lag(Material_cost_t_EUR),
    dF_eur  = Fut_EUR_per_ton - lag(Fut_EUR_per_ton)
  ) %>%
  filter(!is.na(dCM_eur), !is.na(dF_eur))

# Unhedged risk
risk_unhedged_base_eur = bind_rows(
  ebit_var_etl(df_base_eur$EBIT_eur_a, alpha) %>% 
  mutate(case = "a) fixed sales", strategy = "unhedged"),
  ebit_var_etl(df_base_eur$EBIT_eur_b1, alpha) %>%
  mutate(case = "b) rho = 0.74",  strategy = "unhedged"),
  ebit_var_etl(df_base_eur$EBIT_eur_b2, alpha) %>% 
  mutate(case = "b) rho = 0.92",  strategy = "unhedged")
) %>%
  mutate(rho_deriv = NA_real_)

sd_target_eur = sd(df_base_eur$dF_eur)


risk_hedged_list_eur = list()
eff_list_eur = list()

for (rho in rho_set) {

  dDeriv = make_dDeriv(df_base_eur$dCM_eur, sd_target_eur, rho, seed = 42)

  # Hedge ratio (tons): h* = Cov(dCM, dDeriv) / Var(dDeriv)
  h_star_ton_eur = cov(df_base_eur$dCM_eur, dDeriv) / var(dDeriv)

  # Hedge effectiveness on material-cost changes:
  # residual = dCM - h*dDeriv  (min-variance residual)
  resid = df_base_eur$dCM_eur - h_star_ton_eur * dDeriv
  hedge_eff = 1 - var(resid) / var(df_base_eur$dCM_eur)

  # Apply hedge P&L to EBIT
  hedgePnL = h_star_ton_eur * dDeriv

  EBIT_a_h = df_base_eur$EBIT_eur_a + hedgePnL
  EBIT_b_074_h = df_base_eur$EBIT_eur_b1 + hedgePnL
  EBIT_b_092_h = df_base_eur$EBIT_eur_b2 + hedgePnL

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

  risk_hedged_list_eur[[as.character(rho)]] = risk_hedged

  eff_list_eur[[as.character(rho)]] = tibble(
    rho_deriv = rho,
    sample_corr_eur = cor(df_base_eur$dCM_eur, dDeriv),
    h_star_ton_eur = h_star_ton_eur,
    hedge_effectiveness_eur = hedge_eff
  )
}

risk_part6_eur = bind_rows(risk_unhedged_base_eur, bind_rows(risk_hedged_list_eur)) %>%
  mutate(across(c(EBIT_VaR, EBIT_ETL), ~ .x / 1e6)) %>%   # EUR millions
  select(case, strategy, rho_deriv, alpha, EBIT_VaR, EBIT_ETL)

eff_part6_eur = bind_rows(eff_list_eur) %>%
  mutate(
    h_star_ton_eur = as.numeric(h_star_ton_eur),
    hedge_effectiveness_eur = as.numeric(hedge_effectiveness_eur)
  )

cat("\n--- PART 6: VaR/ETL vs derivative correlation (EUR millions) ---\n")
print(risk_part6_eur)

cat("\n--- PART 6: Hedge efficiency diagnostics (cost-change variance reduction) (EUR) ---\n")
print(eff_part6_eur)



# PART 8

sofr_6m <- read_csv("SOFR180DAYAVG.csv", show_col_types=FALSE) %>%
  rename(Date = observation_date, SOFR_6m = SOFR180DAYAVG)

# for missing data (before 2018) we use T_Bill
treasury_6m <- read_csv("TREASURY6M.csv", show_col_types=FALSE) %>%
  rename(Date = observation_date, T_Bill = DTB6)

df_sofr <- data.frame(
  Date = seq(as.Date("2010-11-01"), as.Date("2025-06-01"), by="month")
  ) %>%
  left_join(sofr_6m, by="Date") %>%
  mutate(
    # If SOFR is missing (pre-2018), use the proxy rate
    SOFR = ifelse(is.na(SOFR_6m), treasury_6m$T_Bill, SOFR_6m)
  ) %>%
  select(Date, SOFR)



debt_eur <- 540000000
spread <- 0.0090    # 90 basis points

# Merge Interest data with your existing EU P&L data
df_point8 <- df_hedge_eur %>%
  left_join(df_sofr, by = "Date") %>%
  mutate(
    # Annual interest rate = SOFR + 0.9%
    # We divide by 12 for the monthly expense in the time series
    Interest_Rate_t = (SOFR/100) + spread, 
    Interest_Expense_t = (debt_eur * Interest_Rate_t) / 12,
    
    # Net Income = EBIT (from Point 7) - Interest
    Net_Income_b1_hedged = EBIT_eur_b1_hedged - Interest_Expense_t
  )

# Calculate Risk for the Bottom Line
risk_net_income <- ebit_var_etl(df_point8$Net_Income_b1_hedged, alpha) %>%
  mutate(
    strategy = "Hedged Commodity, Floating Debt",
    VaR_M = EBIT_VaR / 1e6,
    ETL_M = EBIT_ETL / 1e6
  )

print("--- Risk net income: ---")
print(risk_net_income)

# Until here we have a partial hedging strategy.

# Combined:

# 1. Choose a Fixed Rate for the Interest Rate Swap (IRS)
# Let's assume the bank offers a fixed rate of 4.5% (0.045)
fixed_swap_rate <- 0.045 

df_point8 <- df_point8 %>%
  mutate(
    # Floating Interest (What you have now)
    Interest_Expense_Floating = (debt_eur * ((SOFR/100) + spread)) / 12,
    
    # Fixed Interest (The Hedge / Interest Rate Swap)
    # This removes the 'SOFR' variable entirely!
    Interest_Expense_Fixed = (debt_eur * (fixed_swap_rate + spread)) / 12,
    
    # Net Income with COMBINED HEDGE (Commodity + Interest Rate)
    Net_Income_Combined_Hedge = EBIT_eur_b1_hedged - Interest_Expense_Fixed
  )

# 2. Compare the Risk
risk_combined <- bind_rows(
  ebit_var_etl(df_point8$Net_Income_b1_hedged, alpha) %>% 
    mutate(strategy = "Hedged Commodity / Floating Debt"),
  
  ebit_var_etl(df_point8$Net_Income_Combined_Hedge, alpha) %>% 
    mutate(strategy = "Combined Hedge (Commodity + Interest Rate)")
) %>%
  mutate(VaR_M = EBIT_VaR / 1e6, ETL_M = EBIT_ETL / 1e6)

print("Combined hedging strategy:")
print(risk_combined)