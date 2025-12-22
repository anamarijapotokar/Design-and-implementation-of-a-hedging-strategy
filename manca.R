library(readr)
library(dplyr)
library(tibble)
library(readxl)

# =========================
# PART 9: Two-commodity case
# =========================

# ---- Given constants (same as before except other materials ratio) ----
S_0 <- 920000000
OH  <- 77000000
DL_ratio <- 0.14
OM_ratio <- 0.05   # other variable materials NOW 5% (instead of 6%)

# Exposures at current prices
w1 <- 0.57
w2 <- 0.15

CM1_0 <- S_0 * w1
CM2_0 <- S_0 * w2

# ---- 9.1 Choose two commodities and load spot prices ----
# Replace with your real filenames and column names
c1 <- read_csv("copper.csv", show_col_types = FALSE) %>%
  rename(Date = Month, Price_USD = Price)

c2 <- read_xls("aluminium.xls") %>%   
  rename(Date = Month, Price_USD = Price) %>% select(Date, Price_USD)

c2$Price_USD = parse_number(c2$Price_USD)

c1$Date <- as.Date(c1$Date, format = "%m/%d/%y")
c1$Date <- format(c1$Date, "%d.%m.%Y")

c2$Date <- as.Date(c2$Date, format = "%d/%m/%y")
c2$Date <- format(c2$Date, "%d.%m.%Y")

# Align dates
spot2 <- inner_join(c1, c2, by = "Date") %>%
  arrange(Date) %>% rename(Price_USD_co = Price_USD.x, Price_USD_al = Price_USD.y, )

# Current (last) prices
P_co_0 <- tail(spot2$Price_USD_co, 1)
P_al_0 <- tail(spot2$Price_USD_al, 1)

# Effective quantities (k-factors) so that at current price the cost matches 57% and 15% of sales
k1 <- CM1_0 / P_co_0
k2 <- CM2_0 / P_al_0

# Build time series of costs
spot2 <- spot2 %>%
  mutate(
    CM1_t = k1 * Price_USD_co,
    CM2_t = k2 * Price_USD_al,
    CM_t  = CM1_t + CM2_t,                 # total core material cost
    CM_logret = c(NA, diff(log(CM_t)))     # returns for risk & sales simulation
  )

# ---- 9.2 Sales time series: a) fixed, b) correlated with CM returns ----
rho_sales_1 <- 0.74
rho_sales_2 <- 0.92


df9 <- spot2 %>%
  filter(!is.na(CM_logret)) %>%
  select(Date, Price_USD_co, Price_USD_al, CM1_t, CM2_t, CM_t, CM_logret)

N <- nrow(df9)

set.seed(42)
eps <- rnorm(N, 0, 1)

calc_corr_returns <- function(base_ret, eps, rho) {
  # base_ret should be standardized if you want more exact control;
  # for your previous style, we keep it simple:
  rho * base_ret + sqrt(1 - rho^2) * eps * sd(base_ret)
}

# If you want tighter correlation control, standardize base_ret:
base_ret <- df9$CM_logret
base_z   <- as.numeric(scale(base_ret))

R_sales_b1_z <- rho_sales_1 * base_z + sqrt(1 - rho_sales_1^2) * eps
R_sales_b2_z <- rho_sales_2 * base_z + sqrt(1 - rho_sales_2^2) * eps

# Scale back to similar vol as base returns
R_sales_b1 <- R_sales_b1_z * sd(base_ret)
R_sales_b2 <- R_sales_b2_z * sd(base_ret)

returns_to_level <- function(S0, r) {
  out <- numeric(length(r))
  prev <- S0
  for (i in seq_along(r)) {
    prev <- prev * exp(r[i])
    out[i] <- prev
  }
  out
}

df9 <- df9 %>%
  mutate(
    Sales_a = S_0,
    Sales_b_r074 = returns_to_level(S_0, R_sales_b1),
    Sales_b_r092 = returns_to_level(S_0, R_sales_b2)
  )

# ---- 9.3 EBIT simulation (no financial part) ----
EBIT_0 <- S_0 - (CM1_0 + CM2_0) - OM_ratio*S_0 - DL_ratio*S_0 - OH

df9 <- df9 %>%
  mutate(
    EBIT_a      = Sales_a      - CM_t - OM_ratio*Sales_a      - DL_ratio*Sales_a      - OH,
    EBIT_b_r074 = Sales_b_r074 - CM_t - OM_ratio*Sales_b_r074 - DL_ratio*Sales_b_r074 - OH,
    EBIT_b_r092 = Sales_b_r092 - CM_t - OM_ratio*Sales_b_r092 - DL_ratio*Sales_b_r092 - OH
  )

# ---- 9.4 VaR & ETL for EBIT (same function as before) ----
alpha <- 0.05

ebit_var_etl <- function(ebit_vec, alpha = 0.05) {
  ebit_vec <- ebit_vec[!is.na(ebit_vec)]
  VaR <- as.numeric(quantile(ebit_vec, probs = alpha))
  ETL <- mean(ebit_vec[ebit_vec <= VaR], na.rm = TRUE)
  tibble(alpha = alpha, EBIT_VaR = VaR, EBIT_ETL = ETL)
}

risk_unhedged_9 <- bind_rows(
  ebit_var_etl(df9$EBIT_a, alpha)      %>% mutate(case = "a) fixed sales"),
  ebit_var_etl(df9$EBIT_b_r074, alpha) %>% mutate(case = "b) sales corr 0.74"),
  ebit_var_etl(df9$EBIT_b_r092, alpha) %>% mutate(case = "b) sales corr 0.92")
) %>%
  mutate(across(c(EBIT_VaR, EBIT_ETL), ~ .x/1e6))

print(paste("Point estimate EBIT_0 (USD):", format(EBIT_0)))
print(risk_unhedged_9)

#---- 9.5 Hedging with TWO futures (minimum variance hedge vector) ----
lb_per_ton = 2204.62262185

futures1 = read_csv(
  "copper_futures.csv",
  skip = 3,
  col_names = c("Date","Close","High","Low","Open","Volume"),
  show_col_types = FALSE
) %>%
  mutate(
    Date = as.Date(Date),
    Close = as.numeric(Close),
    Fut_USD_per_ton_co = Close * lb_per_ton
  ) %>%
  select(Date, Fut_USD_per_ton_co)

futures2 = read_csv(
  "aluminium_futures.csv",
  skip = 3,
  col_names = c("Date","Close","High","Low","Open","Volume"),
  show_col_types = FALSE
) %>%
  mutate(
    Date = as.Date(Date),
    Close = as.numeric(Close),
    Fut_USD_per_ton_al = Close * lb_per_ton
  ) %>%
  select(Date, Fut_USD_per_ton_al) %>% filter(format(Date, "%d") == "01") 


futures1$Date <- as.Date(futures1$Date)
futures2$Date <- as.Date(futures2$Date)

df9 = df9 %>% 
  mutate(Date = as.Date(Date, format = "%d.%m.%Y"))

dfH <- df9 %>%
  inner_join(futures1, by = "Date") %>%
  inner_join(futures2, by = "Date") %>%
  arrange(Date) %>%
  mutate(
    dCM = CM_t - lag(CM_t),
    dF_Cu = Fut_USD_per_ton_co   - lag(Fut_USD_per_ton_co),
    dF_Al = Fut_USD_per_ton_al   - lag(Fut_USD_per_ton_al)
  ) %>%
  filter(!is.na(dCM), !is.na(dF_Cu), !is.na(dF_Al))

# Variance-covariance matrix of futures
Sigma_F <- var(cbind(dfH$dF_Cu, dfH$dF_Al))

# Covariance with material cost changes
Sigma_F_CM <- cov(cbind(dfH$dF_Cu, dfH$dF_Al), dfH$dCM)[,1]

# Minimum-variance hedge ratios
h_star <- solve(Sigma_F, Sigma_F_CM)

names(h_star) <- c("h_Copper", "h_Aluminium")
print(h_star)


dfH <- dfH %>%
  mutate(
    HedgePnL <- h_star[1]*dfH$dF_Cu + h_star[2]*dfH$dF_Al,
    EBIT_a_h      = EBIT_a      + HedgePnL,
    EBIT_b_r074_h = EBIT_b_r074 + HedgePnL,
    EBIT_b_r092_h = EBIT_b_r092 + HedgePnL
  )

risk_hedged_9 <- bind_rows(
  ebit_var_etl(dfH$EBIT_a_h, alpha)      %>% mutate(case = "a) fixed sales"),
  ebit_var_etl(dfH$EBIT_b_r074_h, alpha) %>% mutate(case = "b) sales corr 0.74"),
  ebit_var_etl(dfH$EBIT_b_r092_h, alpha) %>% mutate(case = "b) sales corr 0.92")
) %>%
  mutate(strategy = "hedged (2-futures)",
         h1 = h_star[1], h2 = h_star[2]) %>%
  mutate(across(c(EBIT_VaR, EBIT_ETL), ~ .x/1e6))

# print(h_star)
print(risk_hedged_9)

# ---- 9.6 Sensitivity: derivative correlation (basis risk) for each instrument ----

make_deriv <- function(dCM, sd_target, rho, seed = 42) {
  set.seed(seed + round(1000*rho))
  zCM <- as.numeric(scale(dCM))
  eps <- rnorm(length(zCM))
  eps <- eps - (sum(eps*zCM)/sum(zCM^2))*zCM
  zDer <- rho*zCM + sqrt(1-rho^2)*eps
  zDer * sd_target
}

rho_set <- c(0.60, 0.75, 0.90)
results_9_6 <- list()

for (rho in rho_set) {
  
  dF1 <- make_deriv(dfH$dCM, sd(dfH$dF_Cu), rho)
  dF2 <- make_deriv(dfH$dCM, sd(dfH$dF_Al), rho+0.01)
  
  Sigma_F <- var(cbind(dF1, dF2))
  Sigma_F_CM <- cov(cbind(dF1, dF2), dfH$dCM)[,1]
  h_star <- solve(Sigma_F, Sigma_F_CM)
  
  HedgePnL <- h_star[1]*dF1 + h_star[2]*dF2
  
  EBIT_h <- dfH$EBIT_a + HedgePnL
  
  results_9_6[[as.character(rho)]] <-
    ebit_var_etl(EBIT_h, alpha) %>%
    mutate(rho_deriv = rho)
}

risk_9_6 <- bind_rows(results_9_6) %>%
  mutate(across(c(EBIT_VaR, EBIT_ETL), ~ .x/1e6))

print(risk_9_6)


#----Poročilo izračuni
cor(c1$Price_USD, c2$Price_USD, use = "complete.obs")
cor(
  diff(df9$CM1_t),
  diff(df9$CM2_t),
  use = "complete.obs"
)
paste(
  "Sample period:",
  format(min(df9$Date)),
  "-",
  format(max(df9$Date))
)

risk_unhedged_9 %>%
  mutate(case = paste(case, "(two-commodity)"))

risk_unhedged_9 %>%
  mutate(case = paste(case, "(two-commodity)"))