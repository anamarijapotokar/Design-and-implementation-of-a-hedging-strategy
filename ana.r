library(readr)
library(dplyr)
library(lubridate)
library(tidyr)
library(tibble)

lb_per_ton <- 2204.62262185
contract_size_lb  <- 25000
contract_size_ton <- contract_size_lb / lb_per_ton

read_futures <- function(path) {
  read_csv(
    path,
    skip = 3,
    col_names = c("Date","Close","High","Low","Open","Volume"),
    show_col_types = FALSE
  ) %>%
    mutate(
      Date = as.Date(Date),
      Close = suppressWarnings(as.numeric(gsub(",", "", as.character(Close)))),
      Fut_USD_per_ton = Close * lb_per_ton
    ) %>%
    select(Date, Fut_USD_per_ton)
}

prep_base <- function(final_data2, futures) {
  final_data2 %>%
    mutate(Date = if (is.character(Date)) dmy(Date) else as.Date(Date)) %>%
    inner_join(futures, by = "Date") %>%
    arrange(Date) %>%
    mutate(
      dCM = Material_cost_t - lag(Material_cost_t),     # USD change in total material cost
      dF  = Fut_USD_per_ton - lag(Fut_USD_per_ton)      # USD/ton change in futures
    ) %>%
    drop_na(dCM, dF)
}

minvar_h <- function(dY, dX) cov(dY, dX) / var(dX)

ebit_var_etl <- function(ebit_vec, alpha = 0.05) {
  x <- ebit_vec[!is.na(ebit_vec)]
  q <- as.numeric(quantile(x, probs = alpha, type = 7))
  etl <- if (any(x <= q)) mean(x[x <= q]) else NA_real_
  tibble(alpha = alpha, EBIT_VaR = q, EBIT_ETL = etl)
}

risk_table <- function(df, cols, cases, strategy, alpha = 0.05) {
  bind_rows(lapply(seq_along(cols), function(i) {
    ebit_var_etl(df[[cols[i]]], alpha) %>%
      mutate(case = cases[i], strategy = strategy)
  })) %>%
    select(case, strategy, alpha, EBIT_VaR, EBIT_ETL)
}

# Create derivative changes with (approximately exact) SAMPLE correlation = rho to dCM
# and a chosen target SD (in USD/ton)
make_dDeriv <- function(dCM, rho, sd_target, seed = 42) {
  set.seed(seed + round(rho * 1000))

  zCM <- as.numeric(scale(dCM, center = TRUE, scale = TRUE))
  eps <- rnorm(length(zCM))

  # Orthogonalize eps vs zCM to get sample corr ~ 0
  eps_orth <- eps - (sum(eps * zCM) / sum(zCM^2)) * zCM
  eps_orth <- eps_orth / sd(eps_orth)

  zD <- rho * zCM + sqrt(1 - rho^2) * eps_orth
  dDeriv <- zD * sd_target
  dDeriv
}

apply_hedge <- function(df, dX, h) {
  hedgePnL <- h * dX
# Hedge interpretation:
# The firm is exposed to rising copper prices because they increase input/material costs.
# To offset this, we take a LONG position in the hedge instrument (futures/derivative),
# so the hedge P&L h * ΔX is positive when copper prices rise.
  df %>%
    mutate(
      hedgePnL = hedgePnL,
      EBIT_a_hedged       = EBIT_a       + hedgePnL,
      EBIT_b_rho_1_hedged = EBIT_b_rho_1 + hedgePnL,
      EBIT_b_rho_2_hedged = EBIT_b_rho_2 + hedgePnL
    )
}

# ---------------------------
# PART 5
# ---------------------------
if (!exists("final_data2")) stop("final_data2 not found. Run upstream script first.")

futures <- read_futures("copper_futures.csv")
df_base <- prep_base(final_data2, futures)

# Futures hedge ratio
h_fut <- minvar_h(df_base$dCM, df_base$dF)
N_fut <- h_fut / contract_size_ton

cat("\n--- Hedge ratio using actual copper futures ---\n")
cat("Optimal hedge quantity h* (metric tons):", round(h_fut, 2), "\n")
cat("Contract size (metric tons):", round(contract_size_ton, 4), "\n")
cat("Approx. number of contracts:", round(N_fut, 0), "\n")

df_fut <- apply_hedge(df_base, df_base$dF, h_fut)

cases <- c("a) fixed sales", "b) rho = 0.74", "b) rho = 0.92")
alpha <- 0.05

risk_unhedged <- risk_table(
  df_base,
  cols  = c("EBIT_a", "EBIT_b_rho_1", "EBIT_b_rho_2"),
  cases = cases,
  strategy = "unhedged",
  alpha = alpha
)

risk_hedged_fut <- risk_table(
  df_fut,
  cols  = c("EBIT_a_hedged", "EBIT_b_rho_1_hedged", "EBIT_b_rho_2_hedged"),
  cases = cases,
  strategy = "hedged (copper futures)",
  alpha = alpha
)

risk_part5_futures <- bind_rows(risk_unhedged, risk_hedged_fut) %>%
  mutate(across(c(EBIT_VaR, EBIT_ETL), ~ .x / 1e6))

cat("\n--- VaR/ETL comparison (USD millions) ---\n")
print(risk_part5_futures)

# Synthetic derivative (corr ~ 0.75) constructed properly on CHANGES
rho_deriv <- 0.75
sd_target <- sd(df_base$dF)  # keep units USD/ton comparable to futures changes
dDeriv <- make_dDeriv(df_base$dCM, rho = rho_deriv, sd_target = sd_target, seed = 42)

cat("\n--- Hedge ratio using synthetic derivative (corr ~ 0.75 to dCM) ---\n")
cat("Sample corr(dCM, dDeriv):", round(cor(df_base$dCM, dDeriv), 3), "\n")

h_deriv <- minvar_h(df_base$dCM, dDeriv)
N_deriv <- h_deriv / contract_size_ton

cat("Optimal hedge quantity h* (metric tons):", round(h_deriv, 2), "\n")
cat("Approx. number of contracts (futures-equivalent):", round(N_deriv, 0), "\n")

df_der <- apply_hedge(df_base, dDeriv, h_deriv)

risk_hedged_deriv <- risk_table(
  df_der,
  cols  = c("EBIT_a_hedged", "EBIT_b_rho_1_hedged", "EBIT_b_rho_2_hedged"),
  cases = cases,
  strategy = paste0("hedged (deriv corr~", rho_deriv, ")"),
  alpha = alpha
)

risk_part5_deriv <- bind_rows(risk_unhedged, risk_hedged_deriv) %>%
  mutate(across(c(EBIT_VaR, EBIT_ETL), ~ .x / 1e6))

cat("\n--- VaR/ETL comparison with synthetic derivative (USD millions) ---\n")
print(risk_part5_deriv)

# ---------------------------
# PART 6
# ---------------------------
rho_set <- c(0.60, 0.75, 0.90)

risk_hedged_list <- list()
eff_list <- list()

for (rho in rho_set) {
  dDer <- make_dDeriv(df_base$dCM, rho = rho, sd_target = sd_target, seed = 42)

  h <- minvar_h(df_base$dCM, dDer)
  resid <- df_base$dCM - h * dDer
  hedge_eff <- 1 - var(resid) / var(df_base$dCM)

  df_h <- apply_hedge(df_base, dDer, h)

  risk_h <- risk_table(
    df_h,
    cols  = c("EBIT_a_hedged", "EBIT_b_rho_1_hedged", "EBIT_b_rho_2_hedged"),
    cases = cases,
    strategy = paste0("hedged (deriv corr~", rho, ")"),
    alpha = alpha
  ) %>%
    mutate(rho_deriv = rho)

  risk_hedged_list[[as.character(rho)]] <- risk_h

  eff_list[[as.character(rho)]] <- tibble(
    rho_deriv = rho,
    sample_corr = cor(df_base$dCM, dDer),
    h_star_ton = h,
    hedge_effectiveness = hedge_eff
  )
}

risk_part6 <- bind_rows(
  risk_unhedged %>% mutate(rho_deriv = NA_real_),
  bind_rows(risk_hedged_list)
) %>%
  mutate(across(c(EBIT_VaR, EBIT_ETL), ~ .x / 1e6)) %>%
  select(case, strategy, rho_deriv, alpha, EBIT_VaR, EBIT_ETL)

eff_part6 <- bind_rows(eff_list)

cat("\n--- PART 6: VaR/ETL vs derivative correlation (USD millions) ---\n")
print(risk_part6)

cat("\n--- PART 6: Hedge efficiency diagnostics (cost-change variance reduction) ---\n")
print(eff_part6)

