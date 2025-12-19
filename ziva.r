library(readr)
library(dplyr)
library(zoo)
library(lubridate)

# 1. 

# We selected copper for our trading commodity. 

# Initial annual P&L data
S_0 <- 920000000 # annual sales
material_cost_ratio <- 0.69

material_cost_0 <- S_0 * material_cost_ratio

print(paste("Current annual material cost (USD):", format(material_cost_0)))

copper_data <- read_csv("copper.csv", show_col_types = FALSE)

copper_data <- copper_data %>%
    rename(Date = Month, Price_USD = Price)
copper_data$Date <- as.Date(copper_data$Date, format = "%m/%d/%y")
copper_data$Date <- format(copper_data$Date, "%d.%m.%Y")

# P_C_0 is the last closing price from the data (initial copper price)
P_C_0 <- last(copper_data$Price_USD)

print(paste("Current Copper price (USD):", format(P_C_0)))

# We calculate the conversion factor k (effective quantity of copper units needed)
k_factor <- material_cost_0 / P_C_0

# We calculate the time series of the simulated material cost
copper_data <- copper_data %>%
  mutate(
    Material_cost_t = Price_USD * k_factor,  # CM_t = P_C,t * k
    Log_Returns = c(NA, diff(log(Material_cost_t)))
    )

material_cost_table <- copper_data %>%
  # Pomnožimo vsako zgodovinsko ceno bakra z izračunano efektivno količino
  mutate(
    Material_cost_t = Price_USD * k_factor,
    # Izračun mesečnih logaritmičnih donosov (za nadaljnje analize tveganja)
    Log_Returns = c(NA, diff(log(Price_USD)))
  ) %>%
  # Pretvorimo v časovno vrsto zoo za lažjo manipulacijo
  select(Date, Material_cost_t, Log_Returns)


# 2.

rho_1 <- 0.74  # correlation 1
rho_2 <- 0.92  # correlation 2

copper_data2 <- copper_data %>%
  filter(!is.na(Log_Returns))  # removes the first NA value

cost_log_returns <- copper_data2$Log_Returns
N <- length(cost_log_returns)

set.seed(42)
epsilon <- rnorm(N, mean = 0, sd = sd(cost_log_returns)) 

#---------------------------------------

#Če želiš, da je korelacija res točno 0.74 / 0.92, mora biti epsilon standardni normal (sd=1), ne z sd = sd(cost_log_returns).
#epsilon <- rnorm(N, 0, 1)

#R_cost_z <- scale(cost_log_returns)[,1]   # standardiziraj returns
#R_sales_z <- rho * R_cost_z + sqrt(1-rho^2) * epsilon

# potem vrni na originalno skalo (isto sd kot cost)
#R_sales <- as.numeric(R_sales_z) * sd(cost_log_returns)
#--------------------------------------

# New data frame to hold sales returns
sales_returns <- data.frame(Date = copper_data2$Date)

# a. Non-correlated sales - fixed sales

sales_returns <- sales_returns %>%
  mutate(
    R_Sales_a = 0  # No returns since sales are fixed
  )

# b. Correlated sales (rho_1, rho_2)

# Function to calculate correlated returns R_S = rho * cost_log_returns + sqrt(1 - rho^2) * epsilon, it ensures the resulting correlation is exactly rho.
calculate_correlated_returns <- function(cost_log_returns, epsilon, rho) {
  return(rho * cost_log_returns + sqrt(1 - rho^2) * epsilon)
}

sales_returns <- sales_returns %>%
  mutate(
    R_Sales_b_rho_1 = calculate_correlated_returns(cost_log_returns, epsilon, rho_1),
    R_Sales_b_rho_2 = calculate_correlated_returns(cost_log_returns, epsilon, rho_2)
  )

# Calculate sales amounts based on returns

# Function for sales P_t = P_{t-1} * exp(R_t)
returns_to_price_R <- function(S_0, returns_series) {
  price_series <- numeric(length(returns_series) + 1)
  price_series[1] <- S_0
  
  for (t in 1:length(returns_series)) {
    price_series[t+1] <- price_series[t] * exp(returns_series[t])
  }
  
  return(price_series[-1])
}

sales <- data.frame(Date = copper_data2$Date)

sales <- sales %>%
  mutate(
    Sales_a = S_0,  # Fixed sales
    Sales_b_rho_1 = returns_to_price_R(S_0, sales_returns$R_Sales_b_rho_1),
    Sales_b_rho_2 = returns_to_price_R(S_0, sales_returns$R_Sales_b_rho_2)
  )

final_data <- copper_data2 %>%
  left_join(sales, by = "Date")


# 3.

# EBIT_t = S_t - CM_t - OC_t - DL_t - OH, kjer je
# S_t - sales
# CM_t - material costs
# OC_t  - other variable costs (6%)
# DL_t  - direct labor costs (14%)
# OH    - overheads (77 million USD annually)

OC_ratio <- 0.06
OC_0 <- OC_ratio * S_0
DL_ratio <- 0.14
DL_0 <- DL_ratio * S_0
OH <- 77000000

EBIT_0 <- S_0 - material_cost_0 - OC_0 - DL_0 - OH

final_data2 <- final_data %>%
  mutate(
    EBIT_a = Sales_a - Material_cost_t - 0.06*Sales_a - 0.14*Sales_a - OH,
    EBIT_b_rho_1 = Sales_b_rho_1 - Material_cost_t - 0.06 * Sales_b_rho_1 - 0.14 * Sales_b_rho_1 - OH,
    EBIT_b_rho_2 = Sales_b_rho_2 - Material_cost_t - 0.06 * Sales_b_rho_2 - 0.14 * Sales_b_rho_2 - OH
  )


#----------------------------------------- Ker so tvoje Sales in stroški na letnem nivoju, je mean čez 12 mesecev smiselna “expected annual EBIT” ocena
#last12 <- final_data2 %>% tail(12)

#EBIT_year <- last12 %>% summarise(
#  EBIT_a = mean(EBIT_a, na.rm = TRUE),
#  EBIT_b_rho_1 = mean(EBIT_b_rho_1, na.rm = TRUE), 
#  EBIT_b_rho_2 = mean(EBIT_b_rho_2, na.rm = TRUE)
#)
#-----------------------------------------------

EBIT <- final_data2 %>%
  summarise(
    EBIT_a = mean(EBIT_a, na.rm = TRUE),
    EBIT_b_rho_1 = mean(EBIT_b_rho_1, na.rm = TRUE), 
    EBIT_b_rho_2 = mean(EBIT_b_rho_2, na.rm = TRUE)
  )

EBIT_a_last <- tail(final_data2$EBIT_a, 1)  # should equal EBIT_0 (≈24.2m)
EBIT_b_rho_1_last <- tail(final_data2$EBIT_b_rho_1, 1) 
EBIT_b_rho_2_last <- tail(final_data2$EBIT_b_rho_2, 1) 

print(paste(EBIT))

# 4. VaR and ETL (Expected Tail Loss) for EBIT

alpha <- 0.05
#---------------------------------------------
# loss L = -EBIT. Zato je bolje, da VaR/ETL računaš na L, nato pa (če želiš) pretvoriš nazaj v EBIT.

#var_etl_loss_from_ebit <- function(ebit_vec, alpha = 0.05) {
#  ebit_vec <- ebit_vec[!is.na(ebit_vec)]
#  L <- -ebit_vec
#  
#  VaR_L <- as.numeric(quantile(L, probs = 1 - alpha))         # 95% VaR of loss
#  ETL_L <- mean(L[L >= VaR_L], na.rm = TRUE)                  # tail mean of loss
#  
# tibble(alpha = alpha, VaR_L = VaR_L, ETL_L = ETL_L,
#         VaR_EBIT = -VaR_L, ETL_EBIT = -ETL_L)
#}

# Zakaj tako: “slab rep” za EBIT je spodaj, “slab rep” za loss je zgoraj.
#-------------------------------------------
ebit_var_etl <- function(ebit_vec, alpha = 0.05) {
  ebit_vec <- ebit_vec[!is.na(ebit_vec)]

  EBIT_VaR <- as.numeric(quantile(ebit_vec, probs = alpha))
  EBIT_ETL <- mean(ebit_vec[ebit_vec <= EBIT_VaR], na.rm = TRUE)

  tibble(
    alpha = alpha,
    EBIT_VaR = EBIT_VaR,
    EBIT_ETL = EBIT_ETL
  )
}

risk_table <- bind_rows(
  ebit_var_etl(final_data2$EBIT_a, alpha)       %>% mutate(case = "a) fixed sales"),
  ebit_var_etl(final_data2$EBIT_b_rho_1, alpha) %>% mutate(case = "b) rho = 0.74"),
  ebit_var_etl(final_data2$EBIT_b_rho_2, alpha) %>% mutate(case = "b) rho = 0.92")
) %>% select(case, alpha, EBIT_VaR, EBIT_ETL)


risk_table_m <- risk_table %>%
  mutate(across(c(EBIT_VaR, EBIT_ETL), ~ .x/1e6))

