library(quantmod)
library(dplyr)

# 1. Define the specific timeframe
start_date <- "2010-11-01"
end_date   <- "2025-06-01"

# 2. Fetch daily data (Yahoo Finance)
# Note: FX rates are quoted as USD per 1 EUR
getSymbols("EURUSD=X", src = "yahoo", from = start_date, to = end_date, auto.assign = TRUE)

# 3. Create a target sequence of the 1st of every month
target_dates <- seq(as.Date(start_date), as.Date(end_date), by = "month")

# 4. Filter and handle non-trading days (weekends/holidays)
# If the 1st is a holiday, we'll take the next available trading day
fx_daily <- data.frame(Date = index(`EURUSD=X`), FX_Rate = as.numeric(Ad(`EURUSD=X`)))

fx_monthly_1st <- fx_daily %>%
  filter(Date %in% target_dates)

# Optional: If you want to ensure EVERY month is present (filling weekend gaps)
# We can join the target sequence with the daily data and use 'na.locf' 
# (Last Observation Carried Forward) for dates where the 1st was a weekend.
all_firsts <- data.frame(Date = target_dates)
fx_final <- all_firsts %>%
  left_join(fx_daily, by = "Date") %>%
  mutate(FX_Rate = na.locf(FX_Rate, na.rm = FALSE))

# 5. Export to CSV
write.csv(fx_final, "fx_rates_monthly.csv", row.names = FALSE)

print("File 'fx_rates_monthly.csv' created with 1st-of-month data.")