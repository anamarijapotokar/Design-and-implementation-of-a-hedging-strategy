import pandas as pd

# 1. Preberi CSV (preskoči vrstico 'Ticker ...')
df = pd.read_csv(
    "aluminium_futures.csv",
    skiprows=[1],        # preskoči drugo vrstico
    parse_dates=["Date"],
    dayfirst=True
)

# 2. Uredi po datumu (za vsak slučaj)
df = df.sort_values("Date")

# 3. Izberi prvi trgovalni dan v mesecu
df_monthly = (
    df.set_index("Date")
      .resample("MS")   # Month Start
      .first()
      .reset_index()
)

# 5. Shrani nov CSV
df_monthly.to_csv("aluminium_futures_monthly.csv", index=False)
