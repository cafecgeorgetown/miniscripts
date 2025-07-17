df <- combined.maq.df

climate <- read_csv("lockdown_climate_data/climate_cleaned.csv")
climate <- climate[, -1]
colnames(climate) <- c("country", "state", "recorded_date", "precipitation_metric", "max_temp_c", "week_avg_precip", "week_avg_temp")

df <- left_join(df, climate, by=c("recorded_date", "country", "state"))
combined.maq.df <- df
