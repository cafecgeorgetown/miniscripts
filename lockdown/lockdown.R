processing_lockdown <- function(measure, df, name) {
  
  lockdown_data <- read_excel("lockdown_climate_data/OxCGRT_timeseries_all.xlsx", measure)
  
  # remove unneeded columns
  lockdown_data <- lockdown_data[,-c(1,3,5)]
  
  # pivot the table so date and lockdown status are columns 
  lockdown_data <- 
    lockdown_data %>% pivot_longer(cols=c(3:988), names_to='date',values_to='lockdown_status')
  
  # make the date column a date type 
  lockdown_data$date <- as.Date(lockdown_data$date, "%d%b%Y")
  
  # convert country to match data 
  lockdown_data[lockdown_data$country_name == "United States", ]$country_name <- "us"
  lockdown_data[lockdown_data$country_name == "New Zealand", ]$country_name <- "newzealand"
  lockdown_data[lockdown_data$country_name == "Sweden", ]$country_name <- "sweden"
  
  # rename columns to match study data 
  colnames(lockdown_data) <- c("country", "state", "recorded_date", "lockdown_status")
  lockdown_data <-  lockdown_data[with(lockdown_data, order(country, state)), ] 
  lockdown_data$lockdown_status_avg <- c(rep_len(NA, 6), rowMeans(embed(lockdown_data$lockdown_status, 7), na.rm = TRUE))
  
  # merge based on country 
  lockdown_countries <- lockdown_data[is.na(lockdown_data$state) == TRUE,]
  country_df <- left_join(df, lockdown_countries[, c("recorded_date", "country", "lockdown_status_avg")], by=c("recorded_date", "country"))
  
  # merge based on state 
  lockdown_countries <- lockdown_data[is.na(lockdown_data$state) == FALSE,]
  state_df <- left_join(df, lockdown_countries[, c("recorded_date", "state", "lockdown_status_avg")], by=c("recorded_date", "state"))
  
  # add lockdown status for entire data 
  df[, name] <- country_df$lockdown_status_avg 
  df[is.na(df$state) == FALSE, name] <- state_df[is.na(df$state) == FALSE,]$lockdown_status_avg
  
  return(df)
}

combined.maq.df$state <- NA 

combined.maq.df[combined.maq.df$study == "Denver",]$state <- "Colorado"
combined.maq.df[combined.maq.df$study == "babyEMU",]$state <- "Washington DC"
combined.maq.df[combined.maq.df$study == "oklahoma",]$state <- "Oklahoma"
combined.maq.df[combined.maq.df$study == "mitten",]$state <- "Michigan"
combined.maq.df[combined.maq.df$study == "michigan_tablet",]$state <- "Michigan"
combined.maq.df[combined.maq.df$study == "emu",]$state <- "Washington DC"
combined.maq.df[combined.maq.df$study == "gu",]$state <- "Washington DC"
combined.maq.df[combined.maq.df$study == "novascotia",]$state <- "Nova Scotia"
combined.maq.df[combined.maq.df$study == "uwcrt",]$state <- "Wisconsin"
combined.maq.df[combined.maq.df$study == "wiamp",]$state <- "Wisconsin"

combined.maq.df <- processing_lockdown("stringency_index_avg", combined.maq.df, "covid_stringency_index")

combined.maq.df$covid_status_binary <- "pre_covid"
combined.maq.df[which(combined.maq.df$recorded_date >= as.Date("2020-03-20")),]$covid_status_binary <- "post_covid"

combined.maq.df$covid_status <- "pre_covid"
combined.maq.df[which(combined.maq.df$recorded_date >= as.Date("2020-03-20")),]$covid_status <- "during_covid"
combined.maq.df[which(combined.maq.df$recorded_date >= as.Date("2022-05-01")),]$covid_status <- "post_covid"


