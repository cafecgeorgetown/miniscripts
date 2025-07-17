# correcting sleep/wakeup time 

df[which(df$sleep_yesterday_time == "PM" & df$sleep_yesterday_hour != 12), ]$sleep_yesterday_hour <- df[which(df$sleep_yesterday_time == "PM" & df$sleep_yesterday_hour != 12), ]$sleep_yesterday_hour + 12 

df[which(df$sleep_yesterday_time == "AM" & df$sleep_yesterday_hour == 12), ]$sleep_yesterday_hour <- df[which(df$sleep_yesterday_time == "AM" & df$sleep_yesterday_hour == 12), ]$sleep_yesterday_hour - 12  

df[which(df$sleep_yesterday_hour == 24), ]$sleep_yesterday_hour <- df[which(df$sleep_yesterday_hour == 24),]$sleep_yesterday_hour - 24 

df[which(df$wake_today_time == "PM" & df$wake_today_hour != 12), ]$wake_today_hour <- df[which(df$wake_today_time == "PM" & df$wake_today_hour != 12), ]$wake_today_hour + 12 

df[which(df$wake_today_time == "AM" & df$wake_today_hour == 12), ]$wake_today_hour <- df[which(df$wake_today_time == "AM" & df$wake_today_hour == 12), ]$wake_today_hour - 12  

df[which(df$wake_today_hour == 24), ]$wake_today_hour <- df[which(df$wake_today_hour == 24),]$wake_today_hour - 24 

# add sleep minutes for sweden 
df[df$study == "sweden",]$sleep_yesterday_min <- 0
df[df$study == "sweden",]$wake_today_min <- 0

## calculate total time slept 

# Function to calculate hours slept
calculate_hours_slept <- function(asleep_hour, asleep_minute, awake_hour, awake_minute) {
  # Convert asleep and awake times to minutes since midnight
  asleep_total_minutes <- 1440 - (asleep_hour * 60 + asleep_minute)
  awake_total_minutes <- awake_hour * 60 + awake_minute
  
  # Calculate the difference in minutes
  minutes_slept <- awake_total_minutes + asleep_total_minutes
  
  # Convert minutes slept to hours
  hours_slept <- minutes_slept / 60
  
  return(hours_slept)
}

df$total_sleep_time <- calculate_hours_slept(df$sleep_yesterday_hour, df$sleep_yesterday_min, df$wake_today_hour, df$wake_today_min)

df$bedtime <- 1200- (df$sleep_yesterday_hour * 60 + df$sleep_yesterday_min)
df[which(df$sleep_yesterday_hour == 0),]$bedtime <- -240 - df[which(df$sleep_yesterday_hour == 0),]$sleep_yesterday_min