######## Recoding Reasons Variables ###################################################### 

# select names of reason variables 
names <- colnames(combined.maq.df %>% dplyr::select(contains("reason_")))
combined.maq.df[names] <- sapply(combined.maq.df[names],as.character)

# select binary reason variables and convert to 0/1 scale
df2 <- combined.maq.df %>% dplyr::select(contains("reason_")) 
df2 <- df2[,c("reason_educate_media", "reason_calm_media", "reason_keepbusy_media", 
              "reason_communicate_media", "reason_enjoy_media", "reason_work_media")]
df2[df2 == "never"] <- "0"
df2[df2 != "0"] <- "1"

# change column names and merge with overall data 
colnames(df2) <- c("reason_educate_binary", "reason_calm_binary", "reason_keepbusy_binary", "reason_communicate_binary",
                   "reason_enjoy_binary", "reason_work_binary")
combined.maq.df <- cbind(combined.maq.df, df2)

# for second version of MAQ, select questions that correspond to a reason and add to binary variables 
combined.maq.df[which(combined.maq.df$child_regulatory_4 == "never"),]$reason_calm_binary <- 0
combined.maq.df[which(combined.maq.df$child_regulatory_4 != "never"),]$reason_calm_binary <- 1

combined.maq.df[which(combined.maq.df$child_regulatory_6 == "never"),]$reason_keepbusy_binary <- 0
combined.maq.df[which(combined.maq.df$child_regulatory_6 != "never"),]$reason_keepbusy_binary <- 1

combined.maq.df[which(combined.maq.df$child_regulatory_7 == "never"),]$reason_keepbusy_binary <- 0
combined.maq.df[which(combined.maq.df$child_regulatory_7 != "never"),]$reason_keepbusy_binary <- 1

combined.maq.df[which(combined.maq.df$child_regulatory_8 == "never"),]$reason_keepbusy_binary <- 0
combined.maq.df[which(combined.maq.df$child_regulatory_8 != "never"),]$reason_keepbusy_binary <- 1

combined.maq.df[which(combined.maq.df$child_regulatory_14 == "never"),]$reason_keepbusy_binary <- 0
combined.maq.df[which(combined.maq.df$child_regulatory_14 != "never"),]$reason_keepbusy_binary <- 1

df[which(df$child_rum_learn == "never"),]$reason_educate_binary <- 0
df[which(df$child_rum_learn != "never"),]$reason_educate_binary <- 1

df[which(df$child_rum_occupypublic == "never"),]$reason_keepbusy_binary <- 0
df[which(df$child_rum_occupypublic != "never"),]$reason_keepbusy_binary <- 1

df[which(df$child_rum_calmupset == "never"),]$reason_calm_binary <- 0
df[which(df$child_rum_calmupset != "never"),]$reason_calm_binary <- 1

df[which(df$child_rum_occupy == "never"),]$reason_keepbusy_binary <- 0
df[which(df$child_rum_occupy != "never"),]$reason_keepbusy_binary <- 1

# add converted likert scale questions to data
names_adjusted <- paste0(names, "_adjusted")
combined.maq.df[, names_adjusted] <- combined.maq.df[,names]

# combine scales
combined.maq.df[ ,names_adjusted][combined.maq.df[,names_adjusted] == "less_once_per_week" ] <- "rarely"
combined.maq.df[ ,names_adjusted][combined.maq.df[,names_adjusted] == "once_per_week" ] <- "sometimes"
combined.maq.df[ ,names_adjusted][combined.maq.df[,names_adjusted] == "2_3_times_per_week" ] <- "sometimes"
combined.maq.df[ ,names_adjusted][combined.maq.df[,names_adjusted] == "4_6_times_per_week" ] <- "often"
combined.maq.df[ ,names_adjusted][combined.maq.df[,names_adjusted] == "everyday" ] <- "often"
combined.maq.df[ ,names_adjusted][combined.maq.df[,names_adjusted] == "several_times_per_day" ] <- "very_often"

