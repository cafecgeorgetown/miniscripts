# function to get percentiles based on total words and norm tables 
mcdi.percentile <- function(male_norm, female_norm, total_var, over_age, under_age, min_row, max_row, perc_var) {
  
  female <- read_csv(female_norm)
  male <- read_csv(male_norm)
  
  for (n in 1:dim(combined.maq.df)[1]){
    # if mcdi was done 
    if (!is.na(combined.maq.df[n,][[total_var]])){
      # for males and females get the number closest to the total, get percentile 
      if (!is.na(combined.maq.df[n,]$age_months) == TRUE && combined.maq.df[n,]$age_months == over_age) {
        combined.maq.df[n,]$age_months <- over_age-1
      }
      else if (!is.na(combined.maq.df[n,]$age_months) == TRUE && combined.maq.df[n,]$age_months == under_age) {
        combined.maq.df[n,]$age_months <- under_age + 1 
      }
      if (!is.na(combined.maq.df[n,]$age_months) == TRUE && !is.na(combined.maq.df[n,]$child_sex) == TRUE && combined.maq.df[n,]$child_sex == "male") {
        closest <- which.min(abs(male[male$age==combined.maq.df[n,]$age_months, min_row:max_row] - combined.maq.df[n,][[total_var]]))[1]
        combined.maq.df[n,][[perc_var]] <- rownames(as.data.frame(closest))
      }
      if (!is.na(combined.maq.df[n,]$age_months) == TRUE && !is.na(combined.maq.df[n,]$child_sex) == TRUE && combined.maq.df[n,]$child_sex == "female") {
        closest <- which.min(abs(female[female$age==combined.maq.df[n,]$age_months, min_row:max_row] - combined.maq.df[n,][[total_var]]))[1]
        combined.maq.df[n,][[perc_var]] <- rownames(as.data.frame(closest))
      }
    }
  }
  return(combined.maq.df)
}


### Process MCDI #####################################################################

combined.maq.df$mcdi_produce_percentile <- NA 
combined.maq.df$mcdi_understand_percentile <- NA 

# 8-17 months group ###
new.df <- combined.maq.df[ , grepl("mcdi_8_17" , names(combined.maq.df))]
new.df <- new.df[ , !grepl("Sp_" , names(new.df))]
understand_df <- new.df 
produce_df <- new.df 

# compute total understand words 
understand_df[understand_df == "understand_say"] <- 1
understand_df[understand_df == "understand_only"] <- 1
understand_df[understand_df == "not_understand"] <- 0 
understand_df <- understand_df %>% mutate_all(as.numeric)
combined.maq.df$mcdi_8_17_total_understand <- rowSums(understand_df)

combined.maq.df <-
  mcdi.percentile("norm_tables/8_18_male_understand.csv", 
                  "norm_tables/8_18_female_understand.csv", 
                  "mcdi_8_17_total_understand", 
                  18, 
                  7, 
                  6, 
                  26, 
                  "mcdi_understand_percentile")

# compute total produce words 
produce_df[produce_df == "understand_say"] <- 1
produce_df[produce_df == "understand_only"] <- 0
produce_df[produce_df == "not_understand"] <- 0 
produce_df <- produce_df %>% mutate_all(as.numeric)
combined.maq.df$mcdi_8_17_total_produce <- rowSums(produce_df)

combined.maq.df <-
  mcdi.percentile("norm_tables/8_18_male_produce.csv", 
                  "norm_tables/8_18_female_produce.csv", 
                  "mcdi_8_17_total_produce", 
                  18, 
                  7, 
                  6, 
                  26, 
                  "mcdi_produce_percentile")

# 18-29 months group ####

# get columns for that age group
new.df <- combined.maq.df[, grepl("mcdi_18_29" , names(combined.maq.df))]
new.df <- new.df[ , !names(new.df) %in% c("mcdi_18_29_combine")]
new.df[new.df == "no"] <- 0
new.df[new.df == "yes"] <- 1
new.df <- new.df %>% mutate_all(as.numeric)
# compute total number of words produced 
combined.maq.df$mcdi_18_29_total <- rowSums(new.df)

combined.maq.df <-
  mcdi.percentile("norm_tables/16_30_male.csv", 
                  "norm_tables/16_30_female.csv", 
                  "mcdi_18_29_total", 
                  30, 
                  17, 
                  6, 
                  26, 
                  "mcdi_produce_percentile")

# 30-37 months age group ####

# compute total number of words 
new.df <- combined.maq.df[ , grepl("mcdi_30_37_lista" , names(combined.maq.df))]
new.df <- new.df[ , !names(new.df) %in% c("mcdi_30_37_lista", "mcdi_30_37_lista_Quality_control")]
new.df <- new.df %>% mutate_all(as.numeric)
combined.maq.df$mcdi_30_37_total <- rowSums(new.df, na=TRUE)
combined.maq.df$mcdi_30_37_total[combined.maq.df$mcdi_30_37_total==0] <- NA

combined.maq.df <-
  mcdi.percentile("norm_tables/30_37_male.csv", 
                  "norm_tables/30_37_female.csv", 
                  "mcdi_30_37_total", 
                  38, 
                  29, 
                  2, 
                  21, 
                  "mcdi_produce_percentile")

# remove nonnumeric characters for percentile 
combined.maq.df$mcdi_produce_percentile <- as.numeric(gsub("[^0-9.-]", "", combined.maq.df$mcdi_produce_percentile))
combined.maq.df$mcdi_understand_percentile <- as.numeric(gsub("[^0-9.-]", "", combined.maq.df$mcdi_understand_percentile)) 

combined.maq.df$mcdi_produce_percentile[combined.maq.df$mcdi_produce_percentile==1]<- NA
combined.maq.df$mcdi_understand_percentile[combined.maq.df$mcdi_understand_percentile==1]<- NA
