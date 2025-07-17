
########## Calculate Total Screen Time #####################################

# replace variables with the midpoint 
combined.maq.df <- combined.maq.df %>%
  group_by(masked_pid, wave) %>%
  mutate(across(ends_with(c("_time_weekday", "_time_weekend")), 
                ~ case_when(. == "never" ~ 0,
                            . == "less30m" ~ 15,
                            . == "30m1h" ~ 45,
                            . == "1h2h" ~ 90,
                            . == "2h3h" ~ 150,
                            . == "3h4h" ~ 210,
                            . == "5h6h" ~ 270,
                            . == "more5h"  ~ 330,
                            TRUE ~ NA_real_
                )))

## Set weights for averaging the typical screen time value
weights <- c(5,2)

## Calculate typical screen time using weighted mean
combined.maq.df <- combined.maq.df %>%
  group_by(masked_pid, wave) %>%
  mutate(tv_typical = weighted.mean(x = c(child_tv_time_weekday,child_tv_time_weekend), w = weights, na.rm = TRUE),
         computer_typical = weighted.mean(x = c(child_computer_time_weekday,child_computer_time_weekend), w = weights, na.rm = TRUE),
         ebook_typical = weighted.mean(x = c(child_electro_book_time_weekday,child_electro_book_time_weekend), w = weights, na.rm = TRUE),
         videogame_typical = weighted.mean(x = c(child_videogame_time_weekday,child_videogame_time_weekend), w = weights, na.rm = TRUE),
         tablet_typical = weighted.mean(x = c(child_tablet_time_weekday,child_tablet_time_weekend), w = weights, na.rm = TRUE),
         smartphone_typical = weighted.mean(x = c(child_smartphone_time_weekday,child_smartphone_time_weekend), w = weights, na.rm = TRUE),
         virtual_typical = weighted.mean(x = c(child_virtual_time_weekday,child_virtual_time_weekend), w = weights, na.rm = TRUE)) %>%
  mutate(screentime_weekday = sum(across(child_tv_time_weekday:child_virtual_time_weekday & ends_with("_weekday") & !starts_with("child_paper")), na.rm = TRUE),
         screentime_weekend = sum(across(child_tv_time_weekend:child_virtual_time_weekend & ends_with("_weekend") & !starts_with("child_paper")), na.rm = TRUE),
         screentime_typical = weighted.mean(x = c(screentime_weekday,screentime_weekend), w = weights, na.rm = TRUE),
         screentime_ave = mean(x = c(screentime_weekday,screentime_weekend), w = weights, na.rm = TRUE))

# convert back to original scale 
combined.maq.df <- combined.maq.df %>%
  group_by(masked_pid, wave) %>%
  mutate(across(ends_with(c("_time_weekday", "_time_weekend")), 
                ~ case_when(. == 0 ~ "never",
                            . == 15 ~ "less30m",
                            . == 45 ~ "30m1h",
                            . == 90 ~ "1h2h",
                            . == 150 ~ "2h3h",
                            . == 210 ~ "3h4h",
                            . == 270 ~ "5h6h",
                            . == 330 ~ "more5h"
                ))) 

## Add screentime for Israel ## 

########### SKIP if not running Israel ############### 

combined.maq.df <- combined.maq.df %>%
  mutate(tv_typical = ifelse(country == "Israel", as.numeric(activity_tv_Israel), tv_typical),
         computer_typical = ifelse(country == "Israel", as.numeric(activity_mediaL_Israel), computer_typical),
         videogame_typical = ifelse(country == "Israel", as.numeric(activity_SonyP_Israel), videogame_typical),
         tablet_typical = ifelse(country == "Israel", as.numeric(activity_mediaP_Israel), tablet_typical),
         screentime_typical = ifelse(country == "Israel", sum(c(as.numeric(tv_typical), 
                                                                as.numeric(computer_typical), 
                                                                as.numeric(videogame_typical),
                                                                as.numeric(tablet_typical)),na.rm = TRUE), screentime_typical)) 


# correcting screentime for emu and Sweden 
#### NOTE: will have to do this for other v2 studies as well depending on how they ask about screentime. Use this template. 

screentime_combined.maq.df <- combined.maq.df[combined.maq.df$study == "emu" | combined.maq.df$study == "sweden", c("video_daily_weekday", "game_daily_weekday", "chat_daily_weekday", "ebook_daily_weekday")] %>% mutate_at(1:4, as.numeric)
combined.maq.df[combined.maq.df$study == "emu" | combined.maq.df$study == "sweden",]$screentime_weekday <- rowSums(screentime_combined.maq.df, na.rm = TRUE)
combined.maq.df[combined.maq.df$study == "emu" | combined.maq.df$study == "sweden",]$screentime_weekend <- rowSums(screentime_combined.maq.df, na.rm = TRUE)

# calculate typical and average 
combined.maq.df$screentime_weekday <- as.numeric(combined.maq.df$screentime_weekday)
combined.maq.df$screentime_weekend <- as.numeric(combined.maq.df$screentime_weekend)
weights <- c(5,2)
combined.maq.df[combined.maq.df$study == "emu" | combined.maq.df$study == "sweden",] <- combined.maq.df[combined.maq.df$study == "emu" | combined.maq.df$study == "sweden",] %>% 
  group_by(masked_pid, wave) %>%
  mutate(screentime_typical = weighted.mean(x = c(screentime_weekday,screentime_weekend), w = weights, na.rm = TRUE),
         screentime_ave = mean(x = c(screentime_weekday,screentime_weekend), w = weights, na.rm = TRUE))


