# calculate JMES 

# Select columns that contain "jme_" in their name
jme_columns <- grep("jme_", names(df_selected), value = TRUE)

# Define the mapping for responses
response_map <- c("never" = 0, "rarely" = 1, "occasionally" = 2, "often" = 3, "always" = 4)

# Apply the transformation to the selected columns
df_selected <- df_selected %>%
  mutate(across(all_of(jme_columns), ~ dplyr::recode(.x, !!!response_map), .names = "{.col}"))

jme_video_columns <- grep("jme_video", names(df_selected), value = TRUE)
jme_game_columns <- grep("jme_game", names(df_selected), value = TRUE)

# Add jme_video_score and jme_game_score by summing the respective columns
df_selected <- df_selected %>%
  mutate(
    jme_video_score = rowSums(select(., all_of(jme_video_columns)), na.rm = TRUE),
    jme_game_score = rowSums(select(., all_of(jme_game_columns)), na.rm = TRUE)
  )