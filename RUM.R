# calculate rums 

# Select columns that contain "_rum" in their name
rum_columns <- grep("_rum", names(df_selected), value = TRUE)

# Define the mapping for responses
response_map <- c("never" = 0, "rarely" = 1, "sometimes" = 2, "often" = 3, "veryoften" = 4)

# Apply the transformation to the selected columns
df_selected <- df_selected %>%
  mutate(across(all_of(rum_columns), ~ dplyr::recode(.x, !!!response_map), .names = "{.col}"))

child_rum_columns <- grep("child_rum", names(df_selected), value = TRUE)
parent_rum_columns <- grep("parent_rum", names(df_selected), value = TRUE)

# Add child_rum_score and parent_rum_score by summing the respective columns
df_selected <- df_selected %>%
  mutate(
    child_rum_score = rowSums(select(., all_of(child_rum_columns)), na.rm = TRUE),
    parent_rum_score = rowSums(select(., all_of(parent_rum_columns)), na.rm = TRUE)
  )
