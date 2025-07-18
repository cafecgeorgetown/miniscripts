# calculate workplace interference

# Select columns that contain "work_" in their name
work_columns <- grep("work_", names(df_selected), value = TRUE)

# Define the mapping for responses on a 1–6 agreement scale
response_map <- c(
  "strongly_disagree" = 1,
  "disagree" = 2,
  "somewhat_disagree" = 3,
  "somewhat_agree" = 4,
  "agree" = 5,
  "strongly_agree" = 6
)

# Apply the transformation to the selected columns
df_selected <- df_selected %>%
  mutate(across(all_of(work_columns), ~ dplyr::recode(.x, !!!response_map), .names = "{.col}"))

# Add work_interference_score (sum of all work_ items)
df_selected <- df_selected %>%
  mutate(
    work_interference_score = rowSums(select(., all_of(work_columns)), na.rm = TRUE)
  )