## CAFE Mediation full scale (adapted Valkenburg Scale)

# calculate mediation 

# Select columns that contain "med_" in their name
med_columns <- grep("med_", names(df_selected), value = TRUE)

# Define the mapping for responses
response_map <- c("never" = 0, "rarely" = 1, "sometimes" = 2, "somtimes" = 2, "often" = 3, "veryoften" = 4)

# Apply the transformation to the selected columns
df_selected <- df_selected %>%
  mutate(across(all_of(med_columns), ~ dplyr::recode(.x, !!!response_map), .names = "{.col}"))

# Define scale items
active_items <- c(
  "med_1_choosequality", "med_2_whathappening", "med_3_howtouse", 
  "med_4_identifyads", "med_5_helplearn", "med_11_understandsee",
  "med_12_badthings", "med_13_explainmeaning", "med_14_explainmotives",
  "med_15_goodthings"
)

restrictive_items <- c(
  "med_6_settimes", "med_7_restrictamount", "med_8_stopunsuitable",
  "med_9_telladvance", "med_10_forbidcertain"
)

# Create scale scores using rowMeans
df_selected <- df_selected %>%
  mutate(
    active_mediation_scale = rowMeans(select(., all_of(active_items)), na.rm = TRUE),
    restrictive_mediation_scale = rowMeans(select(., all_of(restrictive_items)), na.rm = TRUE)
  )







## CAFE Mediation adjusted scale (validated for younger children)

# calculate mediation 

# Select columns that contain "med_" in their name
med_columns <- grep("med_", names(df_selected), value = TRUE)

# Define the mapping for responses
response_map <- c("never" = 0, "rarely" = 1, "sometimes" = 2, "somtimes" = 2, "often" = 3, "veryoften" = 4)

# Apply the transformation to the selected columns
df_selected <- df_selected %>%
  mutate(across(all_of(med_columns), ~ dplyr::recode(.x, !!!response_map), .names = "{.col}"))

# Define scale items
active_items <- c(
  "med_1_choosequality", "med_2_whathappening", "med_9_telladvance", 
  "med_11_understandsee", "med_14_explainmotives", "med_15_goodthings"
)

restrictive_items <- c(
  "med_8_stopunsuitable", "med_10_forbidcertain", "med_12_badthings"
)

# Create scale scores using rowMeans
df_selected <- df_selected %>%
  mutate(
    active_mediation_scale = rowMeans(select(., all_of(active_items)), na.rm = TRUE),
    restrictive_mediation_scale = rowMeans(select(., all_of(restrictive_items)), na.rm = TRUE)
  )