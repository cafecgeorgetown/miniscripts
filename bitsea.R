
# calculate bitsea 

# List all BITSEA columns but exclude those containing "quality_control"
bitsea_columns <- grep("BITSEA", names(df_selected), value = TRUE)
bitsea_columns <- setdiff(bitsea_columns, grep("quality_control", bitsea_columns, value = TRUE))

# Recode the BITSEA columns to numeric values (excluding those with "quality_control")
df_selected <- df_selected %>%
  mutate(across(all_of(bitsea_columns), ~ dplyr::recode(.x, 
                                                        "not_true_rarely" = 0, 
                                                        "somewhat_true_sometimes" = 1, 
                                                        "very_true_often" = 2, 
                                                        .default = NA_real_)))

# List of BITSEA columns for bitsea_competence
bitsea_competence_columns <- c("BITSEA_1_1_show_pleasure_success", "BITSEA_1_5_follow_rules", 
                               "BITSEA_1_10_seek_parent_upset", "BITSEA_1_13_respond_to_name", 
                               "BITSEA_1_15_affectionate", "BITSEA_2_1_play_well_with_other", 
                               "BITSEA_3_1_pay_attention", "BITSEA_3_3_help_when_other_hurt", 
                               "BITSEA_3_6_imitate_playful_sound", "BITSEA_5_2_point_to_show", 
                               "BITSEA_5_4_nurture_toy")

# Calculate bitsea_competence by summing the relevant BITSEA columns
df_selected <- df_selected %>%
  mutate(bitsea_competence = rowSums(select(., all_of(bitsea_competence_columns)), na.rm = TRUE))

# List all remaining BITSEA columns for bitsea_problem
bitsea_problem_columns <- setdiff(bitsea_columns, bitsea_competence_columns)

# Calculate bitsea_problem by summing the remaining BITSEA columns (excluding quality_control)
df_selected <- df_selected %>%
  mutate(bitsea_problem = rowSums(select(., all_of(bitsea_problem_columns)), na.rm = TRUE))

