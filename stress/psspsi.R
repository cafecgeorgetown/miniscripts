########## Calculate PSS and PSI Scores #################################

# PSI 

# select psi questions and delete quality check 
df3 <- combined.maq.df %>% select(contains("psi_"))
df3 <- df3[,-36]

# convert to 5 point scale 
df3[df3 == "strongly_disagree"] <- "1"
df3[df3 == "disagree"] <- "2"
df3[df3 == "not_sure"] <- "3"
df3[df3 == "agree"] <- "4"
df3[df3 == "strongly_agree"] <- "5"

df3[df3 == "not_very_good"] <- "1"
df3[df3 == "some_trouble"] <- "2"
df3[df3 == "average"] <- "3"
df3[df3 == "better_than_average"] <- "4"
df3[df3 == "very_good"] <- "5"

df3[df3 == "much_easier"] <- "1"
df3[df3 == "somewhat_easier"] <- "2"
df3[df3 == "as_hard"] <- "3"
df3[df3 == "somewhat_harder"] <- "4"
df3[df3 == "much_harder"] <- "5"

df3[df3 == "3_fewer"] <- "1"
df3[df3 == "4_5"] <- "2"
df3[df3 == "6_7"] <- "3"
df3[df3 == "8_9"] <- "4"
df3[df3 == "10_more"] <- "5"

# sum to get total score 
df3[] <- lapply(df3, as.numeric)
df3$psi_total_score <- rowSums(df3)

combined.maq.df$psi_total_score <- df3$psi_total_score

# calculate a z score 

combined.maq.df <- combined.maq.df %>% 
  mutate(psi_zscore = (psi_total_score - mean(psi_total_score, na.rm = TRUE))/sd(psi_total_score, na.rm = TRUE))

# calculate percentile 

psi_norms <- read_csv("norm_tables/PSI_norm_table.csv")
combined.maq.df$psi_percentile <- psi_norms$percentile[match(combined.maq.df$psi_total_score, psi_norms$score)]

# PSS 

# select pss variables
df4 <- combined.maq.df %>% select(contains("pss_"))

# convert to 5 point scale 
df4[df4 == "strongly_disagree"] <- "1"
df4[df4 == "disagree"] <- "2"
df4[df4 == "undecided"] <- "3"
df4[df4 == "agree"] <- "4"
df4[df4 == "strongly_agree"] <- "5"

# summ to get total score
df4[] <- lapply(df4, as.numeric)
df4$pss_total_score <- rowSums(df4)

combined.maq.df$pss_total_score <- df4$pss_total_score

# calculate z score 

combined.maq.df <- combined.maq.df %>% 
  mutate(pss_zscore = (pss_total_score - mean(pss_total_score, na.rm = TRUE))/sd(pss_total_score, na.rm = TRUE))
