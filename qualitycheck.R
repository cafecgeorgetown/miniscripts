
################# Compute % passed quality checks by study ######################## 

### NOTE: Pipeline is needed to run this, but this script shows the logic of computing scores

quality.df <- data.frame(study = "x",
                         proportion_passed = 0)

for (s in unique(combined.maq.df$study)) {
  total_responses <- c()
  for (q in qs.list$qual) {
    total_responses <- append(total_responses, na.omit(combined.maq.df[combined.maq.df$study == s, q]))
  }
  score <- length(total_responses[total_responses=="TRUE"])/length(total_responses)
  new.row <- c(s, score)
  quality.df <- rbind(quality.df, new.row)
}

# drop first row
quality.df <- quality.df[-1,]

# write to csv
write.csv(quality.df, "quality_checks.csv", row.names=FALSE)