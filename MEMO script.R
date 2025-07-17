
####  READ ME ####

# This script assumes you are using a data frame called "data"
# Run the lines below after removing the hashtags and updating the path name

#### RUN THESE LINES IN YOUR .RMD FILE AFTER UPDATING FILE PATH ####

# scriptspath <- "ENTER YOUR PATH HERE/"
# source(paste0(scriptspath, "MEMO script.R"), local = knitr::knit_global())

####  MODULE ####

# variable names
parreg_names <- c("parent_rum_breakdifficult",  "parent_rum_calmdontyell",
                  "parent_rum_breakstress", "parent_rum_lesslonely")
parrelax_names <- c("parent_rum_entertain", "parent_rum_relax", "parent_rum_reduceboredom",
                    "parent_rum_escapeoverwhelm", "parent_rum_habit")
chreg_names <- c("child_rum_stopmoving", "child_rum_calmupset", "child_rum_newsituation",
                 "child_rum_demands", "child_rum_sleep", "child_rum_reward")
chentertain_names <- c("child_rum_learn", "child_rum_occupy", "child_rum_together")

# compute means
data$memo_parreg <- rowMeans(data[parreg_names], na.rm=TRUE)
data$memo_parrelax <- rowMeans(data[parrelax_names], na.rm=TRUE)
data$memo_chreg <- rowMeans(data[chreg_names], na.rm=TRUE)
data$memo_chentertain <- rowMeans(data[chentertain_names], na.rm=TRUE)

# reliability - parreg
print("Reliability for MEMO: Parent Reg")
print(alpha(data[parreg_names])$total)

# reliability - parrelax
print("Reliability for MEMO: Parent Relax")
print(alpha(data[parrelax_names])$total)

# reliability - chreg
print("Reliability for MEMO: Child Reg")
print(alpha(data[chreg_names])$total)

# reliability - chentertain
print("Reliability for MEMO: Child Entertain")
print(alpha(data[chentertain_names])$total)

# distributions
hist(data$memo_parreg)
hist(data$memo_parrelax)
hist(data$memo_chreg)
hist(data$memo_chentertain)