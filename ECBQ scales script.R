
####  READ ME ####

# This script assumes you are using a data frame called "data"
# Run the lines below after removing the hashtags and updating the path name

#### RUN THESE LINES IN YOUR .RMD FILE AFTER UPDATING FILE PATH ####

# scriptspath <- "ENTER YOUR PATH HERE/"
# source(paste0(scriptspath, "ECBQ scales script.R"), local = knitr::knit_global())

####  MODULE ####

# Open scoring key
key <- readxl::read_excel(paste0(scriptspath, "ECBQ Scoring.xlsx"))

# Get variable names and remove attention checks
ecbq_vars <- grep("ECBQ", names(data), value = TRUE)
ecbq_vars <- ecbq_vars[!grepl("quality", ecbq_vars)]

# Subet ecbq items, recode 'does not apply' as NA, and reverse score items
subdata <- data[ecbq_vars]
subdata[,ecbq_vars][subdata[,ecbq_vars] == 997] <- NA
revitems <- key[key$ReverseScales3 == "Y",]$ItemNum
for (i in revitems) {
  subdata[ecbq_vars[i]] <- (subdata[ecbq_vars[i]]-6)*-1
}

# Add mean scores to main data frame
for (i in unique(key$Scales3)) {
  #print(i)
  items <- key[key$Scales3 == i,]$ItemNum
  data[paste0("ecbq_", i)] <- rowMeans(subdata[ecbq_vars[items]], na.rm=T)
  print(paste0("Reliability for ECBQ - ", i))
  print(alpha(subdata[ecbq_vars[items]])$total)
}

rm(list = c("key"))
