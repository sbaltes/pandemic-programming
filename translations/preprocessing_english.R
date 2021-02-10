# set working directory (see https://stackoverflow.com/a/35842119)
dir = tryCatch({
  # script being sourced
  getSrcDirectory()[1]
}, error = function(e) {
  # script being run in RStudio
  dirname(rstudioapi::getActiveDocumentContext()$path)
})
setwd(dir)

library(data.table)

################################################################################
### UPDATE LANGUAGE, FILTER ANSWER, OUTPUTS FOR N, AND TABLE FOR FILTER QUESTION
################################################################################

LANGUAGE <- "English"
DATASET_VERSION <- "2020-04-16"
FILTER_ANSWER <- "Before the pandemic, I was working at the office. Now I am working from home."

raw <- fread(paste0("data/", DATASET_VERSION, "/", LANGUAGE, ".csv"), header=TRUE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
raw[raw == ""] <- NA  # treat empty strings as NA
n <- nrow(raw)
n
# 897

sort(table(raw[,3]), decreasing=TRUE)
# Before the pandemic, I was working at the office. Now I am working from home. 
# 806 
# I've been working remotely the whole time. 
# 72
# I've been working at the office the whole time. 
# 18 
# Before the pandemic I was working remotely. Now I'm working at the office. 
# 1

passing_filter <- as.logical(raw[,3] == FILTER_ANSWER)

# filter raw responses using filter question and by dropping columsn two, three, and the last (donation question)
filtered <- subset(raw[passing_filter], select=c(1,4:(ncol(raw)-1)))
n <- nrow(filtered)
n
# 806

rm(raw)

# remove empty responses
filtered <- filtered[rowSums(is.na(filtered)) != ncol(filtered)]
n <- nrow(filtered)
n
# 806 

# add column indicating open-ended answers and drop corresponding columns
open_ended_columns <- (ncol(filtered)-3):ncol(filtered)
filtered$AtLeastOneOpenResponse <- rowSums(!is.na(subset(filtered, select=open_ended_columns))) > 0
filtered <- subset(filtered, select=-open_ended_columns)

# write filtered results
write.table(filtered, file=paste0("data/", DATASET_VERSION, "/", LANGUAGE, "_filtered.csv"), sep=",", col.names=TRUE, row.names=FALSE, na="", quote=TRUE, qmethod="double", fileEncoding="UTF-8")
