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

LANGUAGE <- "Japanese"
DATASET_VERSION <- "2020-04-16"
FILTER_ANSWER <- "COVID-19 パンデミックの前はオフィスで仕事をした。今は自宅で仕事をしている。"

raw <- fread(paste0("data/", DATASET_VERSION, "/", LANGUAGE, ".csv"), header=TRUE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
raw[raw == ""] <- NA  # treat empty strings as NA
n <- nrow(raw)
n
# 65

sort(table(raw[,3]), decreasing=TRUE)
# COVID-19 パンデミックの前はオフィスで仕事をした。今は自宅で仕事をしている。
# 52                                                                           
# ずっとリモートで仕事をしている。（以降の質問はありません） 
# 8 
# ずっとオフィスで仕事をしている。（以降の質問はありません） 
# 5 

passing_filter <- as.logical(raw[,3] == FILTER_ANSWER)

# filter raw responses using filter question and by dropping columsn two, three, and the last (donation question)
filtered <- subset(raw[passing_filter], select=c(1,4:(ncol(raw)-1)))
n <- nrow(filtered)
n
# 52

rm(raw)

# remove empty responses
filtered <- filtered[rowSums(is.na(filtered)) != ncol(filtered)]
n <- nrow(filtered)
n
# 52

# add column indicating open-ended answers and drop corresponding columns
open_ended_columns <- (ncol(filtered)-3):ncol(filtered)
filtered$AtLeastOneOpenResponse <- rowSums(!is.na(subset(filtered, select=open_ended_columns))) > 0
filtered <- subset(filtered, select=-open_ended_columns)

# write filtered results
write.table(filtered, file=paste0("data/", DATASET_VERSION, "/", LANGUAGE, "_filtered.csv"), sep=",", col.names=TRUE, row.names=FALSE, na="", quote=TRUE, qmethod="double", fileEncoding="UTF-8")
