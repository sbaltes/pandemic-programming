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

source("functions.R")

################################################################################

VERSION <- "2020-04-16"

recoded <- fread(paste0("data/", VERSION, "/English_recoded.csv"), header=TRUE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null", -99, "NA"), stringsAsFactors=FALSE)

other_languages <- c("Turkish", "French", "Japanese", "Spanish", "Russian", "Italian", "Korean", "Portuguese", "Chinese", "Arabic", "Persian")
for (other_language in other_languages) {
  #print(other_language)
  recoded_new <- fread(paste0("data/", VERSION, "/", other_language, "_recoded.csv"), header=TRUE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null", -99), stringsAsFactors=FALSE)
  # fix IDs
  recoded_new$ID <- 1:nrow(recoded_new)
  recoded <- rbind(recoded, recoded_new)
  rm(recoded_new)
}
n <- nrow(recoded)
n
# 2225

# fix country names
recoded$Country <- clean_countries(recoded)

# analyze and export country information
table_countries <- sort(table(recoded$Country, useNA="ifany"), decreasing=TRUE)
countries <- data.frame(
  Country=names(table_countries),
  Responses=as.integer(table_countries),
  stringsAsFactors=FALSE
)
# write country information
write.table(table_countries, file=paste0("data/", VERSION, "/countries.csv"), sep=",", col.names=FALSE, row.names=FALSE, na="NA", quote=TRUE, qmethod="double", fileEncoding="UTF-8")

table_countries[table_countries >= 20]
# Germany         Russia         Brazil          Italy  United States   Korea, South        Belgium          China 
#     505            366            272            173             99             81             77             76 
# Turkey          India          Japan          Spain           Iran        Austria         Canada    Switzerland 
#     66             55             53             52             40             29             27             20 
# United Kingdom           <NA> 
#             20             20 

sum(table_countries[table_countries < 20])
# 194 (Other)

# split organizational support variables
recoded <- split_os_vars(recoded)

# analyze organisational support
os <- data.frame(
  rep("", 22),
  rep(0, 22),
  rep(0, 22),
  stringsAsFactors=FALSE
)
names(os) <- c("Variable", "Yes", "Helpful")
for (i in 1:22) {
  column_yes <- paste0("OS", i, "_y")
  column_helpful <- paste0("OS", i, "_h")
  os[i, 1] <- paste0("OS", i)
  os[i, 2] <- sum(recoded[[column_yes]], na.rm=TRUE)
  os[i, 3] <- sum(recoded[[column_helpful]], na.rm=TRUE)
}
# write OS results
write.table(os, file=paste0("data/", VERSION, "/os.csv"), sep=",", col.names=TRUE, row.names=FALSE, na="", quote=TRUE, qmethod="double", fileEncoding="UTF-8")

# drop organizational support variables
for (i in 1:22) {
  recoded[[paste0("OS", i)]] <- NULL
}

# select highest COVID status
recoded$COVIDStatus <- get_highest_covid_status(recoded)
table(recoded$COVIDStatus)
#    0    1    2    3    4    5 
# 1853  279   50   33    3    7

# delete non-aggregated COVID status columns
#for (status_number in 5:0) {
#  recoded[[paste0("COVIDStatus", status_number)]] <- NULL
#}

# get number of adult cohabitants
recoded$AdultCohabitants <- get_adult_cohabitants(recoded)
recoded$ChildCohabitants <- clean_child_cohabitants(recoded)
recoded$Coinhabitants <- NULL
recoded$Children <- NULL

# calculate FearResilience score
# weights taken from page 293 of http://cogprints.org/5245/1/Fear%26_Resilience_Bracha_%26_Burkle_Final_PDF.pdf
fr_weights <- c(5, 10, 5, 1, 1, 1, 5, -10, -3, 1)
FearResilience <- rep(0, n)
for (fr_number in 1:10) {
  FearResilience <- FearResilience + recoded[[paste0("FR", fr_number)]] * fr_weights[fr_number]
  recoded[[paste0("FR", fr_number)]] <- NULL
}
recoded$FearResilience <- FearResilience*0.1  # factor 0.1 to reduce variance

# calcuate relative performance measures
#recoded$PerRatioB <- recoded$PerB1item/recoded$PerformanceOthers
#recoded$PerRatioS <- recoded$PerS1item/recoded$PerformanceOthers

# calculate delta wellbeing measure
#SumWHO5B <- rowSums(subset(recoded, select=c("WHO5B1", "WHO5B2", "WHO5B3", "WHO5B4", "WHO5B5")))
#SumWHO5S <- rowSums(subset(recoded, select=c("WHO5S1", "WHO5S2", "WHO5S3", "WHO5S4", "WHO5S5")))
#recoded$DeltaWellbeing <- SumWHO5S - SumWHO5B
# delete columns
#for (who5_number in 1:5) {
#  recoded[[paste0("WHO5B", who5_number)]] <- NULL
#  recoded[[paste0("WHO5S", who5_number)]] <- NULL  
#}

# recode some of the scales
# Isolation
recoded$Isolation <- recoded$Isolation + 1 # 1...4
# DP
for (i in 1:5) {
  recoded[[paste0("DP", i)]] <- recoded[[paste0("DP", i)]] + 1  # 1...5 
}
# Erg
erg_mapping <- "-3=1; -2=2; -1=3; 1=4; 2=5; 3=6; else=NA"
for (i in 1:6) {
  recoded[[paste0("Erg", i)]] <- as.integer(car::recode(unlist(recoded[[paste0("Erg", i)]]), erg_mapping))  # 1...6
}
# WHO5B
for (i in 1:5) {
  recoded[[paste0("WHO5B", i)]] <- recoded[[paste0("WHO5B", i)]] + 1  # 1...6
}
# WHO5S
for (i in 1:5) {
  recoded[[paste0("WHO5S", i)]] <- recoded[[paste0("WHO5S", i)]] + 1  # 1...6
}
# HPQB
for (i in 1:7) {
  recoded[[paste0("HPQB", i)]] <- recoded[[paste0("HPQB", i)]] + 1  # 1...5
}
# HPQB8
hpq8_mapping <- "-3=1; -2=2; -1=3; 0=4; 1=5; 2=6; 3=7; else=NA" 
recoded$HPQB8 <- as.integer(car::recode(recoded$HPQB8, hpq8_mapping))  # 1...7
# HPQS
for (i in 1:7) {
  recoded[[paste0("HPQS", i)]] <- recoded[[paste0("HPQS", i)]] + 1  # 1...5
}
# HPQS8
recoded$HPQS8 <- as.integer(car::recode(recoded$HPQS8, hpq8_mapping))  # 1...7

# Disabilities
recoded$Disabilities <- recoded$Disabilities + 1  # 1, 2, 3

# COVIDStatus
recoded$COVIDStatus <- recoded$COVIDStatus + 1  # 1...6

# Education
recoded$Education <- recoded$Education + 1  # 1...5

# append "UTC" to converted timestamps
recoded$Timestamp <- paste0(recoded$Timestamp, " UTC")

# set timestamp as first column
recoded <- recoded[,c(61, 1:60, 62:109)]

# export the merged dataset
DATE <- "2020-04-16"
write.table(recoded, file=paste0("data/export_", DATE, ".csv"), sep=",", col.names=TRUE, row.names=FALSE, na="NA", quote=TRUE, qmethod="double", fileEncoding="UTF-8")
