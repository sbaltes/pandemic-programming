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

# function definitions (no need to change)
split_os_vars <- function(df) {
  for (i in 1:22) {
    column_name <- paste0("OS", i)
    column_yes <- paste0("OS", i, "_y")
    column_helpful <- paste0("OS", i, "_h")
    df[[column_yes]] <- as.integer(df[[column_name]] == "yes" | df[[column_name]] == "both")
    df[[column_helpful]] <- as.integer(df[[column_name]] == "helpful" | df[[column_name]] == "both")
    df[[column_name]] <- NULL
  }
  df
}
sum_difference <- function(df_1, df_2, column_name) {
  sum(df_1[[column_name]], na.rm=TRUE) - sum(df_2[[column_name]], na.rm=TRUE)
}
print_sum_difference <- function(df_1, df_2, column_name) {
  print(paste0(column_name, ": ", sum_difference(df_1, df_2, column_name)))
}

################################################################################
### UPDATE LANGUAGE
################################################################################

LANGUAGE <- "English"

recoded <- fread(paste0("data/", LANGUAGE, "_recoded.csv"), header=TRUE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
n <- nrow(recoded)
n
# 726

# split organizational support variables
recoded <- split_os_vars(recoded)

# read Paul's data for validation
recoded_val <- fread(paste0("data/Paul/PPQ-English-2020APR10.csv"), header=TRUE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
n_val <- nrow(recoded_val)
n_val
# 712

# split organizational support variables
recoded_val <- split_os_vars(recoded_val)

# write version for Paul with new organizational support variables
write.table(recoded_val, file=paste0("data/Paul/PPQ-English-2020APR10_recoded_2.csv"), sep=",", col.names=TRUE, row.names=FALSE, na="", quote=TRUE, qmethod="double", fileEncoding="UTF-8")

# validate recoding
recoded_subset <- recoded[1:n_val]
print(paste0("ID: ", Reduce(`&`, recoded_val$ID == recoded_subset$ID)))
print(paste0("Language: ", Reduce(`&`, recoded_val$Language == recoded_subset$Language)))
for (i in 5:0) {
  column_name <- paste0("COVIDStatus", i)
  print_sum_difference(recoded_val, recoded_subset, column_name)
}
print_sum_difference(recoded_val, recoded_subset, "Isolation")
for (i in 1:10) {
  column_name <- paste0("FR", i)
  print_sum_difference(recoded_val, recoded_subset, column_name)
}
for (i in 1:5) {
  column_name <- paste0("DP", i)
  print_sum_difference(recoded_val, recoded_subset, column_name)
}
for (i in 1:6) {
  column_name <- paste0("Erg", i)
  print_sum_difference(recoded_val, recoded_subset, column_name)
}
for (i in 1:22) {
  column_name <- paste0("OS", i, "_y")
  print_sum_difference(recoded_val, recoded_subset, column_name)
  column_name <- paste0("OS", i, "_h")
  print_sum_difference(recoded_val, recoded_subset, column_name)
}
for (i in 1:5) {
  column_name <- paste0("WHO5B", i)
  print_sum_difference(recoded_val, recoded_subset, column_name)
}
for (i in 1:5) {
  column_name <- paste0("WHO5S", i)
  print_sum_difference(recoded_val, recoded_subset, column_name)
}
for (i in 1:5) {
  column_name <- paste0("HPQB", i)
  print_sum_difference(recoded_val, recoded_subset, column_name)
}
for (i in 1:5) {
  column_name <- paste0("HPQS", i)
  print_sum_difference(recoded_val, recoded_subset, column_name)
}
print_sum_difference(recoded_val, recoded_subset, "PerformanceOthers")
print_sum_difference(recoded_val, recoded_subset, "PerB1item")
print_sum_difference(recoded_val, recoded_subset, "PerS1item")
print_sum_difference(recoded_val, recoded_subset, "YearsOfExperience")
print_sum_difference(recoded_val, recoded_subset, "YearsOfWorkFromHomeExperience")
print_sum_difference(recoded_val, recoded_subset, "Coinhabitants")
print_sum_difference(recoded_val, recoded_subset, "Children")
print_sum_difference(recoded_val, recoded_subset, "Age")
print_sum_difference(recoded_val, recoded_subset, "Education")
print_sum_difference(recoded_val, recoded_subset, "OrganizationSize")
print_sum_difference(recoded_val, recoded_subset, "Disabilities")
sort(table(recoded_val$Gender), decreasing=TRUE)
# Ma  Fe  Ot  NB 
# 648  54   7   3 
sort(table(recoded_subset$Gender), decreasing=TRUE)
# Male Female     NB 
# 648     54      3 
sort(table(recoded_val$Country), decreasing=TRUE)[1:5]
# Germany United States       Austria   Switzerland        Canada 
# 470            73            25            16            14 
sort(table(recoded_subset$Country), decreasing=TRUE)[1:5]
# Germany United States       Austria   Switzerland        Canada 
# 470            73            25            16            14 
