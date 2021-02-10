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
library(fasttime)
library(hms)

################################################################################

LANGUAGE <- "English"
DATASET_VERSION <- "2020-04-16"

recoded <- fread(paste0("data/", DATASET_VERSION, "/", LANGUAGE, "_recoded.csv"), header=TRUE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
recoded[recoded == ""] <- NA  # treat empty strings as NA

process_timestamps <- function(timestamps, dateformat, timezone) {
  # get rid of additional information present for the data processed with R, keep AM/PM information
  if (grepl("[:alpha:]", timestamps[1])) {
    timestamps <- gsub("(.+) (AM|PM) (.+)", "\\1\\2", toupper(timestamps))
  }
   
  # we have to process date and time separately, because Excel dropped the leading zeros for days and months
  dates_times <- strsplit(timestamps, " ")
  dates <- unlist(dates_times)[2*(1:length(timestamps))-1]
  
  # fix missing padding with zeros
  dates_split <- strsplit(dates, "/")
  dates_converted <- rep("", length(timestamps))
  for (i in 1:length(timestamps)) {
    row <- unlist(dates_split[i])
    result <- ""
    if (nchar(row[1]) == 1) {
      result <- paste0("0", row[1])
    } else {
      result <- row[1]
    }
    if (nchar(row[2]) == 1) {
      result <- paste0(result, "/0", row[2])
    } else {
      result <- paste0(result, "/", row[2])
    }
    if (nchar(row[3]) == 1) {
      result <- paste0(result, "/0", row[3])
    } else {
      result <- paste0(result, "/", row[3])
    }
    dates_converted[i] <- result
  }
  
  times <- unlist(dates_times)[2*(1:length(timestamps))]

  # convert AM/PM into a reasonable time format
  for (i in 1:length(times)) {
    if (endsWith(times[i], "PM")) {
      time_parts <- unlist(strsplit(times[i], ":"))
      times[i] <- paste((as.integer(time_parts[1])+12)%%24, time_parts[2], time_parts[3], sep=":")
    }
    times[i] <- gsub("(AM|PM)", "", times[i])
  }
  
  # parse dates and times
  dates <- as.character(as.Date(dates_converted, format=dateformat, tz="UTC"))
  times <- as.character(hms::as.hms(times))

  # parse adapted timestamps
  timestamps_converted <- as.POSIXct(
    paste(dates, times, sep=" "), # adapt time zone depending on input file
    format = "%Y-%m-%d %H:%M:%S",
    tz=timezone,
    usetz = TRUE
  )
  
  # convert to UTC, see https://stackoverflow.com/a/37689169
  attr(timestamps_converted, "tzone") <- "UTC"
  
  return(timestamps_converted)
}

# plot available time zones
#OlsonNames()

# override existing timestamp column
if (LANGUAGE == "Arabic") {
  recoded$Timestamp <- process_timestamps(recoded$Timestamp, "%m/%d/%y", "Etc/GMT-3")
}
if (LANGUAGE == "Chinese") {
  recoded$Timestamp <- process_timestamps(recoded$Timestamp, "%Y/%m/%d", "Etc/GMT-8")
}
if (LANGUAGE == "English") {
  recoded$Timestamp <- process_timestamps(recoded$Timestamp, "%Y/%m/%d", "Etc/GMT-2")
}
if (LANGUAGE == "French") {
  recoded$Timestamp <- process_timestamps(recoded$Timestamp, "%Y/%m/%d", "Etc/GMT-2")
}
if (LANGUAGE == "Italian") {
  recoded$Timestamp <- process_timestamps(recoded$Timestamp, "%m/%d/%y", "Etc/GMT-2")
}
if (LANGUAGE == "Japanese") {
  recoded$Timestamp <- process_timestamps(recoded$Timestamp, "%Y/%m/%d", "Etc/GMT-9")
}
if (LANGUAGE == "Korean") {
  recoded$Timestamp <- process_timestamps(recoded$Timestamp, "%m/%d/%Y", "Etc/GMT-9")
}
if (LANGUAGE == "Persian") {
  recoded$Timestamp <- process_timestamps(recoded$Timestamp, "%m/%d/%Y", "Etc/GMT+7")
}
if (LANGUAGE == "Portuguese") {
  recoded$Timestamp <- process_timestamps(recoded$Timestamp, "%m/%d/%Y", "Etc/GMT+3")
}
if (LANGUAGE == "Russian") {
  recoded$Timestamp <- process_timestamps(recoded$Timestamp, "%Y/%m/%d", "Etc/GMT-2")
}
if (LANGUAGE == "Spanish") {
  recoded$Timestamp <- process_timestamps(recoded$Timestamp, "%Y/%m/%d", "Etc/GMT-2")
}
if (LANGUAGE == "Turkish") {
  recoded$Timestamp <- process_timestamps(recoded$Timestamp, "%Y/%m/%d", "Etc/GMT-10")
}

# write recoded results
write.table(recoded, file=paste0("data/", DATASET_VERSION, "/", LANGUAGE, "_recoded.csv"), sep=",", col.names=TRUE, row.names=FALSE, na="", quote=TRUE, qmethod="double", fileEncoding="UTF-8")
