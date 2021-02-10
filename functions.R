# function definitions

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

get_highest_covid_status <- function(df) {
  result <- rep(NA, nrow(df))
  for (i in 1:nrow(df)) {
    max_status = 0;
    for (status_number in 5:0) {
      status_set <- as.logical(subset(df[i], select=paste0("COVIDStatus", status_number)))
      if (is.na(status_set)) {
        status_set <- FALSE # for some languages, NA is set instead of 0
      }
      if (status_set) {
        max_status <- status_number
        break
      }
    }
    result[i] <- max_status
  }
  result
}

clean_child_cohabitants <- function(df) {
  child_cohabitants <- df$Children
  child_cohabitants[child_cohabitants < 0] <- NA  # some particpants provided a negative value...
  child_cohabitants
}

get_adult_cohabitants <- function(df) {
  adult_cohabitants <- df$Coinhabitants - df$Children - 1  # the respondent themselve has to be substracted
  adult_cohabitants[adult_cohabitants < 0] <- NA
  adult_cohabitants
}

clean_countries <- function(df) {
  country <- df$Country
  country[country == "Russia and Germany"] <- NA
  country[country == "Suíça"] <- "Switzerland"
  country
}
