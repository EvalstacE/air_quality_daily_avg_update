# run_update_monthly.R

library(googlesheets4)
library(httr)
library(jsonlite)
library(dplyr)
library(lubridate)
library(readr)
library(config)
library(tidyr)

extract_sheet_id <- function(url_or_id) {
  id <- stringr::str_extract(url_or_id, "(?<=/d/)[a-zA-Z0-9_-]+")
  if (!is.na(id)) return(id)
  return(url_or_id)
}

sheet_id <- extract_sheet_id(Sys.getenv("SHEET_ID"))
message("📋 Cleaned SHEET_ID: ", sheet_id)


googlesheets4::gs4_auth()
source("R/functions/function_fetch_monthly_avgs.R")
source("R/functions/function_append_monthly_google.R")



tryCatch({
  append_monthly_google(sheet_id)
  message("Weekly air quality update complete.")
}, error = function(e) {
  message("❌ Update failed: ", e$message)
})