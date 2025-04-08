# run_update.R

library(googlesheets4)
library(httr)
library(jsonlite)
library(dplyr)
library(lubridate)
library(readr)
library(config)
library(tidyr)


source("R/functions/function_fetch_aq_data.R")
source("R/functions/function_append_to_google.R")

tryCatch({
  append_to_google()
  message("Weekly air quality update complete.")
}, error = function(e) {
  message("❌ Update failed: ", e$message)
})
