###############################################################################
###################### --- Function to append --- ##############################
###############################################################################


source("R/functions/function_fetch_aq_data.R")


append_to_google <- function(sheet_id) {
  
  new_fetch <- fetch_aq_data()

  existing <- tryCatch(
    read_sheet(sheet_id, sheet = "daily_avg"),
    error = function(e) tibble()
  )
  
  if (nrow(existing) > 0) {
    
# Join and overwrite duplicates
    updated <- existing %>%
      filter(!date_mst %in% new_fetch$date_mst) %>%
      bind_rows(new_fetch) %>%
      arrange(date_mst)
    
    range_clear(sheet_id, sheet = "daily_avg")
    sheet_write(updated, ss = sheet_id, sheet = "daily_avg")
    message("✅ Updated daily_avg sheet with new + updated records.")
    
  } else {
    
# First time write
    sheet_write(existing, ss = sheet_id, sheet = "daily_avg")
    message("✅ Wrote new daily_avg sheet.")
  }
  
# --- Overwrite snapshot ---
  range_clear(sheet_id, sheet = "snapshot")
  sheet_write(new_fetch, ss = sheet_id, sheet = "snapshot")
  message("📸 Snapshot updated.")
  
  return(invisible(new_fetch))
}