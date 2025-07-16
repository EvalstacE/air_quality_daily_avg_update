###############################################################################
###################### --- Function to append --- ##############################
###############################################################################


source("R/functions/function_fetch_monthly_avgs.R")


append_monthly_google <- function(sheet_id) {
  
  new_fetch <- fetch_monthly_avgs()
  
  existing <- tryCatch(
    read_sheet(sheet_id, sheet = "monthly_avgs"),
    error = function(e) tibble()
  )
  
  if (nrow(existing) > 0) {
    
    # Join and overwrite duplicates
    # Clean up types
    existing <- existing %>%
      mutate(
        year = as.numeric(year),
        month = as.numeric(month),
        monthly_avg_aqs_value = as.numeric(monthly_avg_aqs_value)
        
      )
    
    new_fetch <- new_fetch %>%
      mutate(
        year = as.numeric(year),
        month = as.numeric(month),
        monthly_avg_aqs_value = as.numeric(monthly_avg_aqs_value)
      )
# - only rows that exactly match year AND month from the new dataframe
## -- are replaced — all other months from that year (or other years) 
### --- will be preserved   
    
    updated <- existing %>%
      anti_join(new_fetch, by = c("year", "month")) %>%
      bind_rows(new_fetch) %>%
      arrange(desc(year), desc(month))
    
    range_clear(sheet_id, sheet = "monthly_avgs")
    sheet_write(updated, ss = sheet_id, sheet = "monthly_avgs")
    message("✅ Updated monthly_avgs sheet with new + updated records.")
    
  } else {
    
    # First time write
    sheet_write(new_fetch, ss = sheet_id, sheet = "monthly_avgs")
    message("✅ Wrote new daily_avg sheet.")
  }
  
  # --- Overwrite snapshot ---
  range_clear(sheet_id, sheet = "snapshot_monthly")
  sheet_write(new_fetch, ss = sheet_id, sheet = "snapshot_monthly")
  message("📸 Snapshot updated.")
  
  return(invisible(new_fetch))
}