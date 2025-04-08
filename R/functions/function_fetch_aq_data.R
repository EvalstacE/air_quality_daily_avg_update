###############################################################################
####### ---- Function to get air quality data with daily average ---- ######### 
###############################################################################

fetch_aq_data <- function() {
  
  # --- API Setup ---
  url = config::get("mtdeq_api")
  end_time <- as.numeric(as.POSIXct(Sys.Date()+1, tz = "UTC")) * 1000
  start_time <- as.numeric(as.POSIXct(Sys.Date()-30, tz = "UTC")) * 1000
  
  all_data <- data.frame()
  result_offset <- 0
  result_record_count <- 1000
  
  # --- API Pull Loop ---
  repeat {
    response <- httr::GET(url, query = list(
      where = paste0("sitename = 'Helena' AND parameter = 'PM25_COL' AND datetime >= ", start_time, " AND datetime <= ", end_time),
      outFields = "*",
      f = "json",
      resultOffset = result_offset,
      resultRecordCount = result_record_count
    ))
    
    # Parse JSON response
    data <- fromJSON(content(response, "text"))
    
    # Check if response has data 
    if (!is.null(data$features) && length(data$features) > 0) {
      df <- as.data.frame(data$features$attributes)
      
      # Convert datetime from UNIX timestamp (Check if API is using seconds or milliseconds)
      if ("datetime" %in% names(df)) {
        if (max(df$datetime, na.rm = TRUE) > 1e12) {
          # API is providing milliseconds (convert by dividing by 1000)
          df$datetime <- as.POSIXct(df$datetime / 1000, origin = "1970-01-01", tz = "UTC")
        } else {
          # API is providing seconds (no conversion needed)
          df$datetime <- as.POSIXct(df$datetime, origin = "1970-01-01", tz = "UTC")
        }
      }
      
      # Ensure datetime is correctly formatted
      df$datetime <- ymd_hms(df$datetime, tz = "UTC")
      
      # Add derived date and hour columns
      df <- df %>%
        filter(!is.na(date)) %>%
        mutate(
          datetime_mst = lubridate::with_tz(datetime, "America/Denver"),
          date_mst = as.Date(format(datetime, tz = "America/Denver")),
          year = as.integer(format(datetime, "%Y", tz = "America/Denver"))
          
        ) %>%
        
        select(year, datetime_mst, date_mst, sitename, parameter, aqs_value)
      
      # Append to the full dataset
      all_data <- bind_rows(all_data, df)
      
      # If the number of results is less than the max request limit, stop fetching
      if (nrow(df) < result_record_count) {
        break
      }
      
      # Update offset for next batch
      result_offset <- result_offset + result_record_count
    } else {
      break  # No more data
    }
  }
  
# Calculate daily average and sort by date
all_data <- all_data %>%
  filter(!is.na(date_mst)) %>%
  group_by(year,date_mst, sitename, parameter) %>%
    summarise(
      daily_avg_aqs_value = mean(aqs_value, na.rm = TRUE),
      .groups = "drop") %>% 
  mutate(
    above_35 = ifelse(daily_avg_aqs_value >= 35, TRUE, FALSE),
    aqs_category = cut(daily_avg_aqs_value, 
                       breaks = c(-Inf, 12, 20.4, 35.4, 80.4, 150.4, 600),  
                       labels = c("Good", "Moderate", "Unhealthy for Sensitive Groups", 
                                  "Unhealthy", "Very Unhealthy", "Hazardous"), 
                       include.lowest = TRUE)
    ) %>%
  
  ungroup() %>%
  arrange(desc(date_mst))
  
# Return final dataframe  
return(all_data)
  
}
