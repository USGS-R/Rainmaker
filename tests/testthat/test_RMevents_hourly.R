
# Test script to compare x-minute interval rain data to hourly interval rain data
# Tested on ./data/05408480.csv and ./data/rain.test.csv



################################################
########### Testing for 05408480.csv ###########
################################################

# Import test script
# This data contains rain totals every 15 minutes
quarterly <- read.csv(system.file("extdata/05408480.csv", package = "Rainmaker"))

quarterly$dateTime = as.POSIXct(quarterly$dateTime, 
                                      format = "%Y-%m-%d %H:%M:%S",
                                      tz = "America/Chicago")

library(dplyr)
# Format dateTime into POSIXct
# Summarise quarterly rain data into hourly rain data
hourly <- dplyr::mutate(quarterly, hour = lubridate::floor_date(dateTime, unit = "hour")) %>%
  dplyr::group_by(hour) %>%
  dplyr::summarise(rain_hourly = sum(rain))


# Run RMevents.R on quarterly and hourly data
quarterly_result <- RMevents(quarterly, 
                             ieHr = 12,
                             rainthres = 0.1,
                             rain = "rain",
                             time = "dateTime")

hourly_result <- RMevents(hourly, 
                          ieHr = 12,
                          rainthresh = 0.1,
                          rain = "rain_hourly",
                          time = "hour")



# Import test script
rain.test <- read.csv(system.file("extdata/rain.test.csv", package = "Rainmaker")) %>%
  dplyr::mutate(date = as.POSIXct(date,
                                  format = "%m/%d/%Y %H:%M")) %>%
  dplyr::arrange(date)

# Format date into POSIXct
# Summarise rain data into hourly rain data
hourly <- rain.test %>%
  dplyr::mutate(hour = lubridate::floor_date(date, unit = "hour")) %>%
  dplyr::group_by(hour) %>%
  dplyr::summarise(rain_hourly = sum(gage01))


# Run RMevents.R on quarterly and hourly data
rain.test_result <- RMevents(rain.test, 
                             ieHr = 12,
                             rainthres = 0.1,
                             rain = "gage01",
                             time = "date")

hourly_result <- RMevents(hourly, 
                          ieHr = 12,
                          rainthresh = 0.1,
                          rain = "rain_hourly",
                          time = "hour")

# Compare results
# hourly_result[[2]]$rain - rain.test_result[[2]]$rain


