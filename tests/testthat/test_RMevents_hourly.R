
# Test script to compare x-minute interval rain data to hourly interval rain data
# Tested on ./data/05408480.csv and ./data/rain.test.csv

# Import dependencies
library(dplyr)
library(lubridate)

# Source RMevents.R
source(here::here("R", "RMevents.R"))

################################################
########### Testing for 05408480.csv ###########
################################################

# Import test script
# This data contains rain totals every 15 minutes
quarterly <- read.csv("C:/R/Rainmaker/data/05408480.csv") %>%
  dplyr::mutate(dateTime = as.POSIXct(dateTime, 
                                      format = "%Y-%m-%d %H:%M:%S",
                                      tz = tz_cd[1]))

# Format dateTime into POSIXct
# Summarise quarterly rain data into hourly rain data
hourly <- quarterly %>%
  dplyr::mutate(hour = lubridate::floor_date(dateTime, unit = "hour")) %>%
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

# Compare results
hourly_result[[2]]$rain-quarterly_result[[2]]$rain


################################################
########## Testing for rain.test.csv ##########
################################################

# Clear environment and run test on different dataset
rm(list = ls())

# Source RMevents.R
source(here::here("R", "RMevents.R"))

# Import test script
rain.test <- read.csv("C:/R/Rainmaker/data/rain.test.csv") %>%
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
hourly_result[[2]]$rain - rain.test_result[[2]]$rain


