#########################################################
# 1. Fill in requirements below
# 2. Run this file to run all Rainmaker processing steps
#########################################################

#site <- 'AprilShowers' # nickname for site, will be used in result filenames
site <- 'MayFlowers'

################### Date & Times ###################
# R likes dates in a very specific format (YYYY-MM-DD, coded in R as "%Y-%m-%d"), and if your dates/times
# are not in the preferred time format, R needs to know that. Set the variables below to reflect your format.
# R also needs to know  what time zone your input files are in (these should all be the same).
# To figure out the R format code for your own dates, see this brief tutorial: https://www.r-bloggers.com/date-formats-in-r/ 
# you can also see all date codes by looking executing ?strptime in your R Console, and scrolling down to "Details"
# The defaults below are common ways that Excel exports dates/times.

site_tz <- 'Etc/GMT+6' # all data should be converted (or called into R) to the same time zone. 
# Indicate here which timezone that is - e.g. central "America/Chicago" or central without DST "Etc/GMT+6"

datetime_format <- "%m/%d/%Y %H:%M" # format of all your datetime columns. For example, if your datetimes are formatted with
# "mm/dd/yyyy HH:MM" the coded format of the date in R is "%m/%d/%Y %H:%M"

#Import Rain Data
################### Basic Spreadsheet.csv ###################
# Format the raw rain data so there is one header row with headers: 'date and time' and 'rain'
# Format 'date and time' as (%m/%d/%Y %H:%M:%S)
# 'rain' has the rain values (tips)
# alternatively, if the rain data are an export from Aquarius, see 'Aquarius Rain File' below

rain_file <- NA #'data/AprilShowers@424133077495701.csv' # filename of rain file, 'NA' if using Aquarius export or pulling from NWIS. 
# Should be a csv and include ".csv" at end of file name.
# This can either be the entire filepath, or you can set the working directory in R to the folder with the rain file.

################### Aquarius Rain File ###################
# Use this if you have exported AQ precip data and have not edited the file header or columns

rain_AQ_file <- NA #'data/Precipitation.@05408480.20160501.csv' # filename of rain file, 'NA' if using basic spreadsheet or pulling from NWIS. 
# Should be a csv and include ".csv" at end of file name.
# This can either be the entire filepath, or you can set the working directory in R to the folder with the rain file.

################### NWIS Rain Data ###################

rain_site <- '441520088045001' # site number for rain gage if pulling from NWIS, 'NA' if providing data file
start_date <- as.POSIXct('2019-05-01 00:00') # YYYY-MM-DD HH:MM:SS date/time of study start, 'NA' if providing data file
end_date <- as.POSIXct('2019-05-31 23:59') # YYYY-MM-DD HH:MM:SS date/time of study end, 'NA' if providing data file
output_raw <- 'yes' # would you like to save the NWIS download to a file?

################### Exports from other sources .csv ################### 

# Exports with other date-time formating can be made ready for Rainmaker with the "RMprep" function
# See Rainmaker for details

################### Turn Rainmaker Knobs ###################

# Use NA to use defaults
# You can adjust the hours between events (ieHr), 
# the minimum amount it must rain for it to count as an event (rainthresh), 
# and how many days before the event you'd like antecedent rainfall calculated
ieHr = NA #default is 2 
rainthresh = NA #default is 0.008 
antecedentDays = c(.5,3,9) #default is c(1,2,7,14) 

################### Run Rainmaker!!! ######################################
# source functions that are needed
source('scripts/wrapper/fxn_run_rainmaker.R')

# source the master file with all site-specific vars
source('scripts/1_run_rainmaker_workflow.R', echo = F)

#########################################################