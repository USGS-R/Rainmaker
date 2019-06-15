########################################################################
library(dplyr)
library(Rainmaker)
library(dataRetrieval)
library(USGSHydroTools)
library(lubridate)
###########################################################################################################################################################################

# set dates to time zone
.origin <- as.POSIXct(ifelse(Sys.info()[['sysname']] == "Windows", "1899-12-30", "1904-01-01"))
lubridate::tz(.origin) <- site_tz

################### Get rain data ###################
# get data from NWIS if file is not provided
if (is.na(rain_file)&is.na(rain_AQ_file)) {
  
  # set readNWISuv parameters
  parameterCd <- "00045"  # Precipitation
  startDate <- as.Date(start_date) - 15 # put a week buffer on the study start date in case storm started prior to first sample date
  endDate <- as.Date(end_date)
  
  # get NWIS data
  message('Pulling precip data from NWIS.')
  start_time <- Sys.time()
  precip_raw <- readNWISuv(rain_site, parameterCd, startDate = startDate, endDate= endDate, tz = site_tz)
  end_time <- Sys.time()
  if (nrow(precip_raw) > 0){
    message(paste0(nrow(precip_raw)), ' rows of data pulled from NWIS in ', round(difftime(end_time , start_time, units = 'secs'), 0), ' seconds.')
  } else {
    stop('No precip data pulled from NWIS. Please check inputs to verify correct site number and start and end dates. To debug, see code in "data_processing/2_calc_rain_variables.R"')
  }
  
  # rename columns - step 1
  precip_raw <- renameNWISColumns(precip_raw)
  
  # rename columns - step 2
  names(precip_raw)[grep('precip_inst$', names(precip_raw), ignore.case = TRUE)] <- 'rain'
  names(precip_raw)[grep('dateTime', names(precip_raw), ignore.case = TRUE)] <- 'pdate'
  
  # print warning if dates of rain do not span dates of study
  if (min(as.Date(precip_raw$pdate)) > start_date) {
    warning('Data pulled from NWIS does not span the entire study period.')
  }
  
  if (max(as.Date(precip_raw$pdate)) < end_date) {
    warning('Data pulled from NWIS does not span the entire study period.')
  }
  ###Optional Output Raw Data File
  if(output_raw=='yes'){
    write.table(x=precip_raw, file = paste(site,"_RawData.csv",sep=""),sep = ",", row.names = FALSE)
  }
} else {
  
  # read in rain file
  if(is.na(rain_AQ_file)){
    precip_raw <- read.csv(file = rain_file, stringsAsFactors = FALSE, strip.white = TRUE)
    precip_raw[,'pdate'] <- as.POSIXct(precip_raw[,'pdate'], origin = .origin, tz = site_tz, format = datetime_format)
  } else {
    precip_raw <- read.csv(file = rain_AQ_file, skip = 14,header = TRUE)
    colnames(precip_raw)[2] <-'pdate'
    colnames(precip_raw)[3] <-'rain'
    precip_raw[,'pdate'] <- as.POSIXct(precip_raw[,'pdate'], origin = .origin, tz = site_tz)
  }
}

################### Process rain data ###################
# summarize precip data using Rainmaker
# run.rainmaker is a wrapper function for multiple Rainmaker steps/functions

if(is.na(ieHr)){ieHr=2}
if(is.na(rainthresh)){rainthresh=0.008}
if(is.na(antecedentDays)){antecedentDays=c(1,2,7,14)}

precip.dat <- run.rainmaker(precip_raw = precip_raw, ieHr = ieHr, rainthresh = rainthresh, xmin = c(5,10,15,30,60), antecedentDays = antecedentDays)

precip.dat <- rename(precip.dat, 'rain_startdate' = 'StartDate', 'rain_enddate' = 'EndDate')

precip_filename <- file.path(paste0(site, '_stormSummary.csv'))
write.csv(precip.dat, precip_filename, row.names = FALSE)

# check if the precip.dat data frame has values
test <- precip.dat[!is.na(precip.dat$rain), ]

if (nrow(test)>0) {
  message(paste("The precipitation data has been processed. Please check", precip_filename, "to ensure correct processing."))
} else {
  stop("Something went wrong with processing the precipitation data. To debug, see code in 'scripts/data_processing/2_calc_rain_variables.R'.")
}

#########################################################
