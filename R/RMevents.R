#' Rainfall event determination
#' 
#' @description
#' Compute rainfall event variables based on time series of rain data with only one rain
#' gage or one mean radar rain column.
#'
#' @param df dataframe with rainfall
#' @param ieHr numeric Interevent period in hours, defaults to 6, 
#' @param rainthresh numeric Minimum event depth in units of the rain column, default is given as 5.1 assuming millimeters (0.2")
#' @param timeInterval numeric Minimum time interval between measurements in seconds, default is 60 seconds
#' @param rain string Column name of rainfall unit values, defaults to "rain"
#' @param time string column with as.POSIXctdate, defaults to "pdate"
#' @return list of storms and storms2
#' @export
#' @examples
#' RDB <- CedarRRain
#' RDB2 <- RMprep(RDB,
#'                prep.type = 1,
#'                date.type = 1,
#'                dates.in = "CST.Time",
#'                tz = "CST6CDT")
#' event.list <- RMevents(df = RDB2,
#'                        ieHr = 6,
#'                        rainthresh = 0.2,
#'                        timeInterval = 60,
#'                        rain = "upload.ph3_site_basin_cedar_creek.Id.0....Geographical.Mean.kg.m.2.")
#' events.0.2 <- event.list$storms2
RMevents <- function(df,ieHr=6,rainthresh=5.1,timeInterval=1,rain="rain",time="pdate"){
  
  ieSec <- ieHr * 3600 # compute interevent period in seconds to use with POSIX
  dateOrigin <- as.POSIXct('1884-01-01 00:00',origin = '1884-01-01 00:00')
  
  df <- df[df[rain] != 0,]
  df <- df[df[rain] > 0.00001,]
  stormnum <- 0
  
  dif_time <- diff(df[[time]])
  which(dif_time > ieSec)
  for (i in 2:nrow(df)) {
  #   
  # }
     # Initiate variables
     StartRow <- 1
     EndRow <- 1
     StartDryRow <- 1
     dry <- FALSE
     stormnum <- 0
     continue.dry <- FALSE
     sumrain <- 0
    
   #Loop through rain data and define event periods
    for (i in 2:nrow(df)) {
  
     # During dry period, look for start of event
      if(dry) {
  
        # Event initiation
        if(df[i,rain]>0 ) {
          dry=FALSE
          StartRow <- i-1
        }
      }
      #Define event period
      if(!dry) {
        
        # Search for end of event period
        if(df[i,rain]==0) {
          if(!continue.dry){
            continue.dry <- TRUE
            dryduration <- difftime(df[[time]][i],
                                    df[[time]][StartDryRow],
                                    units="secs")
          }
          
          # Continue checking for end of event (dry duration >= interevent period)
          if(continue.dry){                   
            dryduration <- difftime(df[[time]][i],
                                    df[[time]][StartDryRow],
                                    units="secs")
            if(dryduration >= ieSec) {
              EndRow <- StartDryRow
              stormnum <- stormnum + 1
              
              # After event period ends, save start and end dates/times and rain depth
              # Adjust begin time to be one timeInterval before the first rainfall
              
              df[StartRow,time] <- df[(StartRow+1),time] - 
                as.numeric(difftime(df[[time]][(StartRow+1)], dateOrigin,units = 'sec')) %% timeInterval
              current.storm <- data.frame(stormnum=stormnum,
                                          StartDate=df[StartRow,time],
                                          EndDate=df[EndRow,time],
                                          rain=sumrain)
              dry <- TRUE
              if(stormnum>1) storms <- rbind(storms, current.storm)
              else storms <- current.storm        
              sumrain <- 0
              
            }
          }
        }
        # add current rain to event depth
        if (df[i,rain]!=0) {
          sumrain <- sumrain + df[i,rain]
          EndRow <- i
          StartDryRow <- EndRow
          continue.dry <- FALSE
        }
      }
    }
  }
  # Subset based on defined event rain depth threshold        
  storms2 <- subset(storms,rain>=rainthresh,row.names=FALSE)
  
  return(list(storms2=storms2,storms=storms))
}