#' RMevents_sample
#' 
#' @description
#' Compute rainfall event variables based on time series of rain data with only 
#' one rain gage or one mean radar rain column. The function does not calculate events based on
#' the rain data itself (such as in RMevents), but rather calculates event variables based on an input
#' of sample/event start and end times. 
#' 
#' @param df dataframe with rainfall
#' @param ieHr numeric Interevent period in hours, defaults to 6, 
#' @param rain string Column name of rainfall unit values, defaults to "rain"
#' @param time string column with as.POSIXctdate, defaults to "pdate"
#' @param dfsamples dataframe with the beginning and ending dates and times
#' of sampling periods in POSIXct format
#' @param bdate character column name in dfsamples for the beginning of the sampling period
#' @param edate character column name in dfsamples for the ending of the sampling period
#' @return list of storms and storms2
#' @export
#' @examples
#' RDB <- CedarRRain
#' cedarSamples <- cedarSamples
#' names(RDB)[2] <- "UVRain"
#' RDB2 <- RMprep(RDB,prep.type=1,date.type=1,
#'                dates.in="CST.Time",tz="CST6CDT")
#' eventListSamples <- RMeventsSamples(df=RDB2,ieHr=6,
#'                                     rain="UVRain",
#'                                     time="pdate",
#'                                     dfsamples=cedarSamples,
#'                                     bdate="pSstart",edate="pSend")
RMevents_sample <- function(df,
                            ieHr=6,
                            rain="rain",
                            time="pdate",
                            dfsamples,
                            bdate="bpdate",
                            edate="epdate"){
  df.orig <- df
  df <- rbind(df[1,],df[df[,rain]>0,])
  timediff <- difftime(df[2:(nrow(df)),time],df[1:(nrow(df)-1),time],units="secs")
  timediff_min <- difftime(df[2:(nrow(df)),time],df[1:(nrow(df)-1),time],units="mins")
  df$timediff <- c(NA,timediff)
  df$timediff_min <- c(NA, timediff_min)
  #  dfsamples$Braindate <- dfsamples$bpdate
  #  dfsamples$Eraindate <- dfsamples$epdate
  
  ieSec <- ieHr * 3600 # compute interevent period in seconds to use with POSIX
  rainDepth <- numeric()
  startRainDates <- numeric()
  endRainDates <- numeric()
  tipsbystorm <- data.frame()
 
  for (i in 1:nrow(dfsamples)){
    beginRow <- max(which(df[,time]<dfsamples[i,bdate])+1)
    # this fails if you have an event that is not captured in your rain data
    if(i ==1 & is.infinite(beginRow)) {
      startRainDates <- NA
      endRainDates <- NA
      rainDepth <- NA
      next
    }
    
    if (i > 1 & is.infinite(beginRow)) {
      startRainDates <- c(startRainDates, NA)
      endRainDates <- c(endRainDates, NA)
      rainDepth <- c(rainDepth, NA)
      next
    }
    
    endRow <- max(which(df[,time]<dfsamples[i,edate]))
    subdf <- df[c(1:beginRow),]
    startRainRow <- max(which(subdf$timediff>ieSec))
    if (startRainRow == dim(subdf)[1]) {
      BD <- dfsamples[i,bdate]
    } else {
      BD <- subdf[startRainRow,time]
    }
    subdf2 <- df[c(startRainRow:endRow),]
    
    if (sum(subdf2[,rain]>0)>0) {
      endRainRow <- max(which(subdf2[,rain]>0))
      ED <- subdf2[endRainRow,time]
      if(ED<BD) ED <- BD + 60
      
    } else {
      endRainRow <- startRainRow
      ED <- BD + 60
    }
    eventRows <- which(df.orig[,time]>=BD & df.orig[,time]<=ED)
    eventRows_tips <- which(df[,time]>=BD & df[,time]<=ED)
    
    eventRain <- ifelse(length(eventRows)>0,sum(df.orig[eventRows,rain]),0)
    rainDepth <- c(rainDepth,eventRain)
    
    # get data frame of all rain from this event, add event id column
    sub_tips <- df[eventRows_tips, ]
    
    
    if(i ==1) {
      startRainDates <- BD
      endRainDates <- ED
      if (nrow(sub_tips) > 0) {
        event <- 1
      } else {
        event <- 0
      }
      if (nrow(sub_tips) > 0){
        sub_tips$event <- event
      }
      tipsbystorm <- sub_tips
    } else {
      startRainDates <- c(startRainDates,BD)
      endRainDates <- c(endRainDates,ED)
      if (nrow(sub_tips) > 0) {
        event <- event + 1
      } else {
        event <- event
      }
      if (nrow(sub_tips) > 0){
        sub_tips$event <- event
      }
      
      tipsbystorm <- rbind(tipsbystorm, sub_tips)
    }
  }
  
  dfsamples$StartDate <- startRainDates
  dfsamples$EndDate <- endRainDates
  dfsamples$rain <- rainDepth
  dfsamples$stormnum <- 1:nrow(dfsamples)
  
  dfsamples <- dfsamples[,c('stormnum', 'StartDate', 'EndDate', 'rain')]
  timeInterval <- min(timediff_min, na.rm = T)
  tipsbystorm <- tipsbystorm[,c(rain, time, 'timediff_min', 'event')]
  names(tipsbystorm)[3] <- 'dif_time'
  
  out <- list(dfsamples, dfsamples, tipsbystorm, timeInterval)
  names(out) <- c('storms2', 'storms', 'tipsbystorm', 'timeInterval')
  
  return(out)
}

##########################################################################################