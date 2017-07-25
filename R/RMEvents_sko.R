#' Rainfall event determination
#' 
#' @description
#' Compute rainfall event variables based on time series of rain data with only one rain
#' gage or one mean radar rain column.
#'
#' @param df dataframe with rainfall
#' @param ieHr numeric Interevent period in hours, defaults to 6, 
#' @param rainthresh numeric Minimum event depth in units of the rain column, default is given as 5.1 assuming millimeters (0.2")
#' @param rain string Column name of rainfall unit values, defaults to "rain"
#' @param time string column with as.POSIXctdate, defaults to "pdate"
#' @return list of all rain events that surpass rainthresh (storms2) and all rain events (storms). Also returns all
#' a data frame of all rain observations > 0 with the associated date/time and assigned event number (tipsbystorm) and 
#' the minimum time difference between observations (timeInterval)
#' @export
RMevents_sko <- function(df,ieHr=6,rainthresh=5.1,rain="rain",time="pdate"){
  
  if(!time %in% names(df)){
    stop("Supplied 'time' column name not in df")
  }
  
  if(all(is.na(df[[time]]))){
    stop("All time values are NA")
  }
  
  ieMin <- ieHr * 60 # compute interevent period in minutes
  dateOrigin <- as.POSIXct('1884-01-01 00:00',origin = '1884-01-01 00:00')
  
  
  df <- df[df[rain] != 0,]
  df <- df[df[rain] > 0.00001,]
  df["event"] <- NA
  df[1, "event"] <- 1
  
  dif_time <- diff(df[[time]])
  timeInterval <- min(dif_time)
  df$dif_time[2:nrow(df)] <- dif_time
  
  
  # loop that assigns each row to an event number based on dif_time
  for (i in 2:nrow(df)){
    if (dif_time[[i-1]] >= ieMin) {
      df$event[i] <- df$event[i-1] + 1
    } else {
      df$event[i] <- df$event[i-1]
    }
  }
  

  rain.events <- aggregate(x = df[[rain]], by = list(df$event), sum) #find sum of rain in each event
  start.dates <- aggregate(x = df[[time]], by = list(df$event), min)[,2] #find minimum date for each event
  start.dates <- start.dates - timeInterval
  end.dates <- aggregate(x = df[[time]], by = list(df$event), max)[,2]
  
  out <- data.frame(stormnum = rain.events[,1],
                       StartDate = start.dates,
                       EndDate = end.dates,
                       rain = rain.events[,2])
  out2 <- subset(out, rain >= rainthresh, row.names = FALSE)
  return(list(storms2 = out2, storms = out, tipsbystorm = df[,c('rain', 'pdate', 'dif_time', 'event')], timeInterval = timeInterval))
}
  
