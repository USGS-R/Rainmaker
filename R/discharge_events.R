#' discharge_events
#' 
#' @description
#' Discharge event determination - calculates event start and end times based on discharge record.
#'
#' @param df dataframe that contains discharge and timestamps
#' @param qthresh numeric, Discharge threshold value, over which an event is considered to be occuring.
#' @param ieHr numeric, Interevent period in hours, defaults to 6. 
#' The amount of time between discharge measurements above threshold required to be considered a new event.
#' @param discharge string, Column name where discharge values are stored. Defaults to "Value".
#' @param time string, column name where dates are stored as POSIXct values, defaults to "pdate"
#' @return dataframe of all discharge events based on ieHr and qthresh criteria. Includes start and end times for each event. 
#' @import dplyr
#' @importFrom rlang sym
#' @export
discharge_events <- function(df,ieHr=6,qthresh,discharge="Value",time="pdate"){
  
  if(!time %in% names(df)){
    stop("Supplied 'time' column name not in df")
  }
  
  if(!discharge %in% names(df)){
    stop("Supplied 'discharge' column name not in df")
  }
  
  if(all(is.na(df[[time]]))){
    stop("All time values are NA")
  }
  
  ieMin <- ieHr * 60 # compute interevent period in minutes
  #dateOrigin <- as.POSIXct('1884-01-01 00:00',origin = '1884-01-01 00:00')
  
  # store original input data to use later on
  df_input <- df
  
  # make sure data are arranged in order by time
  df <- df[order(df[,time]), ]
  
  # be sure to remove any repeated rows
  df <- unique(df)
  
  # filter out timestamps with zeros as value
  df <- df[df[discharge] != 0,]
  
  # filter out timestamps below threshold
  df <- df[df[discharge] > qthresh,]
  
  # define events, first row
  df["event"] <- NA
  df[1, "event"] <- 1
  
  dif_time <- diff(df[[time]])
  #timeInterval <- min(dif_time)
  df$dif_time[2:nrow(df)] <- dif_time
  
  
  # loop that assigns each row to an event number based on dif_time
  for (i in 2:nrow(df)){
    if (dif_time[[i-1]] >= ieMin) {
      df$event[i] <- df$event[i-1] + 1
    } else {
      df$event[i] <- df$event[i-1]
    }
  }
  

  #discharge.events <- aggregate(x = df[[discharge]], by = list(df$event), sum) #find sum of discharge in each event
  
  # create new variable so can use in dplyr function
  time_quo <- sym(time)
  start.dates <- group_by(df, event) %>%
    summarize(start_date = min(!!time_quo)) #find minimum date/time for each event
  
  # find "punch" from original data that precedes start time
  
  start.indices <- which(df_input[[time]] %in% start.dates$start_date)
  start.indices <- start.indices -1
  
  start.dates.adjusted <- filter(df_input, row_number() %in% start.indices) %>%
    select(!!time_quo)
  

  # find "punch" from original data that is after last event timestamp
  end.dates <- group_by(df, event) %>%
    summarize(end_date = max(!!time_quo))
  
  end.indices <- which(df_input[[time]] %in% end.dates$end_date)
  end.indices <- end.indices +1
  
  end.dates.adjusted <- filter(df_input, row_number() %in% end.indices) %>%
    select(!!time_quo)
  
  out <- data.frame(stormnum = start.dates$event,
                       StartDate = start.dates.adjusted[[time]],
                       EndDate = end.dates.adjusted[[time]])
  return(out)
}
  
