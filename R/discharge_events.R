#' Calculate events from discharge record
#' 
#' @description
#' Discharge event determination - calculates event start and end times based on discharge record.
#'
#' @details When a discharge measurement is above \code{qthresh}, the algorthim decides whether it belongs to a 
#' "new" event by looking backwards at the last measurement above qthresh. If the difference in time 
#' is greater than \code{ieHr}, then a new event begins. The start and end times of each event, 
#' however, are adjusted to the previous or next timestamp, respectively, to account for the "tails" of
#' the event. The consequence of adjusting the start and end times, however, is that depending on the frequency 
#' of discharge observations, two observations above \code{qthresh} that are greater than ieHr apart (and therefore in seperate events)
#' can have start and end times that are closer together than \code{ieHr}. If \code{ieHr_check = TRUE} (default), the algorithm makes a
#' final pass through the events dataset and checks for any events that are closer in time than ieHr. If so, 
#' it will combine those events. If \code{ieHr_check = FALSE}, the algorithm will leave the events as-is, and depending 
#' on discharge measurement frequency and \code{ieHr}, you may see events that are closer together than ieHr. In most use-cases, 
#' \code{ieHr_check = TRUE} is appropriate. However, if discharge measurements are infrequent, and the adjustment to the start 
#' and end times are significant, the user may not want the secondary filter on the events. 
#' 
#' @param df dataframe that contains discharge and timestamps
#' @param qthresh numeric, Discharge threshold value, over which an event is considered to be occuring.
#' @param ieHr numeric, Interevent period in hours, defaults to 6. 
#' The amount of time between discharge measurements above threshold required to be considered a new event.
#' @param discharge string, Column name where discharge values are stored. Defaults to "Value".
#' @param time string, column name where dates are stored as POSIXct values, defaults to "pdate".
#' @param ieHr_check logical, whether to check the calculated events data frame for events that are closer
#' together than ieHr. This can happen due to the start and end time corrections for the "tails" of the event. 
#' If TRUE, events closer than ieHr apart will be combined. If FALSE, events will be left as-is. 
#' See the details section below for more information. 
#' @return dataframe of all discharge events based on ieHr and qthresh criteria. Includes start and end times for each event. 
#' @import dplyr
#' @export
discharge_events <- function(df, ieHr=6, qthresh, discharge="Value", time="pdate", ieHr_check = TRUE){
  
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
  

  #discharge.events <- aggregate(x = df[[discharge]], by = list(df$event), sum) 
  #find sum of discharge in each event
  
  # create new variable so can use in dplyr function
  time_quo <- rlang::sym(time)
  #find minimum date/time for each event (change to aggregate?)
  start.dates <- dplyr::group_by(df, event) 
  start.dates <- dplyr::summarize(start.dates,
                                  start_date = min(!!time_quo)) 
  
  # find "punch" from original data that precedes start time
  
  start.indices <- which(df_input[[time]] %in% start.dates$start_date)
  start.indices <- start.indices -1

  # find "punch" from original data that is after last event timestamp
  end.dates <- dplyr::group_by(df, event)
  end.dates <- dplyr::summarize(end.dates,
                                end_date = max(!!time_quo, na.rm = TRUE))
  
  end.indices <- which(df_input[[time]] %in% end.dates$end_date)
  end.indices <- end.indices +1
  
  # throw warning if either start or end index are outside of data frame
  if (any(end.indices > nrow(df_input))) {
    warning("Discharge record ends mid-storm. Cannot calculate end of 
         storm as end + 1 time step. Last storm end time not calculated.")
  }
  
  if (any(start.indices < 1)) {
    warning("Discharge record starts mid-storm. Cannot calculate start of 
         storm as start - 1 time step. First storm start time not calculated.")
  }
  
  end.indices <- ifelse(end.indices > nrow(df_input), NA, end.indices)
  start.indices <- ifelse(start.indices < 1, NA, start.indices)
  
  start.dates.adjusted <- df_input[start.indices, ]
  start.dates.adjusted <- dplyr::select(start.dates.adjusted, !!time_quo)
 
  end.dates.adjusted <- df_input[end.indices, ]
  end.dates.adjusted <- dplyr::select(end.dates.adjusted, !!time_quo)
  
  
  out <- data.frame(stormnum = start.dates$event,
                       StartDate = start.dates.adjusted[[time]],
                       EndDate = end.dates.adjusted[[time]])
  
  if (isTRUE(ieHr_check)) {
    event_diff <- difftime(out$StartDate[2:nrow(out)], out$EndDate[1:(nrow(out)-1)], units = 'mins')
    out$event_check <- NA
    out$event_check[1] <- 1
    
    # loop that assigns new event column
    for (i in 2:nrow(out)){
      if (event_diff[i-1] >= ieMin) {
        out$event_check[i] <- out$event_check[i-1] + 1
      } else {
        out$event_check[i] <- out$event_check[i-1]
      }
    }
    
    out <- dplyr::group_by(out, event_check) %>%
      dplyr::summarize(StartDate = min(StartDate, na.rm = TRUE), 
                EndDate = max(EndDate, na.rm = TRUE)) %>%
      dplyr::select(-event_check)
    
    out$stormnum <- 1:nrow(out)
    
    out <- dplyr::select(out, stormnum, StartDate, EndDate)
  }
  out <- as.data.frame(out)
  return(out)
}
  
