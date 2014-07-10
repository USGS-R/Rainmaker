#'RMarf 
#'
#' @description
#' This function computes antecedent rainfall for Rainmaker event files or any file with 
#' a list of specified dates.
#'
#' Input files must have a as.POSIXctformatted date/time column.
#' This format can be achieved by using the RMprep function
#' The name of the rainfall column can also be changed as desired using the RMprep funtion
#' 
#' Subset the data by antecedent time period (can also assign to a df if you like)
#' then define ARF values for the subset. Do this for all date periods
#' in the sample file.
#' @param df dataframe Unit values rain file
#' @param date string Date column name in df as POSIX
#' @param rain string Column in df with instantaneous rain values
#' @param df.events dataframe with dates/times for events
#' @param sdate string Name of start date column in df.events rain file as POSIX
#' @param days vector of times in days for summing antecedent rainfall 
#' @param varnameout string prefix for resulting antecedent rainfall variable names
#' @return df.events dataframe
#' @export 
#' @examples
#' RDB <- CedarRRain
#' RDB2 <- RMprep(RDB,prep.type=1,date.type=1,dates.in="CST.Time",tz="CST6CDT")
#' RDB3 <- subset(RDB2,upload.ph3_site_basin_cedar_creek.Id.0....Geographical.Mean.kg.m.2.>-1)
#' event.list <- RMevents(df=RDB3,ieHr=6,rainthresh=0.2,rain="upload.ph3_site_basin_cedar_creek.Id.0....Geographical.Mean.kg.m.2.")
#' events.0.2 <- event.list$storms2
#' intensities <- RMIntense(RDB3,date="pdate",rain="upload.ph3_site_basin_cedar_creek.Id.0....Geographical.Mean.kg.m.2.",events.0.2,sdate="StartDate",edate="EndDate",depth="rain",xmin=c(5,15,30))
#' ARFrain <- RMarf(df=RDB3,date="pdate",rain="upload.ph3_site_basin_cedar_creek.Id.0....Geographical.Mean.kg.m.2.",df.events=intensities,sdate="StartDate",days=c(1,3,5),varnameout="ARF")
RMarf <- function(df, date="date", rain="rain",
                  df.events, sdate="StartDate", 
                  days=c(0.5,1,2,3,5,10,15,20),
                  varnameout="ARF") {
  
  arfdate <- "arfdate"
  
  #initialize varsum vector
  maxrows <- nrow(df.events)           #determine how many rows are in the dates data frame
  varsum=vector(length=maxrows)
  
  # compute the antecedent rain (ARF) for all identified durations
  for(j in 1:length(days)) {      
    df.events$arfdate <- df.events[,sdate] - days[j]*24*60*60
    
    # Compute ARF for all dates in the sample dates file
    for (i in 1:maxrows){
      subdata <- df[which(df[,date]>= df.events[i,arfdate]
                          & df[,date] < df.events[i,sdate]),]
      
      varsum[i] <- sum(subdata[,rain])
    }
    
    sumname <- paste(varnameout,days[j],sep="")
    
    df.events[,sumname] <- varsum
  }
  df.events <- df.events[,-which(names(df.events)==arfdate)]
  return(df.events)
}