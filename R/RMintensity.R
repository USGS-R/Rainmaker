#' RMintensity
#'
#' Function to compute maximum x-minute rainfall intensities in units of depth/hr
#'
#' @param df dataframe
#' @param date string Date column name in df as POSIX
#' @param rain string Column name in df with instantaneous rain values
#' @param df.events dateframe with start and end dates/times for events
#' @param sdate string Start date column in df.events rain file as POSIX
#' @param edate string End date column in df.events rain file as POSIX
#' @param depth string rain depth in event file, defaults to "depth",  
#' @param xmin vector vector of values representing X-minute max rainfall requested
#' @return df.events dataframe, X-hour maximum rainfall intensities
#' @export
#' @examples
#' RDB <- CedarRRain
#' RDB2 <- RMprep(RDB,prep.type=1,date.type=1,dates.in="CST.Time",tz="CST6CDT")
#' RDB3 <- subset(RDB2,upload.ph3_site_basin_cedar_creek.Id.0....Geographical.Mean.kg.m.2.>-1)
#' event.list <- RMevents(df=RDB3,ieHr=6,rainthresh=0.2,rain="upload.ph3_site_basin_cedar_creek.Id.0....Geographical.Mean.kg.m.2.")
#' events.0.2 <- event.list$storms2
#' intensities <- RMIntense(RDB3,date="pdate",rain="upload.ph3_site_basin_cedar_creek.Id.0....Geographical.Mean.kg.m.2.",events.0.2,sdate="StartDate",edate="EndDate",depth="rain",xmin=c(5,15,30))
RMintensity <- function(df,date="r.date",rain = "rain",
                      df.events,sdate="StartDate",edate="EndDate",
                      depth="depth",
                      xmin=c(60,180,360)) {
  
  # Compute overall event intensity
  df.events$duration <- (as.numeric(difftime(df.events[,edate],df.events[,sdate],units="hours")))
  df.events$Ievent <- df.events[,depth]/df.events$duration
  
  # Determine x-minute intensities for each of the intensities specified  
  
  for (i in 1:length(xmin)){
    x <- xmin[i]*60
    intensity.var <- paste("I",xmin[i],sep="")
    df.events[,intensity.var] <- NA
    
    #   Isolate individual events and Compute max x-min intensity for each event 
    #   period: compute sum rain and divide by duration. Report x-min intensity 
    #   in units/hr
    
    for (j in 1:nrow(df.events)) {
      subdf <- df[which(df[,date] >= df.events[j,sdate] & df[,date] <= df.events[j,edate]),]
      # Initialize intensity vector
      intensity <- numeric(length=nrow(subdf))
      
      for (k in 1:nrow(subdf)){
        enddate <- subdf[k,date]+x
        bdate <- subdf[k,date]
        
        subdf2 <- subdf[which(subdf[,date] >= bdate & subdf[,date] < enddate),]
        intensity[k] <- sum(subdf2[,rain], na.rm = T)/(x/60/60)
        
        #      k;bdate;enddate;intensity[k];max(subdf2$rain)
      }
      df.events[j,intensity.var] <- max(intensity,na.rm=TRUE)
    }
  }
  
  return(df.events)
}
