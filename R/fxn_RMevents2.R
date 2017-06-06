#Compute rainfall event variables based on time series of rain data with only one rain
#gage or one mean radar rain column.
#
# Usage: RMevents  (df,             # Data frame with rainfall
#                  ieHr=6,          # Interevent period in hours
#                  rainthresh=5.1   # minimum event depth in units of the rain column
#                                   # default is given as 5.1 assuming millimeters (0.2")
#                  rain="rain",     # column of rainfall unit values
#                  time="pdate"  # column with POSIX date
#                  GMToffset=6      # hour offset from GMT for variable time
#                  ) 

 
###########

RMevents2 <- function(df,ieHr=6,rainthresh=5.1,rain="rain",time="pdate",GMToffset=6){

ieSec <- ieHr * 3600 # compute interevent period in seconds to use with POSIX

# Initiate variables
StartRow <- 1
EndRow <- 1
StartDryRow <- 1
dry <- TRUE
stormnum <- 0
continue.dry <- TRUE
sumrain <- 0

# Loop through rain data and define event periods
for (i in 2:nrow(df)) {

  # During dry period, look for start of event
  if(dry) {
    
    # Event initiation
    if(df[i,rain]>0) {  
      dry=FALSE
      StartRow <- i
    }
  }
    # Define event period
    if(!dry) {
      
      # Search for end of event period
      if(df[i,rain]==0 | difftime(df[[time]][i],df[[time]][StartDryRow],units="secs")>ieSec) {
        if(!continue.dry){
          continue.dry <- TRUE
          dryduration <- difftime(df[[time]][i],
                                  df[[time]][StartDryRow],
                                  units="secs")
        }
        
        # Continue checking for end of event (dry duration >= interevent period)
        if(continue.dry){                   
        dryduration <- difftime(df[[time]][i],df[[time]][StartDryRow],units="secs")
        if(dryduration >= ieSec) {
          EndRow <- StartDryRow
          stormnum <- stormnum + 1
          
          # After event period ends, save start and end dates/times and rain depth
          current.storm <- data.frame(stormnum=stormnum,
                                      StartDate=df[StartRow,time],
                                      EndDate=df[EndRow,time],
                                      rain=sumrain)
          dry <- TRUE
          if(stormnum>1) storms <- rbind(storms,current.storm)
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

# Subset based on defined event rain depth threshold        
storms2 <- subset(storms,rain>=rainthresh,row.names=FALSE)

return(list(storms2,storms))
}