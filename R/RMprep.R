#' RMprep function
#'
#' This function is used to prepare data files for Rainmaker functions.
#' Dates are transformed to as.POSIXctdates using the as.POSIXct function
#' Multiple common date formats are included as options for tranformation
#' The original date column is transformed to a character variable.
#' Column header names are changed to desired names
#'
#' @param df dataframe
#' @param prep.type numeric 1=date to as.POSIXct 2=name change, 3=both
#' @param date.type numeric 1=mm/dd/YYYY hh:mm, 2=YYYY-mm-ddTHH:MM, 3=RDB_example1: 2 columns, Date and Time
#' Date=m/d/Y; time=h:mm, 4=RDB_example2: 4 columns, Year, Month, Day and Minute Date=m/d/Y; time=h:mm,
#' 5=Lake level from Great Lakes "Tides and Currents", Date=YYYYMMDD; Time = H:MM
#' @param dates.in string Vector of column names for date/time definition. Defaults are as follows for different date.type options
#' date.type=1: One column name -> "GMT.Time", 
#' date.type=2: One column name -> "GMT.Time", 
#' date.type=3: two column names -> c("DATE","TIME"), 
#' date.type=4: four column names > c("YEAR","MONTH","DAY","MINUTE"), 
#' date.type=5: two column names -> c("Date","Time"), 
#' If no value is given, the defaults given above are used. Enter value as c("name1","name2",...)
#' @param dates.out string
#' @param cnames.in string
#' @param cnames.new string
#' @param tz string time zone, CST6CDT for central time. For other times use values in the TZ* column here:
#' \url{http://en.wikipedia.org/wiki/List_of_zoneinfo_time_zones}
#' @return df dataframe
#' @export
#' @examples
#' RDB <- cedarq
#' RDB2 <- RMprep(RDB,prep.type=1,date.type=3,tz="CST6CDT")
#' RDB <- CedarRRain
#' RDB2 <- RMprep(RDB,prep.type=1,date.type=1,dates.in="CST.Time",tz="CST6CDT")
RMprep <- function(df,
                   prep.type=1,          #1=date to as.POSIXct 2=name change, 3=both
                   date.type=1,          #1=mm/dd/YYYY hh:mm, 
                   #2=YYYY-mm-ddTHH:MM
                   #3=RDB_example1: 2 columns, Date and Time
                   # Date=m/d/Y; time=h:mm
                   #4=RDB_example2: 4 columns, Year, Month, Day and Minute
                   # Date=m/d/Y; time=h:mm    
                   #5=Lake level from Great Lakes "Tides and Currents"
                   # Date=YYYYMMDD; Time = H:MM
                   dates.in="default",   #Vector of column names for date/time definition
                   # defaults are as follows for different date.type options
                   # date.type=1: One column name -> "GMT.Time"
                   # date.type=2: One column name _> "GMT.Time"
                   # date.type=3: two column names -> c("DATE","TIME") 
                   # date.type=4: four column names > c("YEAR","MONTH","DAY","MINUTE")
                   # date.type=5: two column names -> c("Date","Time")
                   # if no value is given, the defaults given above
                   # are used. 
                   #Enter value as c("name1","name2",...)
                   dates.out="pdate",
                   cnames.in="",
                   cnames.new="rain",
                   tz="")                #time zone. CST6CDT for central time, for other times use values in the TZ* column here: http://en.wikipedia.org/wiki/List_of_zoneinfo_time_zones
{
  
  date.options <- list (c("GMT.Time"),
                        c("GMT.Time"),
                        c("DATE","TIME"),
                        c("YEAR","MONTH","DAY","MINUTE"),
                        c("Date","Time"))
  
  if(dates.in[1]=="default") dates.in <- date.options[[date.type]]
  #  for (i in 1:length(dates.in)) df[,dates.in[i]] <- as.character(df[,dates.in[i]])
  
  Date.style <- c("%m/%d/%Y %H:%M","%Y-%m-%d %H:%M",
                  "%m/%d/%Y %H:%M","%m/%d/%Y %H:%M",
                  "%Y%m%d %H:%M")
  
  # Convert to as.POSIXctdate/time
  if(prep.type==1 | prep.type==3){
    if(date.type==1) dates <- df[,dates.in]
    if(date.type==2) {
      for (i in 1:length(dates.in)) df[,dates.in[i]] <- as.character(df[,dates.in[i]])
      dates <- sub("T"," ",df[,dates.in])
    }
    if(date.type==3 | date.type==5) dates <- paste(df[,dates.in[1]],df[,dates.in[2]])
    if(date.type==4) {
      hour <- trunc(df[,dates.in[4]]/60)
      minute <- df[,dates.in[4]]-hour*60
      dates <- paste(df[,dates.in[2]],"/",df[,dates.in[3]],"/",
                     df[,dates.in[1]]," ",
                     hour,":",minute,sep="")
    }
    if(tz=="") pdate <-  as.POSIXct(dates,format=Date.style[date.type])
    else pdate <-  as.POSIXct(dates,format=Date.style[date.type],tz=tz)
    df$pdate <- pdate
    names(df)[ncol(df)] <- dates.out
  }
  
  # Change column headers as specified
  if(prep.type==2 | prep.type==3){
    current.names <- names(df)
    name.locs <- which(current.names==cnames.in)
    names(df)[name.locs] <- cnames.new
  }
  return(df)
}