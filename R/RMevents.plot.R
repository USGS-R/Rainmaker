#' RMevents.plot
#'
#' Function to graph rainfall for a given x-day window around specified event periods
#'
#' @param df dataframe with unit value rainfall data
#' @param date string Date column in df as POSIX
#' @param rain string Column in df with instantaneous rain values
#' @param df.events dateframe with start and end dates/times for events
#' @param sdate string Start date column in df.events rain file as POSIX
#' @param edate string End date column in df.events rain file as POSIX
#' @param depth string column in df.events with event rain depth
#' @param plot.buffer numeric Used to define plotting window in days. Graphs will include
#'    data Time period preceding beginning of event for including in the graphs
#' @param site.name string
#' @export 
#' @examples
#' RDB <- CedarRRain
#' RDB2 <- RMprep(RDB, prep.type = 1, date.type = 1,
#'                dates.in = "CST.Time", tz = "CST6CDT")
#' RDB3 <- subset(RDB2,
#'           upload.ph3_site_basin_cedar_creek.Id.0....Geographical.Mean.kg.m.2. > -1)
#' event.list <- RMevents(df=RDB3,ieHr=6,rainthresh=0.2,rain="upload.ph3_site_basin_cedar_creek.Id.0....Geographical.Mean.kg.m.2.")
#' events.0.2 <- event.list$storms2
#' # pdf("events.pdf")
#' RMevents.plot(RDB3,date="pdate",rain="upload.ph3_site_basin_cedar_creek.Id.0....Geographical.Mean.kg.m.2.",df.events=events.0.2,sdate="StartDate","EndDate",depth= "rain",plot.buffer=2,site.name="Example Site")
#' # dev.off()
RMevents.plot <- function(df,date="pdate",rain = "rain",
                          df.events,sdate="StartDate",edate="EndDate",
                          depth="depth",plot.buffer=3,site.name="") {
  
  # Read file with event dates
  
#   pdf("events.pdf")
  main.title <- paste(site.name,"Precipitation Event")
  for (i in 1:(nrow(df.events))) {
    p.sdate <- as.POSIXct(df.events[i,sdate] - plot.buffer*24*3600/2)
    p.edate <- as.POSIXct(df.events[i,edate] + plot.buffer*24*3600/2)
    subdf <- subset(df, df[,date]>=p.sdate & df[,date]<=p.edate)
    rmax <- max(subdf[,rain] + 0.3)
    subrain <- subdf[,rain]
    subdate <- as.POSIXct(subdf[,date])
    plot(subrain~subdate,
         #       data=subdf,
         type="h",
         xaxt="n",
         ylab="precipitation (mm)", 
         xlab="",
         col="blue",
         lwd=1,
         yaxs="i",
         ylim=c(0,rmax),
         xlim=as.POSIXct(range(subdf[,date])),
         main = main.title)
    
    r <- as.POSIXct(trunc(range(subdf[,date]), "days"))
    r[2] <- r[2]+24*3600
    rhour <- seq(r[1], r[2], by=24*3600/4)
    rday <- seq(r[1], r[2], by="days")
    graphics::axis.POSIXct(1,subdf[,date],at=rhour,format=" ",tcl=-0.2)
    graphics::axis.POSIXct(1,subdf[,date],at=rday,format=" ",tcl=-0.5)
    graphics::axis.POSIXct(3,subdf[,date],at=rhour,format=" ",tcl=0.2)
    graphics::axis.POSIXct(3,subdf[,date],at=rday,format=" ",tcl=0.5)
    graphics::axis.POSIXct(1,subdf[,date],format = "%m/%d/%y")
    graphics::arrows(as.POSIXct(df.events[i,sdate]),(rmax-0.15),
           as.POSIXct(df.events[i,edate]),(rmax-0.15),
           length=0.07,angle=20,col= grDevices::colors()[84],
           code=3)
    graphics::mtext(paste("Event depth =",
                round(df.events[i,depth],2),"mm"),
          side=3,line=0.1,col= grDevices::colors()[84])
  }
  
#   dev.off()
  
  
}