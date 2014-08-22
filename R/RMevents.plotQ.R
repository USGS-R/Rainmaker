#' RMevents.plotQ
#'
#' Function to graph rainfall and flow for a given x-day window around specified event periods
#'
#' @param df dataframe with unit value rainfall data
#' @param dfQ dataframe with unit value Q data
#' @param date string Date column in df as POSIXct
#' @param Qdate string Date column in dfQ as POSIXct
#' @param rain string Column in df with instantaneous rain values
#' @param Q string Column in dfQ with instantaneous Q values
#' @param df.events dataframe with start and end dates/times for events
#' @param sdate string Start date column in df.events rain file as POSIXct
#' @param edate string End date column in df.events as POSIXct
#' @param erain string Event rainfall depth column in df.events
#' @param plot.buffer numeric Used to define plotting window in days. Graphs will include
#     data Time period preceding beginning of event for including in the graphs
#' @param logy string "y" if log y-axis for Q or "" if linear axis. Will default to "".
#                                if not specific or if minimum Q <= 0.
#' @param site.name string
#' @export 
#' @return logy
#' @examples
#' RDB <- CedarRRain
#' dfQ <- cedarq
#' dfQ <- RMprep(dfQ,prep.type=1,date.type=3,tz="CST6CDT")
#' RDB2 <- RMprep(RDB,prep.type=1,date.type=1,dates.in="CST.Time",tz="CST6CDT")
#' RDB3 <- subset(RDB2,upload.ph3_site_basin_cedar_creek.Id.0....Geographical.Mean.kg.m.2.>-1)
#' event.list <- RMevents(df=RDB3,ieHr=6,rainthresh=0.2,rain="upload.ph3_site_basin_cedar_creek.Id.0....Geographical.Mean.kg.m.2.")
#' events.0.2 <- event.list$storms2
#' site.name <- "Example Site"
#' pdf(paste(site.name,"_events.pdf",sep=""))
#' RMevents.plotQ(RDB3,dfQ,rain="upload.ph3_site_basin_cedar_creek.Id.0....Geographical.Mean.kg.m.2.",df.events=events.0.2,erain="rain",site.name=site.name)
#' dev.off()
RMevents.plotQ <- function(df,dfQ,date="pdate",Qdate="pdate",rain = "rain",Q="Q",
                           df.events,sdate="StartDate",edate="EndDate", erain="depth",
                           plot.buffer=3,logy="",site.name="") {
    
  df.events[,sdate] <- as.POSIXct(df.events[,sdate])
  df.events[,edate] <- as.POSIXct(df.events[,edate])  
  df <- rbind(df[1,],subset(df[-1,],rain>0.0))
  
#   pdf(paste(site.name,"_events.pdf",sep=""))
  
  # Define plot layout: panel 1 for Q and panel 2 for FIB
  mylayout <- matrix(c(1,
                       1,
                       2,
                       2,
                       2),5,1,byrow=TRUE)
  layout(mylayout)
  
  main.title <- paste(site.name,"Precipitation and Q Event")
  for (i in 1:(nrow(df.events))) {
    ########################## Graph Precip  ###########################################
    p.sdate <- as.POSIXct(df.events[i,sdate] - plot.buffer*24*3600/2,tz="")
    p.edate <- as.POSIXct(df.events[i,edate] + plot.buffer*24*3600/2,tz="")
    subdf <- subset(df, df[,date]>=p.sdate & df[,date]<=p.edate)
    rmax <- max(subdf[,rain] + 0.3)
    subrain <- subdf[,rain]
    subdate <- as.POSIXct(subdf[,date])
    #Set Margins for first plot
    par(mar= c(0, 4, 4, 2) + 0.1)
    plot(subrain~subdate,
         #       data=subdf,
         type="h",
         xaxt="n",
         ylab="precipitation (mm)", 
         xlab="",
         col="blue",
         lwd=1,
         yaxs="i",
         ylim=c(rmax,0),
         xlim=c(p.sdate,p.edate),
         main = "")
    mtext(main.title,side=3,line=2,cex=1.5)
    
    mtext(paste("Event depth =",
                round(df.events[i,erain],2),"mm"),
          side=3,line=0.5,col=colors()[84])
    arrows(df.events[i,sdate],(rmax-0.15),
           df.events[i,edate],(rmax-0.15),
           length=0.07,angle=20,col=colors()[84],
           code=3) 
    
    
    ########################## Graph Q  ################################################
    subdfQ <- subset(dfQ, dfQ[,Qdate]>=p.sdate & dfQ[,Qdate]<=p.edate)
    Qmax <- max(subdfQ[,Q] *1.05)
    if(Qmax < 0) {Qmax <- Qmax*0.95}
    if(Qmax > 0) {Qmax <- Qmax*1.05} 
    Qmin <- min(c(subdfQ[,Q]))
    if(Qmin <= 0) {Qmin <- Qmin*1.05; logy <- ""}
    if(Qmin > 0) {Qmin <- Qmin*0.95; logy <- logy}
    
    subQ <- subdfQ[,Q]
    subdateQ <- as.POSIXct(subdfQ[,date])
    
    #Set Margins for second plot
    par(mar= c(5, 4, 0, 2) + 0.1)
    plot(subQ~subdateQ,
         #       data=subdf,
         type="l",
         xaxt="n",
         ylab="Flow (cfs)", 
         xlab="",
         col="red",
         lwd=1,
         yaxs="i",
         log=logy,
         ylim=c(Qmin,Qmax),
         xlim=c(p.sdate,p.edate),
         main = "")
    
    r <- as.POSIXct(trunc(range(subdf[,date]), "days"))
    r[2] <- r[2]+24*3600
    rhour <- seq(r[1], r[2], by=24*3600/4)
    rday <- seq(r[1], r[2], by="days")
    axis.POSIXct(1,subdf[,date],at=rhour,format=" ",tcl=-0.2)
    axis.POSIXct(1,subdf[,date],at=rday,format=" ",tcl=-0.5)
    axis.POSIXct(3,subdf[,date],at=rhour,format=" ",tcl=0.2)
    axis.POSIXct(3,subdf[,date],at=rday,format=" ",tcl=0.5)
    axis.POSIXct(1,subdf[,date],format = "%m/%d/%y")
    #  timeline <- 
    abline(v=p.sdate,lty=3,col=colors()[100])
    abline(v=p.edate,lty=3,col=colors()[100])
    
    arrows(df.events[i,sdate],(rmax-0.15),
           df.events[i,edate],(rmax-0.15),
           length=0.07,angle=20,col=colors()[84],
           code=3) 
    
    #  abline(v=df.events[i,sdate])
    #  abline(v=df.events[i,edate])
    
    #  axis.POSIXct(3,subdf$date,format = "%m/%d/%y",
    #               at=c(df.events[i,sdate],df.events[i,edate]),
    #               tcl=2)
    
    
  }
}