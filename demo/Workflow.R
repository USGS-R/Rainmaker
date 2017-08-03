#' DataRetrieval + Rainmaker
#' Set up the program
    setwd("M:/NonPoint Evaluation/Discovery Farms/Rainmaker Analysis")
    library("dataRetrieval", lib.loc="~/R/R-3.4.0/library")
    library("Rainmaker", lib.loc="~/R/R-3.4.0/library")
    #'
#'  Set up the run: RDB file name, no need for .rdb, needs to be actual ID for data retrieval option
    siteNumber <- "PEACEFUL" 
    #'  
#'  dataRetrieval:
    # parameterCd <- "00045"  # Precipitation
    # startDate <- "2010-06-30"
    # endDate <- "2016-09-30"
    # precip_raw <- readNWISuv(siteNumber, parameterCd, startDate, endDate,tz = "America/Chicago")
    #   precip_raw <- renameNWISColumns(precip_raw)
    # precip_prep <- precip_raw
    #   colnames(precip_prep)[colnames(precip_prep)=="dateTime"] <- "pdate"
    #   colnames(precip_prep)[colnames(precip_prep)=="Precip_Inst"] <- "rain"
    #   rain <- "rain"
    #   precip_prep <- precip_prep[precip_prep[rain] > 0.00001,] #remove zeroes
    #' 
    #' 
#'  Load the RDB file:
    precip_raw <- read.csv(file = paste(siteNumber,".rdb",sep=""),sep = "",skip = 3,col.names = c("site.dd", "YEAR", "MONTH", "DAY", "MINUTE", "rain"))
    precip_prep <- RMprep(df=precip_raw,prep.type = 1,date.type = 4)
    rain <- "rain"
    precip_prep <- precip_prep[precip_prep[rain] > 0.00001,] #remove zeroes
    #'
#'  RMevents function: Defines events based on unit value rainfall and user defined parameters
    ieHr <- 2 
    rainthresh <- 0.008
    rain <- "rain"
    time <- "pdate"
    precip_events <- RMevents_sko(df=precip_prep, ieHr=ieHr, rainthresh=rainthresh, rain=rain, time=time)
    precip_event_list <- precip_events$storms2
    #' 
    #' View the event list and check what timeInterval to use in RMerosivity
    #' 
#' RMintense: Computes x-minute maximum intensities -- This step can take a little time to process...
    StormSummary <- RMintense(df=precip_prep, date=time, rain=rain, df.events=precip_event_list,
                          sdate="StartDate", edate="EndDate", depth=rain, xmin=c(5,10,15,30,60))
    #' 
#' RMerosivity: Compute Erositivity Index
    #' This workflow is written to compute both erosivity methods then combine them into one table at the end.
    #'
    timeInterval <- 5
    StormSummary.1 <- as.data.frame(RMerosivity(df=precip_prep, ieHr=ieHr, rain=rain, 
                                            timeInterval=timeInterval, StormSummary=StormSummary, method=1))
    #'
    StormSummary.2 <- as.data.frame(RMerosivity(df=precip_prep, ieHr=ieHr, rain=rain, 
                                            timeInterval=timeInterval, StormSummary=StormSummary, method=2))
    #' 
#' RMarf: Compute x-day antecedent rainfallRMarf
    antecedentDays <- c(1,2,3)
    ARF <- RMarf(df=precip_prep, date=time, rain=rain, df.events=StormSummary, sdate="StartDate", days=antecedentDays, varnameout="ARFdays")
    #'
#' Build final summary table
    #' -  Add Antecedent Rainfall to the summary table
    StormSummary <- cbind(StormSummary, ARF[!names(ARF) %in% names(StormSummary)])
    #' 
    #'    rename the Erosivity columns so they are unique
    names(StormSummary.1)[names(StormSummary.1)=="erosivity"] <- "Erosivity Index, McGregor (1995) [foot-tons force*in/acre*hr*100]"
    names(StormSummary.1)[names(StormSummary.1)=="energy"] <- "Rainfall Energy, McGregor (1995) [foot-tons force*in/acre]"
    names(StormSummary.2)[names(StormSummary.2)=="erosivity"] <- "Erosivity Index, USDA Ag Handbook 537 (1978, 1981) [foot-tons force*in/acre*hr*100]"
    names(StormSummary.2)[names(StormSummary.2)=="energy"] <- "Rainfall Energy, USDA Ag Handbook 537 (1978, 1981) [foot-tons force*in/acre]"
    #'
    #' -  Add the two erosivity computations to the summary table
    StormSummary <- cbind(StormSummary, StormSummary.1[!names(StormSummary.1) %in% names(StormSummary)])
    StormSummary <- cbind(StormSummary, StormSummary.2[!names(StormSummary.2) %in% names(StormSummary)])
    #'   
    #'    rename the columns
    names(StormSummary)[names(StormSummary)=="stormnum"] <- "Storm Number" 
    names(StormSummary)[names(StormSummary)=="StartDate"] <- "Precipitation Start"
    names(StormSummary)[names(StormSummary)=="EndDate"] <- "Precipitation End"
    names(StormSummary)[names(StormSummary)=="rain"] <- "Total Precipitation [Inches]"
    names(StormSummary)[names(StormSummary)=="duration"] <- "Event Duration [Hours]"
    names(StormSummary)[names(StormSummary)=="Ievent"] <- "Event Intensity [in./hr.]"
    names(StormSummary)[names(StormSummary)=="I5"] <- "5 Min Max Intensity [in./hr.]"
    names(StormSummary)[names(StormSummary)=="I10"] <- "10 Min Max Intensity [in./hr.]"
    names(StormSummary)[names(StormSummary)=="I15"] <- "15 Min Max Intensity [in./hr.]"
    names(StormSummary)[names(StormSummary)=="I30"] <- "30 Min Max Intensity [in./hr.]"
    names(StormSummary)[names(StormSummary)=="I60"] <- "60 Min Max Intensity [in./hr.]"
    names(StormSummary)[names(StormSummary)=="ARFdays1"] <- "Antecedent Rainfall 1 Day [Inches]"
    names(StormSummary)[names(StormSummary)=="ARFdays2"] <- "Antecedent Rainfall 2 Day [Inches]"
    names(StormSummary)[names(StormSummary)=="ARFdays3"] <- "Antecedent Rainfall 3 Day [Inches]"
    #' 
#' Save stormSummary to .csv
    write.table(x=StormSummary, file = paste(siteNumber,"_StormSummary.csv",sep=""),sep = ",", row.names = FALSE)
#'##################################################################################################################    
    
#'###Optional Output Raw Data File
    write.table(x=precip_raw, file = paste(siteNumber,"_RawData.csv",sep=""),sep = ",", row.names = FALSE)
    write.table(x=precip_prep, file = paste(siteNumber,"_PrepData.csv",sep=""),sep = ",", row.names = FALSE)
    