############################# run.rainmaker function #############################

# function to run through Rainmaker and extract events, event characteristics

run.rainmaker <- function(precip_raw, ieHr = 2, rainthresh = 0.008, 
                          xmin = c(5,10,15,30,60), antecedentDays = c(1,3,7,14)) {
  
  # calculate events
  events <- RMevents(df = precip_raw, ieHr = ieHr, rain = 'rain', time = 'pdate')
  
  # extract data from events output
  events_list <- events$storms
  tipsbystorm <- events$tipsbystorm
  
  # calculate storm intensity at different time intervals
  StormSummary <- RMintensity(df=tipsbystorm, date="pdate", rain="rain", df.events=events_list,
                              sdate="StartDate", edate="EndDate", depth="rain", xmin=xmin)
  
  # calculate erosivity 
  # method 1
  StormSummary <- RMerosivity(df = tipsbystorm, ieHr=ieHr, rain='rain', StormSummary=StormSummary, method=1)
  StormSummary <- rename(StormSummary, 'erosivity_m1' = "erosivity", 'energy_m1' = 'energy')
  
  # method 2
  StormSummary <- RMerosivity(df= tipsbystorm, ieHr=ieHr, rain="rain", StormSummary=StormSummary, method=2)
  StormSummary <- rename(StormSummary, 'erosivity_m2' = "erosivity", 'energy_m2' = 'energy')
  
  
  # calculate antecedent rain
  StormSummary <- RMarf(df = precip_raw, date = 'pdate', rain = 'rain', df.events = StormSummary, 
                        sdate = "StartDate", days = antecedentDays, varnameout = "ARFdays")
  
  
  # give storms IDs
  # StormSummary$unique_storm_number <- wq.dat$unique_storm_number
  
  return(StormSummary)
}