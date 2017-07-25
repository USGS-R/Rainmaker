#' RMerosivity
#' 
#' @description This function computes the erosive power of the rainfall
#' @param df dataframe with instantaneous rainfall, same df used for RMIntense
#' @param ieHr time between events in hours
#' @param timeInterval the minimum time between rows of data, 
#'   assumed to be the time it took for precip to accumulate in the collector before the first tip of an event 
#' @param method choose which energy equation to use (see below)
#' @param rain string column name of rainfall unit values, defaults to "rain"
#' @param StormSummary dataframe output by RMIntense, defaults to "StormSummary"
#' 
#' method=1: McGregor (1995) Supercedes Brown and Foster equation (1987), which superceded Agriculture Handbook 537 (1979).
#' 
#' method=2: Wischmeier, Agriculture Handbook 537 (1979, 1981), correct computation of formula 2 found in AH537
#' 
#' method=3: Original Rainmaker (1997) USGS Wisconsin Water Science Center, based on equation in Agriculture Handbook 537. Storms with I30>2.5 are incorrectly computed.
#' @export

#' @references McGregor, K. C., R. L. Binger, A. J. Bowie, and G. R. Foster. 1995. Erosivity index values for northern Mississippi. Trans. Amer. Soc. Agric. Eng. 38:1039-1047;
#' 
#' Wischmeier, W. H. and D. D. Smith. 1978. Predicting rainfall erosion losses—A guide to conservation planning. U.S. Department of Agriculture, Agriculture Handbook 537, 58 pp.
#' 
#' Wischmeier, W. H. and D. D. Smith. 1981. Supplement and Errata for "Predicting rainfall erosion losses—A guide to conservation planning". U.S. Department of Agriculture, Agriculture Handbook 537, 58 pp.
#' 
#' Renard, K. G., G. R. Foster, G. A. Weesies, D. K. McCool, and D. C. Yoder. 1997. Predicting soil erosion by water: A guide to conservation planning with the Revised Soil Loss Equation (RUSLE). U.S. Department of Agriculture, Agriculture Handbook 703, 404 pp.

RMerosivity <- function(df, ieHr, timeInterval, method, rain="rain", StormSummary=StormSummary){
  #Prep file for computation
  library(dplyr)
  
  if(!(rain %in% names(df))){
    stop(rain, " not in df")
  }
  
  #Add a line here to remove any zeroes in the df
  
  #add a dummy row to top of df.PrecipPrep
  x <- data.frame(rain = 0,
                  pdate = df$pdate[1] - 60*60*24)
  df <- bind_rows(x, df)
  
  #find the time between each row, place that value in a column titled "time_gap"
  df$time_between <- NA
  df$time_between[-1] <- diff(df$pdate, lag = 1)
  dif_time <- diff(df[,'pdate'])
  ieMin = 60*ieHr
  
  #re-establish the event numbers
  df["event"] <- NA
  df[1, "event"] <- 1
  for (i in 2:nrow(df)){
    if (dif_time[[i-1]] >= ieMin) {
      df$event[i] <- df$event[i-1] + 1
    } else {
      df$event[i] <- df$event[i-1]
    }
  }
  
  #if the event number is the same as the line above, fill time_gap column with "time_between", else fill in with timeInterval
  df$time_gap <- NA
  for(i in 2:nrow(df)){
    df$time_gap[i] <- ifelse(df$event[i] == df$event[i-1], df$time_between[i], timeInterval)
  }
  
  #find incremental intensity
  df$intensity <- 60*df$rain/df$time_gap
  
  #find incremental energy
  df$energy <- NA
  df$energy <- ifelse(df$intensity < 3,
                      df$rain*(916+331*log10(df$intensity)),
                      1074*df$rain)
  
  #compute incremental energy using desired method
  if(method==1) df$energy <- df[[rain]]*(1099*(1-0.72*exp(-2.08*df$intensity)))
  if(method==2) df$energy <- ifelse(df$intensity < 3,
                                    df[[rain]]*(916+331*log10(df$intensity)),
                                    1074*df[[rain]])
  if(method==3) df$energy <- ifelse(df$intensity < 3,
                                    df[[rain]]*(916+331*log10(df$intensity)),
                                    1074*df$rain)
  #sum energy for each storm
  StormSummary$energy <- NA  
  for(i in 1:nrow(StormSummary)){
    storm_i <- filter(df, pdate >= StormSummary$StartDate[i],
                      pdate <= StormSummary$EndDate[i])
    StormSummary$energy[i] <- sum(storm_i$energy, na.rm = TRUE)
    
  }
  
  #compute erosivity using desired method
  if(method==1) StormSummary$erosivity <- 0.01*StormSummary$I30*StormSummary$energy
  if(method==2) StormSummary$erosivity <- 0.01*StormSummary$I30*StormSummary$energy
  if(method==3) StormSummary$erosivity <- 0.01*ifelse(StormSummary$I30< 2.5,
                                                      StormSummary$I30*StormSummary$energy,
                                                      2.5*StormSummary$energy) #this is the step that is incorrect in Fortran Rainmaker
  return(StormSummary)
}
