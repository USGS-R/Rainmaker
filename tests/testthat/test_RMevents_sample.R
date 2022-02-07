# tests the function RMevents_sample

context("RMevents_sample")

RDB <- CedarRRain
cedarSamples <- cedarSamples
names(RDB)[2] <- "UVRain"
RDB2 <- RMprep(RDB, prep.type = 1, date.type = 1,
               dates.in = "CST.Time", tz = "CST6CDT")

eventListSamples <- RMevents_sample(dfrain = RDB2,
                                    ieHr = 6,
                                    rain = "UVRain",
                                    time = "pdate",
                                    dfsamples = cedarSamples,
                                    bdate = "pSstart",
                                    edate = "pSend")


test_that("RMevents_sample", {
  
  expect_type(eventListSamples[1], "list")
  expect_type(eventListSamples[2], "list")
  expect_type(eventListSamples[3], "list")
  expect_type(eventListSamples[4], "list")
  expect_true(identical(eventListSamples$storms, eventListSamples$storms2))
  
  expect_type(eventListSamples$storms$stormnum, "integer")
  expect_s3_class(eventListSamples$storms$StartDate, "POSIXct")
  expect_s3_class(eventListSamples$storms$EndDate, "POSIXct")
  expect_type(eventListSamples$storms$rain, "double")
  
  expect_type(eventListSamples$storms2$stormnum, "integer")
  expect_s3_class(eventListSamples$storms2$StartDate, "POSIXct")
  expect_s3_class(eventListSamples$storms2$EndDate, "POSIXct")
  expect_type(eventListSamples$storms2$rain, "double")
  
  expect_type(eventListSamples$tipsbystorm$UVRain, "double")
  expect_s3_class(eventListSamples$tipsbystorm$pdate, "POSIXct")
  expect_type(eventListSamples$tipsbystorm$dif_time, "double")
  expect_type(eventListSamples$tipsbystorm$event, "integer")
  
  expect_equal(round(eventListSamples$storms$rain[1], 3), 27.576)
  expect_equal(eventListSamples$storms$StartDate[1], 
               as.POSIXct("2008-01-07 05:00:00", tz = "CST6CDT"))
  expect_equal(eventListSamples$storms$EndDate[1],
               as.POSIXct("2008-01-08 19:00:00", tz = "CST6CDT"))
  
  expect_equal(ncol(eventListSamples$storms), 4)
  expect_equal(nrow(eventListSamples$storms), 17)
  
})