.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste(strwrap(
    'USGS Support Package: 
    https://owi.usgs.gov/R/packages.html#support
    Report issues and ask questions:
    https://github.com/USGS-R/Rainmaker/issues'),
    collapse='\n'))
}

utils::globalVariables(c("pdate"))

#' cedarq
#'
#' Instantaneous values of flow from Cedar Creek
#'
#' @name cedarq
#' @docType data
#' @author Steve Corsi \email{srcorsi@@usgs.gov}
#' @keywords rainmaker
NULL

#' CedarRRain
#'
#' Instantaneous values of rainfall from Cedar Creek
#'
#' @name CedarRRain
#' @docType data
#' @author Steve Corsi \email{srcorsi@@usgs.gov}
#' @keywords rainmaker
NULL

#' cedarSamples
#'
#' Beginning and ending dates and times for composite samples collected at Cedar Creek. This information can be used to determine rainfall for specific time periods using RMeventsSamples.
#'
#' @name cedarSamples
#' @docType data
#' @author Steve Corsi \email{srcorsi@@usgs.gov}
#' @keywords rainmaker
NULL

#' Rainmaker
#'
#' Rainmaker is a collection of functions used to process instantaneous rainfall data to define event rainfall depths, rainfall intensities and antecedent rainfall values. Event definition can be determined solely within rainmaker using the instantaneous rainfall data or can be focused directly on dates and times of interest (for example, using specific water quality sampling periods).
#' 
#' \tabular{ll}{
#' Package: \tab Rainmaker\cr
#' Type: \tab Package\cr
#' Version: \tab 1.0.0\cr
#' Date: \tab 2014-01-10\cr
#' License: \tab Unlimited for this package, dependencies have more restrictive licensing.\cr
#' Copyright: \tab This software is in the public domain because it contains materials
#' that originally came from the United States Geological Survey, an agency of
#' the United States Department of Interior. For more information, see the
#' official USGS copyright policy at
#' http://www.usgs.gov/visual-id/credit_usgs.html#copyright\cr
#' LazyLoad: \tab yes\cr
#' }
#'
#' Collection of functions..
#'
#' @name Rainmaker-package
#' @docType package
#' @author Steve Corsi \email{srcorsi@@usgs.gov}
NULL