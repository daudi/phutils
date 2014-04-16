##' Return the financial year of a given date
##' 
##' @param date A date
##' @details At the moment this is not vectorised.
##' @export
##' @author david.whiting@@publichealth.me.uk
##' @example
##' fin.yr(Sys.Date())

fin.yr <- function(date) {
  if (as.numeric(strftime(date, format = "%m")) >= 4) {
    x <- paste0(as.numeric(strftime(date, format = "%Y")), "/",
                as.numeric(strftime(date, format = "%y")) + 1)
    
  } else {
    x <- paste0(as.numeric(strftime(date, format = "%Y")) - 1, "/",
                as.numeric(strftime(date, format = "%y")))
  }
  x
}
