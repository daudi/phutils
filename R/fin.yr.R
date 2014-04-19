##' Return the financial year of a given date
##' 
##' @param date A date or vector of dates. 
##' @details The date can be a date or can be a character, but only if in the format of 2014/03/12, etc.
##' @export
##' @author david.whiting@@publichealth.me.uk
##' @examples
##' fin.yr(Sys.Date())
##' x <- Sys.Date - (1:10) * 300
##' fin.yr(x)

fin.yr <- function(date) {
  
  .fin.yr <- function(date) {
    if (as.numeric(strftime(date, format = "%m")) >= 4) {
      x <- paste0(as.numeric(strftime(date, format = "%Y")), "/",
                  as.numeric(strftime(date, format = "%y")) + 1)
      
    } else {
      x <- paste0(as.numeric(strftime(date, format = "%Y")) - 1, "/",
                  as.numeric(strftime(date, format = "%y")))
    }
    x
  }
  
  unlist(lapply(date, .fin.yr))
}
