##' Calculate age in years
##' 
##' Calculate age in years from two whole dates. 
##' @param first The first date. If you want positive ages, then this is the older date. can be a vector of dates. The function will try to coerce to date class if not already of class date.
##' @param second The second date. 
##' @return A numeric vector of ages in years.
##' @author David Whiting david.whiting@@publichealth.me.uk
##' @references Based on http://r.789695.n4.nabble.com/Calculate-difference-between-dates-in-years-td835196.html
##' 
##' @export
##' 
##' 
##' @examples
##' age.years("2005-09-23", Sys.Date())

age.years <- function(first, second)
{
  if (any(class(first) != "Date"))
    first <- as.Date(first)
  if (any(class(second) != "Date"))
    second <- as.Date(second)
  lt <- data.frame(first, second)
  age <- as.numeric(format(lt[, "second"], format = "%Y")) - as.numeric(format(lt[, "first"], format = "%Y"))
  x <- paste(format(lt[, "second"], format = "%Y"), "-", format(lt[, "first"], format = "%m-%d"), sep = "")
  x[grepl("NA", x)] <- NA
  first <- as.Date(x)
  i <- which(first > lt[, "second"]) 
  age[i] <- age[i] - 1
  age
} 