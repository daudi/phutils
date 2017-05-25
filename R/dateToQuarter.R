##' Function to convert dates to financial quarters.
##' 
##' Convert dates to quarters. 
##' 
##' 
##' @param x A vector of dates.
##' @param year.first If TRUE format the result as 2010/2011 Q1. If FALSE (the
##' default) format the output with the quarter first, i.e. Q1 2010/2011.
##' @param fin.yr If TRUE (the default) format for financial years
##' @param year.format A character string representing the year format. The default is "\%Y/\%Y", i.e. a 4-digit year 
##' followed by a slash, followed by a 4-digit year, e.g. 2014/2015. A lowercase %y represents a 2-digit year, i.e. 
##' "\%y/\%y" would be formatted as "14/15". The separator can also be changed, e.g. "\%Y-\%y" would produce "2014-15".
##' @param sep The separator between the year and the quarter. Default is a space.
##' 
##' @return An object of the characters class.
##' @author David Whiting, dwhiting@@nhs.net david.whiting@@publichealth.me.uk Mark Chambers,
##' mark.chambers@@nhs.net
##' @keywords utils
##' @examples
##' 
##' x <- seq(from = as.Date("2010-04-01"), to = as.Date("2011-03-31"), by  = 1)
##' y <- data.frame(x = x, 
##'                 month = strftime(x, format = "%m"),
##'                 cmonth = strftime(x, format = "%b"), 
##'                 q = dateToQuarter(x))
##' table(y$q)
##' table(y$q, y$month)
##' 
##' ## Now with the year first (e.g. to help with sorting)
##' y <- data.frame(x = x,
##'   month = strftime(x, format = "%m"),
##'   cmonth = strftime(x, format = "%b"),
##'   q = dateToQuarter(x, year.first = TRUE))
##'   table(y$q)
##'   table(y$q, y$month)
##' 
##' dateToQuarter(Sys.Date())
##' dateToQuarter(Sys.Date(), fin.yr=FALSE)
##' dateToQuarter(Sys.Date(), fin.yr=FALSE, sep = ">>>")
##' dateToQuarter(Sys.Date(), fin.yr=FALSE, sep = "-")
##' dateToQuarter(Sys.Date(), fin.yr=FALSE, sep = "-", year.first=TRUE)
##' dateToQuarter(Sys.Date(), year.format = "%Y/%y")
##' dateToQuarter(Sys.Date(), year.format = "%y/%y")
##' dateToQuarter(Sys.Date(), year.format = "%Y-%y")
##' dd <- seq.Date(from = as.Date("2014/01/01"), to = as.Date("2015/01/01"), by = 1)
##' table(dateToQuarter(dd, year.format = "%Y-%y"))
##' 
##' @export
##' 
dateToQuarter <- function(x, year.first = FALSE, fin.yr = TRUE, year.format = "%Y/%Y", sep = " ") {
  # Convert date used to Quarter plus financial year (e.g. 2010-05-15 to Q1 2010/11)
  # year.first: specify if the output should be Q1 2010/11 or 2010/11 Q1
  i <- is.na(x)
  month <- strftime(x, format = "%m")
  qtrs <- character(length(month))
  if (fin.yr) {
    quarter.labels <- c("Q1", "Q2", "Q3", "Q4")
  } else {
    quarter.labels <- c("Q2", "Q3", "Q4", "Q1")
  }
  qtrs[month %in% c("04","05", "06")] <- quarter.labels[1]
  qtrs[month %in% c("07","08", "09")] <- quarter.labels[2]
  qtrs[month %in% c("10","11", "12")] <- quarter.labels[3]
  qtrs[month %in% c("01","02","03")]  <- quarter.labels[4]

  yr1.format <- substr(year.format, 1, 2)
  yr2.format <- substr(year.format, 4, 5)
  yr.sep  <- substr(year.format, 3, 3)
  finYr1 <- strftime(x, format = yr1.format)
  
  if (fin.yr) {
    q4 <- month %in% c("01","02","03")
    finYr1[q4] <- as.numeric(finYr1[q4]) - 1    
    finYr2 <- strftime(strptime(paste0(finYr1, "06", "01"), format = paste0(yr1.format, "%m%d")) + (365 * 60 * 60 * 24), format = yr2.format)
    yr.part <- paste0(finYr1, yr.sep, finYr2)
  } else {
    yr.part <- finYr1
  }
  
  if (year.first) {
    y <- paste(yr.part, qtrs, sep = sep)
  } else {
    y <- paste(qtrs, yr.part, sep = sep)
  }
  y[i] <- NA
  y
}
