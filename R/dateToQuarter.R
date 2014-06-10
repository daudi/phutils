##' Function to convert dates to financial quarters.
##' 
##' Convert dates to financial quarters.
##' 
##' Only works for financial years at the moment.
##' 
##' @param x A vector of dates.
##' @param year.first If TRUE format the result as 2010/11 Q1. If FALSE (the
##' default) format the output with the quarter first, i.e. Q1 2010/11.
##' @param fin.yr If TRUE (the default) format for financial years
##' @param sep The separator between the year and the quarter. Default is a space.
##' 
##' @return An object of the characters class.
##' @author David Whiting, dwhiting@@nhs.net Mark Chambers,
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
##' 
##' @export
##' 
dateToQuarter <- function(x, year.first = FALSE, fin.yr = TRUE, sep = " ") {
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

  finYr1 <- strftime(x, format = "%Y")
  
  if (fin.yr) {
    q4 <- month %in% c("01","02","03")
    finYr1[q4] <- as.numeric(finYr1[q4]) - 1
    finYr2 <- as.numeric(finYr1) + 1
    yr.part <- paste0(finYr1, "/", finYr2)
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
