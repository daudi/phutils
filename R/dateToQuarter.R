##' Function to convert dates to financial quarters.
##' 
##' Convert dates to financial quarters.
##' 
##' Only works for financial years at the moment.
##' 
##' @param x A vector of dates.
##' @param year.first If TRUE format the result as 2010/11 Q1. If FALSE (the
##' default) format the output with the quarter first, i.e. Q1 2010/11.
##' @return An object of the characters class.
##' @author David Whiting, dwhiting@@nhs.net Mark Chambers,
##' mark.chambers@@nhs.net
##' @keywords utils
##' @examples
##' 
##' library(phutils)
##' x <- seq(from = as.Date("2010-04-01"), to = as.Date("2011-03-31"), by  = 1)
##' y <- data.frame(x = x, 
##'                 month = strftime(x, format = "%m"),
##'                 cmonth = strftime(x, format = "%b"), 
##'                 q = dateToQuarter(x))
##' table(y$q)
##' table(y$q, y$month)
##' 
##' 
##' 
##' 
dateToQuarter <- function(x, year.first = FALSE) {
  # Convert date used to Quarter plus financial year (e.g. 2010-05-15 to Q1 2010/11)
  # year.first: specify if the output should be Q1 2010/11 or 2010/11 Q1
  i <- is.na(x)
  month <- strftime(x, format = "%m")
  qtrs <- character(length(month))
  qtrs[month %in% c("04","05", "06")] <- "Q1"
  qtrs[month %in% c("07","08", "09")] <- "Q2"
  qtrs[month %in% c("10","11", "12")] <- "Q3"
  qtrs[month %in% c("01","02","03")] <- "Q4"
  
  q4 <- month %in% c("01","02","03")
  finYr1 <- strftime(x, format = "%Y")
  finYr1[q4] <- as.numeric(finYr1[q4]) - 1
  finYr2 <- as.numeric(finYr1) + 1
  if (year.first) {
    y <- paste0(finYr1, "/", finYr2, " ", qtrs)
  } else {
    y <- paste0(qtrs, " ", finYr1, "/", finYr2)
  }
  y[i] <- NA
  y
}
