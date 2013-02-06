dateToQuarter <- function(x, year.first = FALSE) {
  # Convert date used to Quarter plus financial year (e.g. 2010-05-15 to Q1 2010/11)
  month <- strftime(x, format = "%m")
  qtrs <- character(length(month))
  qtrs[month %in% c("04","05", "06")] <- "Q1"
  qtrs[month %in% c("07","08", "09")] <- "Q2"
  qtrs[month %in% c("10","11", "12")] <- "Q3"
  qtrs[month %in% c("01","02","03")] <- "Q4"
  
  finYr1 <- strftime(x, format = "%Y")
  # finYr1[month == "02"] <- as.numeric(finYr1[month == "02"]) - 1
  finYr1[month %in% c("01","02","03")] <- as.numeric(finYr1[month %in% c("01","02","03")]) - 1  # Amended by MC 2013-02-01
  finYr2 <- as.numeric(finYr1) + 1
  if (year.first) {
    y <- paste0(finYr1, "/", finYr2, " ", qtrs)
  } else {
    y <- paste0(qtrs, " ", finYr1, "/", finYr2)
  }
  y
}