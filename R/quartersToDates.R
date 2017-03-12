##' Function to convert quarters to dates.
##' 
##' Convert representations of quarters to dates so that R can work with them
##' as dates, e.g. in plots.
##' 
##' This function takes two main parameters, a year and a specification of the
##' quarter, and converts them to a date that represents the same period. This
##' is useful for plotting quarters, especially when other date-based
##' information needs to be presented on the same plot.
##' 
##' 'year' can also contain the quarter, for example "2002 q2".
##' 
##' If the year has a forward slash it is assumed to be a financial year, e.g.
##' 2002/03. If this is the case then quarters are shifted forward by three
##' months. So "2002 q4" returns "2002-11-15" while "2002/03 q4" returns
##' "2003-02-15"
##' 
##' 
##' @param year Calendar year as a numeric or character number. Can also
##' contain the quarter (see details below).
##' @param quarter The letter Q (upper or lower case) followed by 1 to 4.
##' @param sep Character that separates financial years. Default is "/", e.g. 2013/14.
##' @return A list with start points, mid-points and end-points of each quarter
##' @author David Whiting, david.whiting@@publichealth.me.uk 
##' @keywords utils
##' @examples
##' 
##' quartersToDates(2002, "q4") # "2002-11-15"
##' 
##' quartersToDates("2002 q4") # "2002-11-15"
##' quartersToDates("2002/03 q4") # "2003-02-15"
##' 
##' quartersToDates(c("2016/17 q1", "2017/18 q4"))
##' quartersToDates(c("2016-17 q1", "2017-18 q4"), sep = "-")
##' 
##' @export
##' 
##' 


quartersToDates <-
  function(year, quarter = NULL, mid.point = TRUE, sep = "/") {
    ## Purpose: Convert a year and quarter to a date class so that
    ## it works with R functions that understand dates.
    
    ## For vectors make sure that we have the same number of years and quarters
    if (!(is.null(quarter) | (length(year) == length(quarter))))
      warning("The number of years and quarters do not match and this is probably going to end in tears.")
    
    financial.year <- FALSE
    if (any(grepl(sep, year))) {
      financial.year <- TRUE
    }
    if (any(nchar(year) != 4)) {
      ## Assume year and quarter in one object. Could be "2002 q1"
      ## for calendar year or "2010/11 q2" for financial year. But
      ## could also be "2002/11" and quarter could be supplied
      ## separately.
      my.year <- substr(year, 1, 4)
      if (any(is.null(quarter)))
        quarter <- gsub(".*([Qq][1-4]).*", "\\1", year)
    } else {
      my.year <- as.numeric(as.character(year))
    }
    
    my.quarter <- as.numeric(gsub("[Qq]", "", quarter))
    my.year <- as.numeric(as.character(my.year))
    
    if (financial.year) {
      i <- my.quarter == 4
      my.year[i] <- my.year[i] + 1
      my.quarter <- my.quarter + 1
    } 
    
    ## beginning
    my.month <- c(1, 4, 7, 10, 1)[my.quarter]
    first.day <- 1
    start.point <- as.Date(paste0(my.year, "-", my.month, "-", first.day), format = "%Y-%m-%d")
    
    ## mid-point
    my.month <- c(2, 5, 8, 11, 2)[my.quarter]
    mid.day <- 15
    mid.point <- as.Date(paste0(my.year, "-", my.month, "-", mid.day), format = "%Y-%m-%d")
    
    ## end
    my.month <- c(3, 6, 9, 12, 3)[my.quarter]
    last.day <- c(31, 30, 30, 31, 31)[my.quarter]
    end.point <- as.Date(paste0(my.year, "-", my.month, "-", last.day), format = "%Y-%m-%d")
    
    list(start.point = start.point,
         mid.point = mid.point,
         end.point = end.point)
    
} 