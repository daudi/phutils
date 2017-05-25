##' Parse incomplete dates to get a reasonable date value
##' 
##' @param x A vector of incomplete dates, e.g. 2001 - 03, 2008/09
##' @export
##' @details Useful for plotting. This is not designed for turning 2010-11 into 15 November 2010,
##' this will return the mid-point of 2010 and 2011. It is currently focussed on handling dates 
##' in the PHE fingertips data.
##' @return A vector of class Date 



date_parse <- function(x) {
  
  if (is.null(x)) stop("x cannot be NULL")
  x <- factor(x)
  dd <- levels(x)
  
  res <- rep(as.Date("1000-01-01"), length(dd))
  
  ## E.g. 2001 - 03
  i <- grepl("[1-2][0-9][0-9][0-9] *- *[0-9][0-9]", dd)
  if (sum(i) > 0) {
    ff <- dd[i]
    ff1 <- as.numeric(gsub("([1-2][0-9][0-9][0-9]).*", "\\1", ff))
    ff2 <- as.numeric(gsub("[1-2][0-9][0-9][0-9] *- *(.*)", "\\1", ff))
    j <- ff2 < 50
    ff2[j] <- 2000 + ff2
    ff2[!j] <- 1900 + ff2
    ff1 <- as.Date(paste0(ff1, "-07-01"))
    ff2 <- as.Date(paste0(ff2, "-07-01"))
    ff3 <- ff1 + ((ff2 - ff1) / 2)
    res[i] <- as.Date(ff3)
  }
  
  ## E.g. "2008/09"
  i <- grepl("[1-2][0-9][0-9][0-9]/[0-9][0-9]$", dd)
  if (sum(i) > 0) {
    ff <- dd[i]
    ff1 <- as.numeric(gsub("([1-2][0-9][0-9][0-9]).*", "\\1", ff))
    res[i] <- as.Date(paste0(ff1, "-09-30"))  
  } 
  
  ## E.g. "2008/09 - 10/11"
  i <- grepl("[1-2][0-9][0-9][0-9]/[0-9][0-9] *- *[0-9][0-9]/[0-9][0-9]", dd)
  if (sum(i) > 0) {
    ff <- dd[i]
    ff1 <- as.numeric(gsub("([1-2][0-9][0-9][0-9]).*", "\\1", ff))
    ff2 <- as.numeric(gsub(".* *- *[0-9][0-9]/([0-9][0-9])", "\\1", ff))
    j <- ff2 < 50
    ff2[j] <- 2000 + ff2
    ff2[!j] <- 1900 + ff2
    ff1 <- as.Date(paste0(ff1, "-09-30"))
    ff2 <- as.Date(paste0(ff2, "-09-30"))
    ff3 <- ff1 + ((ff2 - ff1) / 2)
    res[i] <- ff3
  } 
  
  ## E.g. "Apr-08"
  i <- grepl("[A-Za-z][A-Za-z][A-Za-z]-[0-9][0-9]", dd)
  if (sum(i) > 0) {
    ff <- dd[i]
    ff1 <- paste0("01-", ff)
    ff3 <- as.Date(ff1, format = "%d-%b-%y")
    res[i] <- ff3
  } 
  
  ## E.g. "Jul 2008"
  i <- grepl("[A-Za-z][A-Za-z][A-Za-z] [0-9][0-9][0-9][0-9]", dd)
  if (sum(i) > 0) {
    ff <- dd[i]
    ff1 <- paste0("01-", ff)
    ff3 <- as.Date(ff1, format = "%d-%b %Y")
    res[i] <- ff3
  } 
  
  ## E.g. "2015"
  i <- grepl("^[0-9][0-9][0-9][0-9]$", dd)
  if (sum(i) > 0) {
    ff <- dd[i]
    ff1 <- paste0(ff, "-07-01")
    ff3 <- as.Date(ff1)
    res[i] <- ff3
  } 
  
  ## E.g. "2009 Q1"
  i <- grepl("^[0-9][0-9][0-9][0-9] Q[1-4]", dd)
  if (sum(i) > 0) {
    ff <- dd[i]
    ff1 <- substr(ff, 1, 4)
    ff2 <- gsub(".*Q([1-4])", "\\1", ff)
    ff3 <- quarterToDates(ff1, ff2)
    res[i] <- ff3
  } 
  
  i <- grepl("^[0-9][0-9][0-9][0-9]/[0-9][0-9] Q[1-4]", dd)
  if (sum(i) > 0) {
    ff <- dd[i]
    ff1 <- substr(ff, 1, 4)
    ff2 <- gsub(".*Q([1-4])", "\\1", ff)
    ff3 <- quarterToDates(ff1, ff2)
    ## Financial quarters are one quarter ahead (in date terms)
    res[i] <- ff3 + (365 / 4)
  } 
  
  
  levels(x) <- as.character(res)
  x <- as.Date(x)
  if (any(is.na(x))) warning("Some dates have not been converted (look for NA)")
  if (any(x == as.Date("1000-01-01"))) warning("Some dates have not been converted (look for 1000-01-01)")
  x
}
