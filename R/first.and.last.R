##' Return the first and last values from a column of a dataframe.
##' 
##' Get the first and last values of a data.frame. This is probably most useful if 
##' by.var is specified as  ??? what??? 
##' 
##' I can't remember why this was useful. I certainly haven't used it for a long time.
##' 
##' 
##' @param x The dataframe
##' @param fld The column you want. A quoted name, e.g. "weight"
##' @param by.var If specified, this will be run for each by.var.
##' @param na.rm Remove NAs first?
##' 
##' @details Dates get returned as their numeric representation, i.e. they lose their 
##' date attributes. I'm not sure how to fix this. One nasty fudge would be to convert
##' the dates to characters first, run this, then convert back to dates.
##' 
##' THIS DOES NOT (CUURENTLY) SORT THE DATA, so if value order matters you will need to sort it yourself.
##' 
##' @return A data.frame with three columns: first value, last value and length of the group. If by.var is 
##' specified each value of by.var is used for the row names in the data.frame.
##' 
##' @examples
##' x <- runif(n = 100)
##' y <- rep(letters[1:5], 20)
##' z <- data.frame(x = x, y = y)
##' first.and.last(z, "x", y)
##' 
##' @export

first.and.last <- function(x, fld, by.var = NULL, na.rm = FALSE) {
  
  f.and.l <- function(x, fld, na.rm = FALSE) {
    y <- with(x, get(fld))
    if (na.rm)
      y <- y[!is.na(y)]
    
    len.y <- length(y)
    if (len.y > 0) {
      first <- y[1]
      last <- y[len.y]
    } else {
      first <- NA
      last  <- NA
    }
    list(first = first, last = last, length = len.y)
  }

  if (!is.null(by.var)) {
    res <- by(x, by.var, f.and.l, fld = fld, na.rm = na.rm)
    res.names <- names(res)
    res <- do.call("rbind", res)
    res <- data.frame(res)
  } else {
    res <- f.and.l(x, fld, na.rm = na.rm)
  }
  res <- data.frame(first = unlist(res$first), last = unlist(res$last), length = unlist(res$length))
  res
}


