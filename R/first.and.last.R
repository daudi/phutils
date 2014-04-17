##' Return the first and last values from a column of a dataframe.
##' 
##' Get the first and last values of a data.frame. This is probably most useful if 
##' by.var is specified as 
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
  res
}


## Example
# load("v:/33 - Pembroke Court/Public Health Intelligence/Datasets/Secure/tipping the balance/ttb_Alldata_reShaped.Rda")
# ## Make sure the data are in the right order
# x <- x[order(x$id, x$time), ]
# 
# first.and.last(x, "AppointmentDate")
# first.and.last(x, "AppointmentDate", na.rm = TRUE)
# first.and.last(x[x$id == 1, ], "AppointmentDate")
# 
# xx <- x[900:1200, ]
# xx <- xx[xx$AppointmentWeight > 0 & !is.na(xx$AppointmentWeight), ]
# first.and.last(xx, "AppointmentDate", by.var = xx$id, na.rm = TRUE)
# first.and.last(xx, "AppointmentHeight", by.var = xx$id, na.rm = TRUE)
# 
# first.and.last(xx, "AppointmentWeight", by.var = xx$id, na.rm = FALSE)
# 
# 
# first.and.last(xx, "AppointmentWeight", na.rm = TRUE)
