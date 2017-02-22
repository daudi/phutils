##' Function to join two data.frames column by column.
##' 
##' Join two data.frames taking the first column from the first data.frame, the
##' first from the second data.frame then the second from the first data.frame
##' and so on.
##' 
##' This is useful, for example, when you have counts in one data.frame and
##' have calculated percents in another, and then want to display the result
##' with count followed by percent.
##' 
##' x and y must have the same number of rows and columns.
##' 
##' @param x A data.frame or matrix.
##' @param y Another data.frame with the same number of rows and columns as x,
##' also of numeric or integer values.
##' @return A data.frame that is a combination of the two supplied data.frames.
##' The result will be coerced to numeric if any of the input columns are
##' numeric.
##' @author David Whiting, dwhiting@@nhs.net david.whiting@@publichealth.me.uk
##' @keywords utils
##' @examples
##' 
##' ## Create a data.frame
##' x <- 1:10
##' y <- 10:1
##' a <- letters[x]
##' z <- data.frame(x, y, a)
##' 
##' ## Create another data.frame
##' x <- 11:20
##' y <- 20:11
##' a <- letters[x]
##' zz <- data.frame(x, y, a)
##' 
##' ## Have a look at z and zz:
##' z
##' zz
##' 
##' splice(z, zz)
##' 
##' @export
##' 
splice <- function (x, y) {
  stopifnot(ncol(x) == ncol(y))
  stopifnot(nrow(x) == nrow(y))
  z <- data.frame(x, y)
  cols <- as.vector(t(matrix(1:ncol(z), ncol(z) / 2)))
  z[, cols]
}
