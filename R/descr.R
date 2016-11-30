##' describe an R object 
##' 
##' Generic function that calls class-specific functions.
##' 
##' @param x An object
##' @param colname The name of the vector (optional)
##' @export
##' 
descr <- function(x, ...) {
  cat(paste("Class:", class(x), "\n\n"))
  UseMethod("desc", x)
}
