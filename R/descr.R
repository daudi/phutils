##' describe an R object 
##' 
##' Generic function that calls class-specific functions.
##' the descr() suite of functions provide a more detailed summary of an 
##' object than summary(), including where appropriate a histogram to show
##' the distribution. This is still a work in progress, but running it on a 
##' data.frame has been useful.
##' 
##' @param x An object
##' @param colname The name of the vector (optional)
##' @export
##' 
descr <- function(x, ...) {
  cat(paste("Class:", class(x), "\n\n"))
  UseMethod("descr", x)
}
