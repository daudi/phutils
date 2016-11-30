##' describe an integer vector
##' 
##' @param x An integer vector
##' @param colname The name of the vector (optional)
##' @export
##' 
descr.integer <- function(x, this_col = NULL) {
  descr.numeric(x, this_col)
}