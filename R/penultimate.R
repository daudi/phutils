##' Return the penulimate item from a vector
##' 
##' @param x A vector
##' @return The penultimate item from the vector. The
##' data type is determined by the data type of the vector.
##' @keywords utils
##' 
##' @export

penultimate <- function(x){
  y <- sort(unique(x))
  if (length(y) > 1) {
    y <- y[length(y) - 1]
  } else {
    y <- y[length(y)]
  }
  y
}