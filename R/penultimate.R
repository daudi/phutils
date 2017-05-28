##' Return the penulimate item from a vector
##' 
##' @param x A vector
##' @details The vector is sorted so this function returns the penultimate value, not 
##' just the second to last item in the list. See the examples.
##' @return The penultimate item from the vector. The
##' data type is determined by the data type of the vector. This is the penultimate 
##' item by value, not by position in the original vector.
##' @author David Whiting, david.whiting@@publichealth.me.uk
##' @keywords utils
##' 
##' @export
##' @examples
##' x <- 1:10
##' x
##' penultimate(x)
##' x <- 10:1
##' x
##' penultimate(x)

penultimate <- function(x){
  y <- sort(unique(x))
  if (length(y) > 1) {
    y <- y[length(y) - 1]
  } else {
    y <- y[length(y)]
  }
  y
}
