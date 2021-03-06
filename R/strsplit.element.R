##' Return a given element (by position) from a split string
##' 
##' @description strsplit() splits a string and returns a list. This functions wraps around
##' strsplit() to make it easy to extract the resulting list elements by position.
##' 
##' 
##' @param x The string (vector) to be split
##' @param split The string to use to split
##' @param element A number specifying the position of the element within each result
##' in the list to return from each split. Or the word "last" in which case the last element is 
##' returned regardless of the length of each split.
##' @param ... Other parameters passed to ???
##' 
##' @author David Whiting, david.whiting@@publichealth.me.uk
##' @export



strsplit.element <- function (x, split, element, ...) {
  xx <- strsplit(x, split)
  if (!is.numeric(element)) {
    element <- unlist(lapply(xx, length))
  } else {
    element <- rep(element, length(xx))
  }
  ret <- list()
  for (i in 1:length(xx)) {
    ret[[i]] <- xx[[i]][element[i]]
  }
  ret
}