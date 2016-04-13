##' Return the extension of a file name
##' 
##' 
##' This is a simple utility function that returns the extension from a file name.
##' 
##' @param x A file name (a character string)
##' @return A character string
##' @export
##' @examples 
##' 
##' file_extension("temp.txt")

file_extension <- function(x) {
  x <- unlist(strsplit(x, "\\."))
  x[length(x)]
}

