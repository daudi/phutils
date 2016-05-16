##' Take characters from the left-hand side of a string
##' 
##' @param x A vector of character strings
##' @param out_length The number of characters starting from the left
##' @export
##' 

left <- function(x, out_length) {
  ret <- substring(x, 1, out_length)
  if (any(length(ret) < out_length))
    warning(paste("Some values are shorter than", out_length, "characters."))
  
  ret
}
