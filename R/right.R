##' Take characters from the right-hand side of a string
##' 
##' @param x A vector of character strings
##' @param out_length The number of characters starting from the right
##' @author David Whiting, david.whiting@@publichealth.me.uk
##' @export
##' 

right <- function(x, out_length) {
  ret <- substring(x, nchar(x) - out_length + 1)
  if (any(length(ret) < out_length))
    warning(paste("Some values are shorter than", out_length, "characters."))
  
  ret
}
