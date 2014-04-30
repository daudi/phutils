##' Simple reasonably random password generator
##' 
##' @param size The length of the password to be generated (default is 10)
##' @param lower.case Logical. Include lowercase letters? Default is TRUE.
##' @param upper.case Logical. Include uppercase letters? Default is TRUE.
##' @param numbers Logical. Include numbers? Default is TRUE.
##' @param symbols Logical. Include symboks (e.g. !"£$%^&* etc.) ? Default is TRUE.
##' 
##' @export 
##' @return A character string.


pw <- function(size = 10, lower.case = TRUE, upper.case = TRUE, numbers = TRUE, symbols = TRUE) {
  sym <- c("!", "$", "%", "^", "&", "*", "(", ")", "-", "_", "+", "=", "@", "~", "#", "<", ">", "?")
  x <- NULL
  
  if (lower.case)
    x <- c(x, letters)
  
  if (upper.case)
    x <- c(x, LETTERS)
  
  if (numbers)
    x <- c(x, 0:9)
    
  if (symbols)
    x <- c(x, sym)
  
  x <- sample(x, size = size, replace = TRUE)
  paste(x, collapse = "")
}
