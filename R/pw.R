##' Simple reasonably random password generator

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
