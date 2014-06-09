##' Convert numbers to words for inclusion in text.

##' @param x A number in numeric format.
##' @param sentence.case If TRUE, capitalise the first letter. Default is FALSE. 
##'
##' @export


`numbers2words` <-
function(x,
         sentence.case = FALSE,
         billion=c("US", "UK"),
         and=if (billion == "US") "" else "and"){
  billion <- match.arg(billion)
  trim <- function(text){
    gsub("(^\ *)|((\ *|-|,\ zero|-zero)$)", "", text)
  }
  makeNumber <- function(x) as.numeric(paste(x, collapse=""))
  makeDigits <- function(x) strsplit(as.character(x), "")[[1]]
  helper <- function(x){
    negative <- x < 0
    x <- abs(x)
    digits <- makeDigits(x)
    nDigits <- length(digits)
    result <- if (nDigits == 1) as.vector(ones[digits])
    else if (nDigits == 2)
      if (x <= 19) as.vector(teens[digits[2]])
      else trim(paste(tens[digits[1]], "-", ones[digits[2]], sep=""))
    else if (nDigits == 3) {
      tail <- makeNumber(digits[2:3])
      if (tail == 0) paste(ones[digits[1]], "hundred")
      else trim(paste(ones[digits[1]], trim(paste("hundred", and)),
                      helper(tail)))
    }
    else {
      nSuffix <- ((nDigits + 2) %/% 3) - 1
      if (nSuffix > length(suffixes) || nDigits > 15)
        stop(paste(x, "is too large!"))
      pick <- 1:(nDigits - 3*nSuffix)
      trim(paste(helper(makeNumber(digits[pick])),
                 suffixes[nSuffix], helper(makeNumber(digits[-pick]))))
    }
    if (billion == "UK"){
      words <- strsplit(result, " ")[[1]]
      if (length(grep("million,", words)) > 1)
        result <- sub(" million, ", ", ", result)
    }
    if (negative) paste("minus", result) else result
  }
  opts <- options(scipen=100)
  on.exit(options(opts))
  ones <- c("zero", "one", "two", "three", "four", "five", "six", "seven",
            "eight", "nine")
  teens <- c("ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen",
             "sixteen", " seventeen", "eighteen", "nineteen")
  names(ones) <- names(teens) <- 0:9
  tens <- c("twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty",
            "ninety")
  names(tens) <- 2:9
  suffixes <- if (billion == "US")
    c("thousand,", "million,", "billion,", "trillion,")
  else
    c("thousand,", "million,", "thousand million,", "billion,")
  x <- round(x)
  if (length(x) > 1) x <- sapply(x, helper) else x <- helper(x)
  if (sentence.case) {
    x <- sub(substr(x, 1, 1), toupper(substr(x, 1, 1)), x)
  }
  x
}


