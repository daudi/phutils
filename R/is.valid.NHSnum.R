##' Validate NHS Numbers using the Modulus 11 algorithm
##' 
##' Validate NHS Numbers using the Modulus 11 algorithm.
##' 
##' From the NHS Data Dictionary:
##' 
##' The NHS NUMBER is 10 numeric digits in length. The tenth digit is a check 
##' digit used to confirm its validity. The check digit is validated using the 
##' Modulus 11 algorithm and the use of this algorithm is mandatory. There are 5 
##' steps in the validation of the check digit:
##' 
##' Step 1 Multiply each of the first nine digits by a weighting factor as 
##' follows: Dig1 X 10, Dig2 x 9, Dig3 x 8, ... Dig9 x 2
##' 
##' Step 2 Add the results of each multiplication together.
##' 
##' Step 3 Divide the total by 11 and establish the remainder.
##' 
##' Step 4 Subtract the remainder from 11 to give the check digit.
##' 
##' If the result is 11 then a check digit of 0 is used. If the result is 10 then 
##' the NHS NUMBER is invalid and not used.
##' 
##' Step 5 Check the remainder matches the check digit. If it does not, the NHS 
##' NUMBER is invalid.
##' 
##' 
##' @param x A vector of NHS numbers.
##' 
##' @return A logical vector of the same length as x. 
##' @author Mark Chambers mark.chambers@@nhs.net and dwhiting@@nhs.net
##' @keywords utils
##' @examples 
##' # Create some dummy data
##' x <- data.frame(NHSnum = c("temp12 1zzz", "712.457 766", "5265 784454", 2222222222, 4146515092, 4446394339, 4380590096, 3459124359))
##' is.valid.NHSnum(x)
##' @export


is.valid.NHSnum <- function(x){
  # Where x is a vector of NHS numbers

  # Create a function to split NHS number into first 9 digits and check digit
  check.sum <- function(x) {
    x <- as.character(x)
    dig1 <- as.numeric(substr(x,1,1))
    dig2 <- as.numeric(substr(x,2,2))
    dig3 <- as.numeric(substr(x,3,3))
    dig4 <- as.numeric(substr(x,4,4))
    dig5 <- as.numeric(substr(x,5,5))
    dig6 <- as.numeric(substr(x,6,6))
    dig7 <- as.numeric(substr(x,7,7))
    dig8 <- as.numeric(substr(x,8,8))
    dig9 <- as.numeric(substr(x,9,9))
    check <- as.numeric(substr(x,10,10))
    
    sum.product <- (dig1 * 10) + (dig2 * 9) + (dig3 * 8) + (dig4 * 7) + 
      (dig5 * 6) + (dig6 * 5) + (dig7 * 4) + (dig8 * 3) + (dig9 * 2)
    
    remainder <- 11 - (sum.product%%11)
    
    j <- remainder == check | (remainder == 11 & check == 0)
    x <- rep(FALSE, length(x))
    x[j] <- TRUE
    x
  }
  
  ## Start the work here.
  # Remove spaces from number (also converts to character)
  x <- gsub(" ", "", x)
  
  # Identify obviously invalid NHS numbers (not 10 characters in length, 
  # contains non-numeric, first number repeated 9 times)
  i <- nchar(as.character(x)) != 10 | grepl("[^0-9]", x) | grepl("(\\d+?)\\1{9}", x)
  x[i] <- FALSE
  
  x[!i] <- check.sum(x[!i])
  x
}
