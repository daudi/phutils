##' Calculate confidence intervals for a simple proportion
##' 
##' Currently only able to do 95\% confidence intervals. It would not be tricky to 
##' extend this function to allow other intervals.
##' 
##' @param x The numerator in a percentage calculation.
##' @param n The denominator in a percentage calculation. 
##' 
##' @return A named numeric vector with the lower confidence interval, 
##' point estimate and upper confidence interval.
##' @author dwhiting@@nhs.net
##' @keywords stats
##' @examples 
##' ci.proportion(3, 20) 
##' 
##' @export

ci.proportion <- function(x, n){
  p <- x / n
  q <- 1 - p
  ci <- 1.96 * sqrt((p * q) / n)
  p.lower <- p - ci
  p.upper <- p + ci
  c(p.lower = p.lower,
    p = p,
    p.upper = p.upper)
}

