##' Calculate confidence intervals for a mean
##' 
##' This function uses the formula:
##' Mean CI = mean +/- critical value * standard deviation/square root of n observations 
##' 
##' @param x A numeric vector
##' @param p The percentage of the normal curve that you want to include. Conventionally 95% is often used (i.e. 0.95) and this is the default. Other values may be more appropriate, depending on the analysis.
##' @details 
##' 90% and 95% confidence intervals are used most often as they strike a balance between confidence and accuracy. For example, a 99.999% confidence interval sounds like it should be the best, but in order to get a confidence that high, we need to have a very wide interval. To say that we are 99.999% confident that the mean is in the interval (-100, 100) is not useful, but to say that we are 90% confident that it is in (- 5, 5) is much better.
##' 
##' @return A named vector with three elements: lower CI, mean, upper CI.
##' @export
##' @examples
##' x <- rnorm(1000)
##' ci.mean(x)
##' ci.mean(x,  p = 0.90)
##' ci.mean(x,  p = 0.99)

ci.mean <- function(x, p = 0.95){
  q <- qnorm(p + (1 - p) / 2)
  meanx <- mean(x)
  sdx <- sd(x)
  sqrtn <- sqrt(length(x))
  upperci <- meanx + (q * (sdx/sqrtn))
  lowerci <- meanx - (q * (sdx/sqrtn))
  c(lower.CI = lowerci, mean = meanx, upper.CI = upperci)
}
