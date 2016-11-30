#' Confidence interval for a directly standardised rate
#' 
#' Confidence interval for a directly standardised rate using an approximation. Based on
#' Statistics with Confidence by Gardner and Altman (1989).
#' 
#' @param N A numeric vector of the number of individuals in each (age) group. 
#' @param x A numeric vector of the number of cases in each (age) group
#' @param n A numeric vector of the denominator in the study population in each (age) group.
#' @param alpha The significance level of the confidence intervals. The default is 0.05, i.e. 95\% confidence intervals.
#' 
#' 
#' @details This uses an approximation and assumes that the rates are small.
#' 
#' @references Statistics with confidence, Gardner and Altman (1989).
#' @author david.whiting@@publichealth.me.uk
#' @keywords utils
#' @seealso 
#' \code{\link{ci.mean}}, 
#' \code{\link{ci.proportion}}, 
#' \code{\link{ci.poisson}}
#' @return A named vector with three elements: lower CI, mean, upper CI
#' 
#' @export
#' @examples
#' # Example from page 62 of Statistics with Confidence
#' std.pop <- c(2773, 2556, 1113, 184)
#' cases <- c(4, 13, 8, 7)
#' n <- c(96, 237, 105, 32)
#' ci.standardised.rate(std.pop, cases, n) * 100
#' # For 90% confidence intervals:
#' ci.standardised.rate(std.pop, cases, n, alpha = 0.1) * 100


ci.standardised.rate <- function(N, x, n, alpha = 0.05) {
  qnorm.val <- qnorm(1 - alpha / 2)
  rate <- x / n
  SR <- sum(N * rate) / sum(N)
  SE <- sqrt(sum(N^2 * rate / n)) / sum(N)
  lower.ci <- SR - (qnorm.val * SE)
  upper.ci <- SR + (qnorm.val * SE)
  c(lower.CI = lower.ci,
    std.rate = SR,
    upper.CI = upper.ci)
}


