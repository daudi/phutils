### https://stat.ethz.ch/pipermail/r-help/2008-February/154897.html
### Use for SMR, e.g. HSMR.



##' Function to calculate lower and upper estimates based on the poisson
##' distribution.
##' 
##' This function is useful for calculating confidence intervals for a
##' standardised mortality ratio (SMR) e.g. HSMR.
##' 

##' 

##' 
##' @param k The number of observed events.
##' @param time
##' 

##' @param p
##' 

##' @param method
##' 

##' @return The lower and upper number of events.
##' @author Terry Therneau, Mayo Clinic
##' @references Therneau, T.
##' https://stat.ethz.ch/pipermail/r-help/2008-February/154897.html
##' @keywords utils
##' @examples
##' 
##' ### Example from Statistics with Confidence (1989), page 60.
##' 
##' observed <- 64
##' expected <- 45.6
##' 
##' SMR <- observed / expected * 100
##' limits <- cipoisson(observed) * 100
##' limits
##' ci.limits <- limits / expected 
##' ci.limits
##' 
##' ci.hsmr <- function(observed, expected) {
##'   SMR <- observed / expected * 100
##'   limits <- cipoisson(observed) * 100
##'   ci.limits <- limits / expected 
##'   list(smr = SMR, ci.limits = ci.limits)
##' }
##' 
##' 
##' @export
"cipoisson" <- 
  function(k, time = 1, p = 0.95, method = c("exact", "anscombe") ) {
    nn <- max(length(k), length(time), length(p))
    if(nn > 1) {
      k <- rep(k, length = nn)
      time <- rep(time, length = nn)
      p <- rep(p, length = nn)
    }
    p <- (1 - p)/2  #two sided
    
    method <- match.arg(method)
    if(method == "exact") {
      dummy1 <- ifelse(k == 0, 1, k)
      #avoid an error message of qgamma
      lower <- ifelse(k == 0, 0, qgamma(p, dummy1))
      upper <- qgamma(1 - p, k + 1)
    }
    else if(method == "anscombe") {
      # anscombe's method
      upper <- (sqrt(k + 7/8) - qnorm(p)/2)^2
      lower <- (sqrt(k - 1/8) + qnorm(p)/2)^2
    }
    else stop("Invalid method")
    if(nn == 1)
      c(lower = lower, upper = upper)/time
    else cbind(lower = lower, upper = upper)/time
  }

