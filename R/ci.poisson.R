##' Function to calculate lower and upper estimates based on the poisson
##' distribution.
##' 
##' Useful for rare count data.
##' 
##' @aliases cipoisson ci.poisson
##' 
##' @param observed The number of observed events.
##' @param time 
##' @param p The percentile of the normal distribution (i.e. 0.95 = 95\% CIs)
##' @param method exact or anscombe
##' @author David Whiting, david.whiting@@publichealth.me.uk
##' 

##' @return The lower and upper number of events.
##' @author Terry Therneau, Mayo Clinic
##' @references Therneau, T.
##' https://stat.ethz.ch/pipermail/r-help/2008-February/154897.html
##' @keywords utils
##' @seealso 
##' \code{\link{ci.mean}}, 
##' \code{\link{ci.proportion}}, 
##' \code{\link{ci.standardised.ratio}}, 
##' \code{\link{ci.standardised.rate}}
##' @examples
##' 
##' ### Example from Statistics with Confidence (1989), page 60.
##' 
##' observed <- 64
##' ci.poisson(observed)
##' 
##' 
##' @export
"ci.poisson" <- 
  function(observed, time = 1, p = 0.95, method = c("exact", "anscombe") ) {
    nn <- max(length(observed), length(time), length(p))
    if(nn > 1) {
      observed <- rep(observed, length = nn)
      time <- rep(time, length = nn)
      p <- rep(p, length = nn)
    }
    p <- (1 - p)/2  #two sided
    
    method <- match.arg(method)
    if(method == "exact") {
      dummy1 <- ifelse(observed == 0, 1, observed)
      #avoid an error message of qgamma
      lower <- ifelse(observed == 0, 0, qgamma(p, dummy1))
      upper <- qgamma(1 - p, observed + 1)
    }
    else if(method == "anscombe") {
      # anscombe's method
      upper <- (sqrt(observed + 7/8) - qnorm(p)/2)^2
      lower <- (sqrt(observed - 1/8) + qnorm(p)/2)^2
    }
    else stop("Invalid method")
    
    if(nn == 1) 
      res <- c(lower.CI = lower, count = observed, upper.CI = upper)/time
    else 
      res <- cbind(lower.CI = lower, count = observed, upper.CI = upper)/time
    
    res
  }


cipoisson <- ci.poisson