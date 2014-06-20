##' Function to calculate confidence intervals around a standardised ratio
##' 
##' This function is useful for calculating confidence intervals for a
##' standardised mortality ratio (SMR) e.g. HSMR.
##' 
##' @param observed The number of observed events.
##' @param time 
##' @param p The percentile of the normal distribution (i.e. 0.95 = 95\% CIs)
##' @param method exact or anscombe
##' 

##' @return The lower and upper number of events.
##' @author Terry Therneau, Mayo Clinic
##' @references Therneau, T.
##' https://stat.ethz.ch/pipermail/r-help/2008-February/154897.html
##' @keywords utils
##' @seealso 
##' \code{\link{ci.mean}}, 
##' \code{\link{ci.proportion}}, 
##' \code{\link{ci.poisson}}, 
##' \code{\link{ci.standardised.rate}}
##' @examples
##' 
##' ### Example from Statistics with Confidence (1989), page 60.
##' 
##' observed <- 64
##' expected <- 45.6
##' ci.standardised.ratio(observed, expected)
##' 
##' 
##' 
##' @export
"ci.standardised.ratio" <- 
  function(observed, expected, time = 1, p = 0.95, method = c("exact", "anscombe") ) {
    res <- ci.poisson(observed, time, p, method)
    res <- c(lower.CI = res[['lower.CI']] / expected, SR = res[['count']] / expected, upper.CI = res[['upper.CI']] / expected)
        
    res
  }

