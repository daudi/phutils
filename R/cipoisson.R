### https://stat.ethz.ch/pipermail/r-help/2008-February/154897.html
### Use for SMR, e.g. HSMR.

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

