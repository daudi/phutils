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

