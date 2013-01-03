penultimate <- function(x){
  y <- sort(unique(x))
  if (length(y) > 1) {
    y <- y[length(y) - 1]
  } else {
    y <- y[length(y)]
  }
  y
}