splice <- function (x, y) {
  stopifnot(ncol(x) == ncol(y))
  stopifnot(nrow(x) == nrow(y))
  z <- data.frame(x, y)
  cols <- as.vector(t(matrix(1:ncol(z), ncol(z) / 2)))
  z[, cols]
}
