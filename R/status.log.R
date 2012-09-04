status.log <- function(x) {
  right.now <- strftime(Sys.time(), format = "%Y-%m-%d %H:%M:%S")
  cat(paste(right.now, x, "\n"), file = "STATUS.LOG", append = TRUE)
}
