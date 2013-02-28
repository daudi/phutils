##' Function to write entries to a status log, with a timestamp.
##' 
##' This function simply writes lines to a status log file with a timestamp.
##' 
##' This function simply writes lines to a status log file with a timestamp.
##' 
##' @param x The message to be written to the status log.
##' @return None.
##' @author David Whiting, dwhiting@@nhs.net
##' @keywords utils
##' @examples
##' 
##' ##   x <- status.log("Finished calculating mortality rates.")
##' 
status.log <- function(x) {
  right.now <- strftime(Sys.time(), format = "%Y-%m-%d %H:%M:%S")
  cat(paste(right.now, x, "\n"), file = "STATUS.LOG", append = TRUE)
}
