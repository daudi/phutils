##' Print a status message and log it with a timestamp
##' 
##' @param msg The status message
##' @param append If TRUE (the default) append to the existing log file. Usually, therefore, you will want to set this to FALSE the first time you call it in your program.
##' @param file The name of the log file. Default is "STATUS.LOG"
##' @return None.
##' @author David Whiting, david.whiting@@publichealth.me.uk
##' @keywords utils
##' @examples
##' 
##' x <- status.log("Finished calculating mortality rates.")
##' readLines("STATUS.LOG")
##' 
##' @export

status.log <- function(msg, append = TRUE, file = LOGFILE) {  
  if (!exists("LOGFILE")) file <- "STATUS.LOG"
  print(msg)
  msg <- paste0("[", Sys.time(), "] ", msg, "\n")
  cat(msg, file = file, append = append)
}