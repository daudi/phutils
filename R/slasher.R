##' Fix slashes in paths copied in Windows
##' 
##' When you copy a path in Windows it has backslashes. These are no good in R
##' code where you need forward slashes. With this function you can copy a path,
##' paste it, and have it saved back to the windows clipboard (so you can paste
##' it) with the backslashes changed to forward slashes.
##' 
##' 
##' @export
##' @references http://stackoverflow.com/questions/1189759/expert-r-users-whats-in-your-rprofile/12703931#12703931
##' @author Tom http://stackoverflow.com/users/378085/tom

slasher <- function() {
  cat('Paste windows file path and hit RETURN twice')
  x <- scan(what = "")
  xa <- gsub('\\\\', '/', x)
  writeClipboard(paste(xa, collapse=" "))
  cat('Here is your de-windowsified path. (It is also on the clipboard.)n', xa)
}