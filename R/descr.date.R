##' describe a date vector
##' 
##' @param x A date vector
##' @param colname The name of the vector (optional)
##' @export
##' 
descr.Date <- function(x, colname = NULL) {
  if (!is.null(colname)) {
    main <- paste("Histogram of", colname)
  } else {
    main <- paste("Histogram of x")
    colname <- "Var"
  }
    
  x_summary <- summary(x)  
  num_na <- sum(is.na(x))
  pc_na <- round(num_na / length(x) * 100, 1)
  
  x_range_days <- max(x, na.rm = TRUE) - min(x, na.rm = TRUE)
  if (x_range_days > 3600) {
    x_breaks <- "years"
  } else if (x_range_days > 1000) {
    x_breaks <- "quarters"
  } else if (x_range_days > 360) {
    x_breaks <- "weeks"
  } else if (x_range_days > 60) {
    x_breaks <- "weeks"
  } else {
    x_breaks <- "days"
  }
  if (!all(is.na(x)))
    hist(x, breaks = x_breaks, main = main, xlab = x_breaks)
  
  ret <- list(summary = x_summary, num_na = num_na, pc_na = pc_na)
  ## Print the output
  msg <- paste0("\n\nNAs: ", ret$num_na, " (", ret$pc_na, "%)\n")
  cat(msg)
  yys <- as.data.frame(as.character(ret$summary))
  colnames(yys) <- colname
  cat("\n\n")
  cat(paste(knitr::kable(yys), collapse = "\n"), "\n")
  invisible(ret)
}
