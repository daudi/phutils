##' describe a numeric vector
##' 
##' @param x A numeric vector
##' @param colname The name of the vector (optional)
##' @export
##' 
descr.numeric <- function(x, colname = NULL) {
  if (!is.null(colname)) {
    main <- paste("Histogram of", colname)
  } else {
    main <- paste("Histogram of x")
  }
  
  x_summary <- summary(x)
  x_NA_pc <- round(x_summary[names(x_summary) == "NA's"] / length(x) * 100, 1)
  if (length(x_NA_pc) == 0) {
    x_NA_pc <- 0
  }
  names(x_NA_pc) <- "% NA"
  x_summary <- c(x_summary, x_NA_pc)
  if (!all(is.na(x)))
    hist(x, main = main, xlab = colname)
  ret <- list(summary = x_summary)
  yy <- as.data.frame(ret$summary)
  names(yy) <- "Value"
  cat("\n\n")
  cat(paste(knitr::kable(yy), collapse = "\n"), "\n")
  invisible(ret)
}

