##' A 'long' view of a row of data
##' 
##' Sometimes it can be easier to read data in long, stacked way (like the \\G option in mysql).
##' This does that for dataframes. 
##' 
##' @details This shows the first row by default. Another row can be shown by specifying the row number.
##' @param x A dataframe
##' @param max_rows The maximum number of rows to show. Defaults to 1. More than a handful is 
##' unlikely to be helpful.
##' @param col_order A vector specifying the column display order, e.g order(names(x)) will 
##' sort alphabetically. The default is NULL, which results in the natural column order being used
##' @author David Whiting, david.whiting@@publichealth.me.uk
##' @export
##'

view_long <- function(x, max_rows = 1, col_order = NULL) {
  cat(paste0("There are ", nrow(x), " row(s) in this dataframe"))
  if (!is.null(col_order)) {
    x <- x[1:max_rows, col_order]
  } else {
    x <- x[1:max_rows, ]
  }
  for (i in 1:nrow(x)) {
    cat(paste("\n-------- Row", i, "--------\n"))
    for (j in 1:ncol(x)) {
      cat(paste0("[", j, "] ", names(x)[j], ": ", x[i, j]), "\n")
    }
  }
}
