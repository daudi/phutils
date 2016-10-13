##' A 'long' view of a row of data
##' 
##' Sometimes it can be easier to read data in long, stacked way (like the \\G option in SQL).
##' This does that for dataframes. 
##' 
##' @details This shows the first row by default. Another row can be shown by specifying the row number.
##' @param x A dataframe
##' @param row The row number. Defaults to 1, to show the first row.
##' @export
##'


view_long <- function(x, row = 1) {
  x <- x[row, ]
  for (i in 1:ncol(x)) {
    cat(paste0("[", i, "] ", names(x)[i], ": ", x[1, i]), "\n")
  }
}
