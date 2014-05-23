##' Read in an excel sheet
##' 
##' This reads in the first sheet from an excel file.
##' 
##' @param file The path to an excel file
##' @template keep.sheets
##' @param fix.cols The columns to run through \code{\link{fix.dumb.excel.percents.and.commas()}}
##' @param rbind If TRUE use do.call("rbind", x) to combine the sheets. See details. Defaults to FALSE.
##' @param ... Other parameters to be passed to read.csv()
##' 
##' @details This only works on Windows. By default it reads in the first sheet from an excel file. You can use 
##' keep.sheets to specify the name of a later sheet. If this picks up the wrong sheet, specify the sheet you want
##' using keep.sheets. See \code{\link{excelToCsv()}} for more details about how this works.
##' 
##' Using fix.cols and/or rbind when reading in multiple sheets is only likely to 
##' work as expected if all of the sheets
##' have exactly the same structure. 
##' 
##' @return If keep.sheets contains the name of more than one sheet 
##' the function returns a list of dataframes. If there is only one sheet 
##' it is returned as a data.frame.
##' 
##' @export 
##' @seealso \code{\link{excelToCsv}}, \code{\link{read.csv}},
##' \code{\link{fix.dumb.excel.percents.and.commas}}

read.excel <- function(file, keep.sheets = NULL, fix.cols = NULL, rbind = FALSE, ...) {
  excelToCsv(file, keep.sheets = keep.sheets)
  x <- file.info(list.files(path = ".", pattern = ".*csv$"))
  csv.file <- rownames(x)
  if (is.null(keep.sheets)) {
    csv.file <- csv.file[x$ctime == max(x$ctime)]
  }
  x <- list()
  for (i in 1:length(csv.file)) {
    y <- read.csv(csv.file[i], ...)  
    if (!is.null(fix.cols))
      y <- fix.dumb.excel.percents.and.commas(y, cols = fix.cols)
    
    x[[i]] <- y
    
    unlink(csv.file[i])
  }
  if (length(x) == 1)
    x <- x[[1]]

  if (rbind)
    x <- do.call("rbind", x)
  x
}





