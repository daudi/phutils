##' Read in an excel sheet
##' 
##' This reads in the first sheet from an excel file.
##' 
##' @param file The path to an excel file
##' @template keep.sheets
##' @param ... Other parameters to be passed to read.csv()
##' 
##' @details This only works on Windows. By default it reads in the first sheet from an excel file. You can use 
##' keep.sheets to specify the name of a later sheet. If this picks up the wrong sheet, specify the sheet you want
##' using keep.sheets. See excelToCsv() for more details about how this works.
##' 
##' @export 
##' @seealso \code{\link{excelToCsv}}, \code{\link{read.csv}}

read.excel <- function(file, keep.sheets = NULL, ...) {
  excelToCsv(file, keep.sheets = keep.sheets)
  x <- file.info(list.files(path = ".", pattern = ".*csv$"))
  csv.file <- rownames(x)[x$ctime == max(x$ctime)]
  x <- read.csv(csv.file, ...)  
  unlink(csv.file)
  x
}




