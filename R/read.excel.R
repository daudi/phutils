##' Read in an excel sheet
##' 
##' This reads in the first sheet from an excel file.
##' 
##' @param file The path to an excel file
##' @param keep.sheets See excelToCsv() DW TO USE templates FOR THIS BIT
##' @param ... Other parameters to be passed to read.csv()
##' 
##' @details This only works on Windows. By default it reads in the first sheet from an excel file. You can use 
##' keep.sheets to specify the name of a later sheet. See excelToCsv() for more details about how this works.
##' 
##' @export 
##' @seealso excelToCsv

read.excel <- function(file, keep.sheets = NULL, ...) {
  excelToCsv(file, keep.sheets = keep.sheets)
  x <- file.info(list.files(path = ".", pattern = ".*csv$"))
  csv.file <- rownames(x)[x$ctime == max(x$ctime)]
  x <- read.csv(csv.file, ...)  
  unlink(csv.file)
  x
}





