##' Read in an excel sheet
##' 
##' This reads in the first sheet from an excel file.
##' 
##' @param file The path to an excel file
##' @template keep.sheets
##' @param fix.cols A list of columns to run through \code{\link{fix.dumb.excel.percents.and.commas}()}
##' @param rbind If TRUE use \code{do.call("rbind", x)} to combine the sheets. See details. Defaults to FALSE.
##' @param save.sheet.name If TRUE the name of the sheet will be added to the data.frame as a new column 
##' @param cols.to.keep A list of vectors that specifies (by number) which columns to keep from each sheet
##' @param ... Other parameters to be passed to \code{\link{read.csv}()}
##' 
##' 
##' @details This only works on Windows. By default it reads in all sheets from an excel file. You can use 
##' keep.sheets to specify the name of specific sheets to read. See \code{\link{excelToCsv()}} for more details about how this works.
##' 
##' Using \code{fix.cols} and/or \code{rbind} when reading in multiple sheets is only likely to 
##' work as expected if all of the sheets
##' have exactly the same structure. 
##' 
##' If \code{cols.to.keep} and \code{fix.cols} are specified, the numbering of the columns to fix using \code{fix.cols} 
##' applies before \code{cols.to.keep} is considered (i.e. before columns are dropped). \code{cols.to.keep} still reads in 
##' all columns initially, and then drops those that are not needed. As with standard R indexing, a negative number can 
##' be used to specify columns to drop.
##' 
##' Please note that \code{keep.sheets} drops sheets early in the function, before \code{cols.to.keep} 
##' and \code{fix.cols} are used, so the lists for \code{cols.to.keep} and \code{fix.cols} do not need to consider
##' these sheets.
##' 
##' 
##' @return If \code{keep.sheets} contains the name of more than one sheet 
##' the function returns a list of dataframes. If there is only one sheet 
##' it is returned as a data.frame.
##' 
##' @export 
##' @seealso \code{\link{excelToCsv}}, \code{\link{read.csv}},
##' \code{\link{fix.dumb.excel.percents.and.commas}}

read.excel <- function(file, keep.sheets = NULL, fix.cols = NULL, rbind = FALSE, save.sheet.name = FALSE, 
                       cols.to.keep = NULL, ...) {
  tmp <- tempfile()
  dir.create(tmp)
  excelToCsv(file, keep.sheets = keep.sheets, target.dir = tmp)
  x <- file.info(list.files(path = tmp, pattern = ".*csv$"))
  csv.files <- rownames(x)
  if (!is.null(cols.to.keep))
    stopifnot(length(csv.files) == length(cols.to.keep))
    
  x <- list()
  i <- 0
  for (thisfile in csv.files) {
    i <- i + 1
    sheet.name <- gsub("(.*).csv$", "\\1", thisfile)
    y <- read.csv(file.path(tmp, thisfile), ...)
    if (!is.null(fix.cols))
      y <- fix.dumb.excel.percents.and.commas(y, cols = fix.cols[[i]])
    if (!is.null(cols.to.keep))
      y <- y[, cols.to.keep[[i]]]
    
    if (save.sheet.name)
      y$sheet.name <- sheet.name
      
    x[[sheet.name]] <- y
    
    unlink(file.path(tmp, thisfile))
  }
  if (length(x) == 1)
    x <- x[[1]]

  if (rbind)
    x <- do.call("rbind", x)
  x
}





