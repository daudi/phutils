##' A function to clean up CSV exports from excel.
##' 
##' Remove percent symbols and commas from data.frames. This is primarily to
##' clean up CSV files saved by Excel with percent symbols or commas.
##' 
##' When excel saves CSV files any numeric columns with \% formatting or commas
##' as thousand separators will be saved with the percents and commas. This is
##' really dumb. When such files are read into R these columns are interpreted
##' as having character and not numeric data.
##' 
##' This function will look for such columns and try to convert them to
##' numeric.
##' 
##' @param x Can be either a dataframe or a vector.
##' @param cols A vector of columns to process. Use this to avoid processing
##' columns that are legitimate factors. The default is 1:ncol(x)
##' @return A data.frame or vector (depending on what we supplied to the
##' function).
##' @author David Whiting, dwhiting@@nhs.net
##' @keywords utils
fix.dumb.excel.percents.and.commas <-
    function(x, cols = 1:ncol(x)) {
        ## When Excel saves columns of numbers formatted as percents
        ## as csv files it stupidly saves % symbols in csv files.
        ## This function strips the % and makes the number numeric.

	fix.percents <- function(x) {
            x <- gsub("%", "", x)
	}
	fix.commas <- function(x) {
            x <- gsub(",", "", x)
	}
        if (is.null(dim(x))) {
            x <- fix.percents(x)
            x <- fix.commas(x)
            x <- as.numeric(x)
        } else {
            for (i in cols) {
                ## If excel has done this then the data will have been read in as factors
                ## so we only need to worry about columns that contain factors.
                if (is.factor(x[, i])) {
                    if (any(grepl("%", levels(x[, i])))) {
                        x[, i] <- fix.percents(x[, i])
                    }
                    if (any(grepl(",", levels(x[, i])))) {
                        x[, i] <- fix.commas(x[, i])
                    }
                    x[, i] <- as.numeric(x[, i])
                }
            }
        }
        x
    }
