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
