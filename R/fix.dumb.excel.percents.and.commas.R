fix.dumb.excel.percents.and.commas <-
function(x, cols = 1:ncol(x)) {
    ## When Excel saves columns of numbers formatted as percents
    ## as csv files it stupidly saves % symbols in csv files.
    ## This function strips the % and makes the number numeric.

	## x: can be either a dataframe or a vector
    if (is.null(dim(x))) {
        x <- gsub("%", "", x)
		x <- gsub(",", "", x)
        x <- as.numeric(x)
    } else {
        for (i in cols) {
            if (is.factor(x[, i])) {
                if (any(grepl("%", levels(x[, i])))) {
                    x[, i] <- gsub("%", "", levels(x[, i]))
                }
				if (any(grepl(",", levels(x[, i])))) {
					x[, i] <- gsub(",", "", levels(x[, i]))
				}
				x[, i] <- as.numeric(as.character(x[, i]))
            }
        }
    }
    x
}
