age.groups <-
function(x, y, final.open = TRUE) {
	## x: age in years (must be integers I think)
	## y: a vector of breaks, e.g. seq(from = 0, to = 100, by = 5) or c(0, 14, 24, 100)
	## By default it will leave the last group open, e.g 85+ but this can be changed
	## by specifying final.open = FALSE
	xx <- cut(x, breaks = y, include.lowest = TRUE, right = FALSE)
	levels(xx) <- gsub("\\(", "", levels(xx))
	levels(xx) <- gsub("\\)", "", levels(xx))
	levels(xx) <- gsub("\\[", "", levels(xx))
	levels(xx) <- gsub("\\]", "", levels(xx))
	x1 <- as.numeric(gsub("([0-9]+),.*", "\\1", levels(xx)))
	x2 <- as.numeric(gsub(".*,([0-9]+)", "\\1", levels(xx)))
	x2 <- x2 - 1
	levels(xx) <- paste(x1, "-", x2, sep = "")
	if (final.open) levels(xx)[nlevels(xx)] <- gsub("-[0-9]+", "\\+", levels(xx)[nlevels(xx)])
	## TODO: ADD CODE TO MAKE THIS AN ORDERED FACTOR
	xx
}
