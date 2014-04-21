##' Function to create age groups
##' 
##' A reasonably flexible way to create formatted age groups from a sequence of
##' age breaks.
##' 
##' If you want your last open age group to be 65+ specify your breaks
##' something like c(0, 19, 65, 1000), i.e. the age group that will be
##' "plussed" should be the penultimate value and the last value should be
##' large enough to include all ages.
##' 
##' @param x Age in (integer) years.
##' @param y A vector of age breaks, e.g. using the seq() function.
##' @param final.open If TRUE (the default) the last age group will be open,
##' e.g 85+ but this can be changed.
##' @param ordered_result logical: should the result be an ordered factor? Default is TRUE.
##' @return A factor with the level that corresponds to the age and age ranges
##' provided.
##' @author David Whiting, dwhiting@@nhs.net
##' @seealso 
##' \code{\link{seq}}, 
##' \code{\link{age.labels}}, 
##' \code{\link{lapply}}, 
##' \code{\link{cut}}
##' @examples
##' 
##' age.groups(5, c(0, 5, 10)) # 5+
##' age.groups(15, c(0, 5, 10, 15, 20, 25)) # 15-19
##' 
##' ## Using a sequence of breaks
##' age.breaks <- seq(from = 0, to = 100, by = 5)
##' age.groups(15, age.breaks) # 15-19
##' age.groups(23, age.breaks) # 20-24
##' age.groups(96, age.breaks) # 95+
##' age.groups(96, age.breaks, final.open = FALSE) # 95-99
##' age.groups(100, age.breaks) # 95+
##' age.groups(101, age.breaks) # NA
##' 
##' age.groups(95:100, age.breaks, final.open = TRUE)
##' age.groups(95:100, age.breaks, final.open = FALSE)
##' 
##' 
##' @export

age.groups <-
function(x, y, final.open = TRUE, ordered_result = TRUE) {
	xx <- cut(x, breaks = y, include.lowest = TRUE, right = FALSE, ordered_result = ordered_result)
	levels(xx) <- gsub("\\(", "", levels(xx))
	levels(xx) <- gsub("\\)", "", levels(xx))
	levels(xx) <- gsub("\\[", "", levels(xx))
	levels(xx) <- gsub("\\]", "", levels(xx))
	x1 <- as.numeric(gsub("([0-9]+),.*", "\\1", levels(xx)))
	x2 <- as.numeric(gsub(".*,([0-9]+)", "\\1", levels(xx)))
	x2 <- x2 - 1
	## If any x > the end of the interval and the final is not open, make it NA
	if (!final.open) 
	  xx[x > x2[length(x2)]] <- NA
	
	levels(xx) <- paste(x1, "-", x2, sep = "")
	if (final.open) 
    levels(xx)[nlevels(xx)] <- gsub("-[0-9]+", "\\+", levels(xx)[nlevels(xx)])
  
  xx
}
