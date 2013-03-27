##' Create age labels based on a sequence.
##' 
##' Create hyphenated age labels based on a simple sequence, by specifying the
##' start, end and interval.
##' 
##' This function creates a simple set of age group labels. The interval is the
##' same for each age group. For more a flexible approach see age.groups()
##' 
##' The last age group is always terminated with a + to indicate an open range,
##' e.g. 65+
##' 
##' @param from An integer defining the start of the sequence.
##' @param to An integer defining the end of the sequence.
##' @param by An integer defining the sequence interval.
##' @return A character vector of hyphenated age groups.
##' @author David Whiting, dwhiting@@nhs.net
##' @seealso \code{\link{age.groups}}
##' @keywords utils
##' @examples
##' 
##' age.labels(0, 100, 10)
##' ## "0-9"   "10-19" "20-29" "30-39" "40-49" "50-59" "60-69" "70-79" "80-89"
##' ## "90-99" "100+" 
##' 
##' 
##' age.labels(0, 99, 10)
##' ## "0-9"   "10-19" "20-29" "30-39" "40-49" "50-59" "60-69" "70-79" "80-89"
##' ## "90+"  
##' 
##' 
##' 
##' @export
age.labels <-
function(from, to, by) {
	## Purpose: create age labels, e.g. 0-4, 5-9... 85+
	## from: the age to start from
	## to: the last *lower* age group, e.g. 85
	## by: the interval, e.g. 5 years.
	lower.age <- seq(from = from, to = to, by = by)
	upper.age <- lower.age + by - 1
	upper.age[length(upper.age)] <- "END"
	x <- paste(lower.age, "-", upper.age, sep = "")
	x <- gsub("-END", "+", x)
	x
}
