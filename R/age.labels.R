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
