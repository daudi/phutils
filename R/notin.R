##' The evil twin of `\%in\%`
##'
##' In a way it is astonishing that this does not already exist in base R. 
##' It is much easier to do `5 \%notin\% 1:4` than it is to do (and read)
##' `!(5 \%in\% 1:4)`
##' 
##' 
##' @param x A vector of values to look for.
##' @param table The object in which to look
##' @usage x \%notin\% y
##' @rdname notin
##' @export
##' @references I think it came from here: http://stackoverflow.com/questions/5831794/opposite-of-in 
##' @return A vector of the same length as x
##' @examples 
##' 4:6 %notin% 1:5

`%notin%` <- function (x, table) is.na(match(x, table, nomatch=NA_integer_))
