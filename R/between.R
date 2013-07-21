##' Determine if a value is between two other values
##' 
##' @details This function simplifies working out if a value is between two other values. The comparison is inclusive. 
##' It is simply (x >= y) & (x <= z) which is easier to read as between(x, y, z)
##' 
##' At the moment it is not very clever, so the y values need to be lower than the z values. Potentially it could 
##' be made smarter to handle situations where elements of y may be greater than elements of z and still return 
##' TRUE if x is between the two values.
##' 
##' @param x The value being tested. This can be a vector.
##' @param y The lower value.
##' @param z The upper value.
##' @export
##' 
##' @return TRUE or FALSE, possibly a vector or matrix of TRUE or FALSE, depending on the input values.
##' 
##' @examples 
##' between(3, 2, 4) # TRUE

between <- function(x, y, z) {
  (x >= y) & (x <= z)
}