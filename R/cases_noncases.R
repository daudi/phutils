##' Function to approximate sensible numerators and denominators for a proportion
##' 
##' This is for approximating the number of images needed for an infographic
##' showing the number of people affected by something. The idea is to use this 
##' to put an appropriate number of images in the "right place" in a powerpoint 
##' slide.
##' 
##' There's probably a nice mathematical solution to this, but as I don't know what it 
##' is I'm using a brute-force approach.
##' 
##' @param x A decimal value
##' @param denoms A vector of denominators to consider when looking for fractions
##' @param units The name of the units in the proportion graphic. The default is people.
##' This is used to construct a message that can be used to describe the graphic.
##' @author David Whiting, david.whiting@@publichealth.me.uk

cases_noncases <- function(x, denoms = 1:20, units = "people") {
  
  ## Function to calculate each of the proportions for the range of 
  ## values for a given denominator
  calc_props <- function(this_denom) {
    numers <- 1:this_denom
    props <- numers / this_denom
    ret <- data.frame(numer = numers, denom = this_denom, prop = props)
    ret
  }
  
  ret <- lapply(denoms, calc_props)
  ret <- do.call("rbind", ret)
  ## Remove duplicate proportions, keeping the one with the smallest denominator
  ret <- ret[order(ret$prop, ret$denom), ]
  ret <- ret[!duplicated(ret$prop), ]
  
  ## Find the proportion that is closest to the one we want to display
  xdiff <- abs(ret$prop - x)
  i <- which(xdiff == min(xdiff))
  msg <- paste(ret$numer[i], "out of", ret$denom[i], units)
  
  ## Work out how many should be in the first and most subsequent 
  ## rows to get a roughly square shape
  full_row_size <- ceiling(sqrt(ret$denom[i]))
  num_full_rows <- floor(ret$denom[i] / full_row_size)
  final_row <- ret$denom[i] - (full_row_size * num_full_rows)
  
  list(numerator = ret$numer[i],
       denominator = ret$denom[i],
       proportion = ret$prop[i],
       msg = msg,
       full_row_size = full_row_size,
       num_full_rows = num_full_rows,
       final_row = final_row)
  
}