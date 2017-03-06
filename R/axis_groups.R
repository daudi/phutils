##' Create a two-level grouped axis
##' 
##' This is especially useful when ploting dates or quarters where you 
##' want the quarter on one line and the year on another.
##' 
##' @param x The x-axis plotting variable
##' @param groups A vector of groups the same length as the vector of x values
##' @param subgroups A vector of sub-groups the same length as the vector of 
##' x values
##' @param subgroup_opts A list of parameters or options that can be used by 
##' axis() for formatting the subgroup labels
##' @param group_opts A list of parameters or options that can be used by 
##' axis() for formatting the group labels
##' @param group_div_opts A list of parameters or options that can be used by 
##' axis() for formatting the tick marks between the main groups
##' 
##' @details Probably the simplest way to make sure that you get the data 
##' right for this is to add new columns to a dataframe with the group and 
##' subgroup labels for each row. 
##' 
##' Make sure that you order the data correctly, i.e. by the values that are 
##' plotted on the x-axis, e.g. your dates.
##' 
##' @author David Whiting david.whiting@@publichealth.me.uk
##' 
##' @export
##'  
##' 
##' @examples 
##' ## Create some data
##' ff <- data.frame(x = rep(LETTERS[1:10], 3), y = rep(LETTERS[1:3], 10), val = runif(n = 30))
##' ff <- ff[order(ff$y, ff$x), ]
##' ff$z <- 1:30
##' ff
##' 
##' plot(ff$z, ff$val, xlab = "", xaxt = "n")
##' axis_groups(ff$z, ff$y, ff$x) # Default grouped axis
##' 
##' ## Simulate year-quarter data
##' ff <- data.frame(x = rep(c("Q1", "Q2", "Q3", "Q4"), 3), 
##'                  y = rep(c("2014/15", "2015/16", "2016/17"), 4), val = runif(n = 12))
##' ff <- ff[order(ff$y, ff$x), ]
##' ff$z <- 1:12
##' ff
##' plot(ff$z, ff$val, xlab = "", xaxt = "n") # Default grouped axis
##' axis_groups(ff$z, ff$y, ff$x)
##' 
##' plot(ff$z, ff$val, xlab = "", xaxt = "n")
##' ## Make the group divider red
##' axis_groups(ff$z, ff$y, ff$x, group_div_opts = list(col = "red")) 
##' 
##' plot(ff$z, ff$val, xlab = "", xaxt = "n")
##' ## Change the font of the main groups
##' axis_groups(ff$z, ff$y, ff$x, group_opts = list(cex.axis = 3, col.axis = "green"))
##' 
##' plot(ff$z, ff$val, xlab = "", xaxt = "n")
##' ## Change the fonts of the main groups and subgroups
##' axis_groups(ff$z, ff$y, ff$x, 
##'             group_opts = list(cex.axis = 3, col.axis = "green"),
##'             subgroup_opts = list(cex.axis = 0.8, col.axis = "blue"))
##' 



axis_groups <- function(x, groups, subgroups, 
                         subgroup_opts = list(),
                         group_opts = list(),
                         group_div_opts = list()
                         ) {
  
  ## Sub-groups
  if (!"side" %in% names(subgroup_opts)) subgroup_opts$side <- 1
  if (!"x" %in% names(subgroup_opts)) subgroup_opts$at <- x
  if (!"labels" %in% names(subgroup_opts)) subgroup_opts$labels <- subgroups
  if (!"tick" %in% names(subgroup_opts)) subgroup_opts$tick <- FALSE
  do.call(axis, subgroup_opts)
  
  ## Work out the midpoints of the groups.
  grps <- table(groups)
  lastidx <- cumsum(grps)
  firstidx <- lastidx - grps + 1
  grppos <- x[firstidx] + (x[lastidx] - x[firstidx]) / 2
  
  if (!"side" %in% names(group_opts)) group_opts$side <- 1
  if (!"x" %in% names(group_opts)) group_opts$at <- grppos
  if (!"labels" %in% names(group_opts)) group_opts$labels <- names(grps)
  if (!"tick" %in% names(group_opts)) group_opts$tick <- FALSE
  if (!"padj" %in% names(group_opts)) group_opts$padj <- 2
  do.call(axis, group_opts)
  
  ## Work out the points between the groups for the tickmarks
  nextidx <- lastidx + 1
  tck_pos <- x[lastidx] + (x[nextidx] - x[lastidx]) / 2
  
  if (!"side" %in% names(group_div_opts)) group_div_opts$side <- 1
  if (!"x" %in% names(group_div_opts)) group_div_opts$at <- tck_pos
  if (!"labels" %in% names(group_div_opts)) group_div_opts$labels <- FALSE
  if (!"tck" %in% names(group_div_opts)) group_div_opts$tck <- -0.10
  if (!"lty" %in% names(group_div_opts)) group_div_opts$lty <- "dashed"
  if (!"lwd" %in% names(group_div_opts)) group_div_opts$lwd <- 0
  if (!"lwd.ticks" %in% names(group_div_opts)) group_div_opts$lwd.ticks <- 1
  do.call(axis, group_div_opts)
  
  invisible(list(firstidx = firstidx, lastidx = lastidx,
                 grppos = grppos, 
                 nextidx = nextidx,
                 tck_pos = tck_pos))
}



