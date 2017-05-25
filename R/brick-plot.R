##' Alternative to a barplot
##' 
##' This plots counts using stacked bricks. The maximum height (number of bricks) is 
##' set and spreads counts larger than this over multiple columns. 
##'
##'  
##' 
##' @details This is inspired by a plot I saw in The Economist (see references). 
##' It is not perfect and could do with some more work to make it prettier.
##' It 
##' is a better form of barchart when the values are integers and 
##' have very large and very small numbers.
##' 
##' This plots counts using stacked bricks. The maximum height (number of bricks) is 
##' set and spreads counts larger than this over multiple columns. 
##' 
##' NOTE: This is be slow with large counts, e.g. 10s of thousands because 
##' each brick is drawn individually. Also, with that many bricks they will be
##' so small that the individual bricks will not be visible.
##' 
##' NOTE: you can create a variable width barplot() with base graphics; see http://stackoverflow.com/questions/14591167/variable-width-bar-plot
##' 
##' @references Economist plot that inspired this approach http://cdn.static-economist.com/sites/default/files/images/2015/11/blogs/graphic-detail/20151121_woc004_3.png
##' @param x A vector of counts
##' @param labels A vector of labels for the counts. If x is a named vector 
##' and labels is NULL, the names are used. If x is not a named vector and 
##' labels is NULL, the counts are used.
##' @param max_height The maximum number of bricks in each column. Defaults 
##' to the square root of the sum of the counts.
##' @param brick_colours A vector of colours, one for each group of columns. 
##' @param axis_opts A list of parameters used by axis(). See ?axis
##' @param rect_opts A list of parameters used by rect() to create the bricks. See ?rect
##' @param ... Other parameters used by plot()
##' @return A dataframe with the plot values, labels and information 
##' used for plotting the blocks and axis labels.
##' 
##' @export
##' @author David Whiting david.whiting@@publichealth.me.uk
##' 
##' @examples 
##' x <- c(1, 34, 128, 10, 23, 34, 56, 123)
##' brick_plot(x, ylab = "", xlab = "", bty = "n", main = "Brick plot FTW!", axis_opts = list(tick = FALSE))
##' brick_plot(x, ylab = "", xlab = "", bty = "n", max_height = 15, main = "Brick plot FTW!", axis_opts = list(tick = FALSE))
##' brick_plot(x, ylab = "", xlab = "", bty = "n", main = "Brick plot FTW!", horizontal = TRUE,
##'            axis_opts = list(tick = FALSE))
##'            
##' ## Most commomn languages spoken in Medway, 2011
##' x <- c(94.8, 0.6, 0.6, 0.3, 0.3, 0.2, 0.2)  
##' names(x) <- c("English", "Polish", "Panjabi", "Slovak", "Bengali", "Lithuanian", "Russian")
##' x <- x * 10 # Scale up decimals to counts per 1,000
##' brick_plot(x, ylab = "", xlab = "", horizontal = TRUE, axis_opts = list(cex.axis = 0.7, tick = FALSE), bty = "n", main = "Most common languages spoken in Medway")
##' 
##' ## Now set the max height and remove the borders from the bricks
##' brick_plot(x, ylab = "", xlab = "", horizontal = TRUE, 
##' max_height = 80, 
##' axis_opts = list(cex.axis = 0.7, tick = FALSE), 
##' rect_opts = list(border = NA), bty = "n", 
##' main = "Most common languages spoken in Medway")
##' 
##' 
##' 
##' 
##' 



brick_plot <- function(x, labels = NULL, 
                       max_height = ceiling(sqrt(sum(x))), 
                       brick_colours = NULL, 
                       horizontal = FALSE,
                       axis_opts = list(),
                       rect_opts = list(),
                       ...) {

  if (is.null(labels) & !is.null(names(x))) labels <- names(x)
  if (is.null(labels) & is.null(names(x))) labels <- x
  
  num_cols <- ceiling(x / max_height)
  x <- data.frame(group = labels, count = x)
    
  x$num_cols <- num_cols
  tot_num_cols <- sum(x$num_cols)
  
  if (horizontal) {
    my_ylim <- c(0, tot_num_cols)
    my_xlim <- c(0, max_height)
  } else {
    my_xlim <- c(0, tot_num_cols)
    my_ylim <- c(0, max_height)
    my_xaxt <- "n"
  }
  
  
  plot(1, type = "n",
       xlim = my_xlim,
       ylim = my_ylim,
       las = 1,
       xaxt = "n",
       yaxt = "n",
       ...)
  
  if (is.null(brick_colours)) my_colours <- rainbow(nrow(x))
  
  current_col_number <- 1
  group_start_pos <- NULL
  group_end_pos <- NULL
  for (i in 1:nrow(x)) {
    this_colour <- my_colours[i]
    current_col_height <- 1
    for (j in 1:x$count[i]) {
      if (j == 1) group_start_pos <- c(group_start_pos, (current_col_number - 1))
      if (current_col_height > max_height) {
        current_col_height <- 1
        ## Next column for this group
        current_col_number <- current_col_number + 1
      }
      ## Work out the positions of the corners of the brick
      my_xleft <- current_col_number - 1
      my_ybottom <- current_col_height - 1
      my_xright <- current_col_number - 0.1
      my_ytop <- current_col_height - 0.1
      if (horizontal) {
        rect_opts$xleft <- my_ybottom
        rect_opts$ybottom <- my_xleft
        rect_opts$xright <- my_ytop
        rect_opts$ytop <- my_xright
      } else {
        rect_opts$xleft <- my_xleft
        rect_opts$ybottom <- my_ybottom
        rect_opts$xright <- my_xright
        rect_opts$ytop <- my_ytop
      }
      rect_opts$col<- this_colour
      ## Draw the brick
      do.call(rect, rect_opts)
      
      current_col_height <- current_col_height + 1
      if (j == x$count[i]) group_end_pos <- c(group_end_pos, (current_col_number - 0.1))
    }
    ## Next group
    current_col_number <- current_col_number + 1
  }
  
  group_mid_pos <- group_start_pos + (group_end_pos - group_start_pos) / 2
  
  x$group_mid_pos <- group_mid_pos
  x$group_start_pos <- group_start_pos
  x$group_end_pos <- group_end_pos
  
  if (!"side" %in% names(axis_opts)) axis_opts$side <- ifelse(horizontal, 2, 1)
  if (!"at" %in% names(axis_opts)) axis_opts$at <- group_mid_pos
  if (!"las" %in% names(axis_opts)) axis_opts$las <- 1
  if (!"labels" %in% names(axis_opts)) axis_opts$labels <- labels
  do.call(axis, axis_opts)
  
  invisible(list(data = x))
}


