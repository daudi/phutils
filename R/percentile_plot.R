##' Create a Percentile plot

##' @description A percentile plot is a means of simultaneously displaying the
##' summary statistics of multiple numeric variables whilst highlighting the 
##' position of a particular element within the distribution of each variable. 

##' @details  Values are scaled so that the mean is centrered in the plotting 
##' window. This is a similar methodology as used by Public Health England in 
##' the area profiles section of 'Finger tips' tools 
##' (http://www.phoutcomes.info/). 
##' 
##' 'Good performance' is typified by being to 
##' the right of the central line so where good performance is associated with 
##' a low value, e.g. smoking prevalence, it is important to ensure this is
##' reflected in the lowIsGood parameter.
##' 
##' Within the function, margin settings change automatically to fit 
##' longest individual label on single line. Shorter labels are preferential to 
##' avoid the plotting window becoming too condensed. Within an Rmd file, labels 
##' can be followed by a sequential numbering system with a key below to allow a 
##' fuller description.

##' @param data data frame or matrix of numeric and/or integer variables
##' @param pick element or row of data to highlight with dot overlaid on bars. 
##' Must be an integer between 1 and nrow(data). Default is 1.
##' @param additionalProbs pair of values at which to draw additional vertical 
##' lines on each bar. Must be a numeric vector of length two where values are 
##' greater than zero and less than one. Default is c(0.05, 0.95).
##' @param lowIsGood boolean vector related to variable plotting order 
##' indicating whether good performance is associated with a low value. If TRUE, 
##' the scale is inverted so that low values appear on the right and high values
##' on the left of the plot. Default is FALSE
##' @param localValueCex size of the dot. Default is 20 divided by number of 
##' variables in data which works as a rough rule of thumb.
##' @param plotLabels character vector of labels to insert to the left of the
##' plot. Default is colnames of data.
##' @param cexLabels size of the labels on left-hand side of plot. Default is 1.
##' @param plotTitle the title given to the plot. This is placed towards the 
##' left-hand corner of the window.
##' @param horizLegend Whether to display the legend horizontally. Default is 
##' TRUE.
##' @param ncolLegend Number of columns used in legend. Default is 1 but is 
##' overidden if horizLegend is set to TRUE.
##' @param xInterspLegend character interspacing factor for horizontal legend. 
##' Default is 1.
##' @param upperMar top margin height. Default is 5.

##' @author Mark Chambers mark.chambers@@medway.gov.uk
##' @reference 
##' @export
##' @return Does not return anything at the moment. Creates a plot as a side-effect.
##' @examples 
##' attach(mtcars)
##' x <- mtcars$wt
##' y <- mtcars$mpg
##' 
##' df1 <- data.frame(var1 = x, var2 = y)
##' df2 <- data.frame(var1 = x, var2 = y, var3 = 1:length(wt), var4 = x, abcdefghijklmno = mpg)
##' df3 <- data.frame(var1 = x, var2 = y, var3 = 1:length(wt), var4 = x, var5 = y, var6 = x, var7 = y, var8 = 1:length(x), var9 = x, abcdefghijklmno = y)
##' 
##' percentile_plot(df3, pick = 5, lowIsGood = c(rep(FALSE, 9), TRUE))
##'
##' percentile_plot(df1, pick = 5)



percentile_plot <- function(data, 
                            pick = 1, 
                            additionalProbs = c(0.05, 0.95), 
                            lowIsGood = rep(FALSE, ncol(data)), 
                            localValueCex = (20/ncol(data)),
                            plotLabels = colnames(data),
                            cexLabels = 1,
                            plotTitle = "Percentile plot",
                            horizLegend = TRUE,
                            ncolLegend = 1,
                            xInterspLegend = 1,
                            upperMar = 5) {
  
  # Data traps
  # pick must be an integer between 1 and nrow(data)
  stopifnot(pick > 0 & pick <= nrow(data))
  # x must be a matrix or dataframe
  stopifnot(class(data) %in% c("matrix", "data.frame"))
  # Additional probabilities must be greater than zero and less than 1
  stopifnot(additionalProbs > 0 & additionalProbs < 1)
  stopifnot(length(additionalProbs) == 2)
  
  # Store number of variables to setup empty plot and cycle through in for loop
  numVariables <- ncol(data)
  
  # Determine necessary left margin par settings to accommodate longer labels
  leftMar <- ceiling(max(nchar(plotLabels), na.rm = TRUE) / (1.5 / cexLabels))
  
  # Setup an empty plotting window
  op <- par(mar = c(5, leftMar, upperMar, 2) + 0.1, xpd = TRUE)
  
  plot(c(0, 1), c(0, numVariables), 
       type = "n", 
       xlim = c(-0.1, 1.1), 
       xaxt = "n",
       yaxt = "n",
       xlab = "",
       ylab = "",
       bty = "n")
  
  axis(side = 2, 
       labels = plotLabels, 
       at = (1:numVariables) - 0.5, 
       las = 2, 
       tick = FALSE, 
       line = 3,
       hadj = 1, 
       cex.axis = cexLabels)
  
  # Annotation
  mtext("Mean\naverage", side = 1, line = 1, at = 0.5, cex = 0.8, font = 1)
  mtext("Worst", side = 1, line = 1, at = -0.07, cex = 0.8, font = 1)
  mtext("Best", side = 1, line = 1, at = 1.07, cex = 0.8, font = 1)
  mtext("Local\nvalue", side = 1, line = 1, at = -0.23, cex = 0.8, font = 1)
  
  # Legend
  prob1 <- paste0(additionalProbs[1] * 100, "%")
  prob2 <- paste0(additionalProbs[2] * 100, "%")
  
  legend("topright",
         c(prob1, "Range", "Local value", "Middle 50%", prob2),
         pch = c(NA, NA, 16, NA, NA),
         fill = c(NA, "lightgrey", NA, "grey", NA),
         border = NA,
         col = c("red", NA, "blue", NA, "darkgreen"),
         lty = c(1, NA, NA, NA, 1),
         horiz = horizLegend,
         ncol = ncolLegend,
         bty = "o",
         box.col = "grey",
         inset = c(0, -0.3),
         cex = 0.8,
         x.intersp = xInterspLegend)
  
  # Title
  mtext(text = plotTitle, 
        side = 3, 
        line = 2, 
        at = -0.4, 
        cex = 1.5)
  
  # Cycle through the variables in data to plot bars etc
  for (i in 1:numVariables) {
    
    x <- data[, i]
    
    # Store values for later processing
    xMin <- min(x, na.rm = TRUE)
    xMax <- max(x, na.rm = TRUE)
    xMean <- mean(x, na.rm = TRUE)
    xBtmQuartile <- quantile(x, probs = 0.25, na.rm = TRUE)
    xTopQuartile <- quantile(x, probs = 0.75, na.rm = TRUE)
    
    tmp <- quantile(x, probs = additionalProbs, na.rm = TRUE)
    # names(tmp) <- gsub("%", "", names(tmp))
    xAddProb1 <- tmp[1]
    xAddProb2 <- tmp[2]
    
    # Determine worst and best values
    worst <- xMin
    best <- xMax
    if(lowIsGood[i] == TRUE) {
      worst <- xMax
      best <- xMin
    }
    
    # Centre data on mean
    dist <- max(c(xMean - xMin), (xMax - xMean))
    xScaleMin <- xMean - dist
    xScaleMax <- xMean + dist
    
    # Determine worst and best scale values
    ScaleWorst <- xScaleMin
    ScaleBest <- xScaleMax
    if(lowIsGood[i] == TRUE) {
      ScaleWorst <- xScaleMax
      ScaleBest <- xScaleMin
    }
    
    # Chart data
    value <- (x[pick] - ScaleWorst) / (ScaleBest - ScaleWorst)
    plotWorst <- (worst - ScaleWorst) / (ScaleBest - ScaleWorst)
    btmQuartile <- (xBtmQuartile - ScaleWorst) / (ScaleBest - ScaleWorst)
    topQuartile <- (xTopQuartile - ScaleWorst) / (ScaleBest - ScaleWorst)
    plotMean <- 0.5
    plotBest <- (best - ScaleWorst) / (ScaleBest - ScaleWorst)
    plotAddProb1 <- (xAddProb1 - ScaleWorst) / (ScaleBest - ScaleWorst)
    plotAddProb2 <- (xAddProb2 - ScaleWorst) / (ScaleBest - ScaleWorst)
    if(lowIsGood[i] == TRUE) {
      plotAddProb1 <- (xAddProb2 - ScaleWorst) / (ScaleBest - ScaleWorst)
      plotAddProb2 <- (xAddProb1 - ScaleWorst) / (ScaleBest - ScaleWorst)
    }
    
    # Plot the data
    y1 <- i + 0.1 - 1
    y2 <- i + 0.9 - 1
    doty <- i - 0.5
    rect(plotWorst, y1, plotBest, y2, col = "lightgrey", border = NA)
    rect(btmQuartile, y1, topQuartile, y2, col = "grey", border = NA)
    segments(x0 = 0.5, y0 = y1, x1 = 0.5, y1 = y2, lwd = 2)
    segments(x0 = plotAddProb1, y0 = y1, x1 = plotAddProb1, y1 = y2, lwd = 2, col = "red")
    segments(x0 = plotAddProb2, y0 = y1, x1 = plotAddProb2, y1 = y2, lwd = 2, col = "darkgreen")
    points(value, doty, pch = 16, col = "blue", cex = localValueCex)
    
    # Annotation
    mtext(x[pick], side = 2, line = 1, at = doty, las = 2, cex = 0.8, col = "blue")
    text(-0.02, doty, worst, cex = 0.8, pos = 2)
    text(1.01, doty, best, cex = 0.8, pos = 4)
  }
  
  par(op)
  
}
