##' Create a Percentile plot

##' @description A percentile plot is a means of simultaneously displaying the
##' summary statistics of multiple numeric variables whilst highlighting the 
##' position of a particular element within the distribution of each variable. 

##' @details  Values are scaled so that the mean is centrered in the plotting 
##' window. This is a similar methodology as used by Public Health England in 
##' the area profiles section of 'Finger tips' tools 
##' (http://www.phoutcomes.info/).

##' @param data data frame or matrix of numeric and/or integer variables
##' @param pick element or row of data to highlight with dot overlaid on bars. 
##' Must be an integer between 1 and nrow(data). Default is 1.
##' @param additionalProbs pair of values at which to draw additional vertical 
##' lines on each bar. Must be a numeric vector of length two where values are 
##' greater than zero and less than one. Default is c(0.05, 0.95).
##' @param lowIsGood
##' @param cex
##' @param labels
##' @param title

##' @author Mark Chambers mark.chambers@@medway.gov.uk
##' @reference 
##' @export
##' @return Does not return anything at the moment. Creates a plot as a side-effect.
##' @examples 
##' attach(mtcars)
##' x <- wt
##' y <- mpg
##' 
##' df1 <- data.frame(var1 = wt, var2 = mpg)
##' df2 <- data.frame(var1 = wt, var2 = mpg, var3 = 1:length(wt), var4 = wt, abcdefghijklmno = mpg)
##' df3 <- data.frame(var1 = wt, var2 = mpg, var3 = 1:length(wt), var4 = wt, var5 = mpg, var6 = wt, var7 = mpg, var8 = 1:length(wt), var9 = wt, abcdefghijklmno = mpg)
##' 
##' percentile_plot(df3, pick = 5, low_is_good = c(rep(FALSE, 9), TRUE))
##'
##' percentile_plot(df1, pick = 5)




percentile_plot <- function(data, 
                       pick = 1, 
                       additionalProbs = c(0.05, 0.95), 
                       lowIsGood = rep(FALSE, ncol(data)), 
                       localValueCex = (20/ncol(data)),
                       plotLabels = colnames(data),
                       plotTitle = "Percentile plot") {
  
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
  data <- a[, 1:7]
  plotLabels <- myLabels
  
  leftMar <- ceiling(max(nchar(plotLabels), na.rm = TRUE) / 1.5)

  # Setup an empty plotting window
  op <- par(mar = c(5, leftMar, 5, 2) + 0.1, xpd = TRUE)
  
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
       hadj = 1)

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
    text(-0.02, doty, worst, cex = 0.8, pos = 2)
    text(1.01, doty, best, cex = 0.8, pos = 4)
    mtext(x[pick], side = 2, line = 1, at = doty, las = 2, cex = 0.8, col = "blue")
    mtext("Mean\naverage", side = 1, line = 1, at = 0.5, cex = 1)
    mtext("Worst", side = 1, line = 1, at = -0.07, cex = 1)
    mtext("Best", side = 1, line = 1, at = 1.07, cex = 1)
    mtext("Local\nvalue", side = 1, line = 1, at = -0.23, cex = 1)
    
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
           horiz = FALSE,
           ncol = 2,
           bty = "o",
           box.col = "grey",
           inset = c(0, -0.2),
           cex = 0.8,
           x.intersp = 0.5)
    
    # Title
    mtext(text = plotTitle, 
          side = 3, 
          line = 2, 
          at = -0.5, 
          cex = 1.5)
}
  
  par(op)
  
}
