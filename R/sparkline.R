#R sparklines
sparkline <- function(ydata = rnorm(100,500,50), 
                      eng = NULL,
                      width = 1.5, height = 0.5, sigfigs = 4,
                      line.col = "gray",
                      cex = 0.5) {
  ## -- Jason Dieterle. http://www.edwardtufte.com/bboard/q-and-a-fetch-msg?msg_id=00037p
  ## ydata = vector of data to be plotted
  ## width = width of sparlkline in inches, including text
  ## height = height of sparkline in inches
  ## sigfigs = number of significant figures to round min, max, and last values to
  
  #temppar<-par(no.readonly = TRUE) # store default graphics parameters
  #par(mai = c(0.10, 0.05, 0.10, 0.05), fin = c(width, height)) # adjust graphics parameters for sparklines
  len <- length(ydata) # determine the length of the data set
  ymin <- min(ydata) # determine the minimum
  tmin <- which.min(ydata) # and its index
  ymax <- max(ydata) # determine the maximum
  tmax <- which.max(ydata) # and its index
  yfin <- signif(ydata[len], sigfigs) #determine most recent data point
  plotrange <- c(ymin - 0.3 * (ymax - ymin), ymax + 0.3 * (ymax - ymin)) # define plot range to leave enough room for min and max circles and text
  plot(x = 1:len, y = ydata, type = "l", xlim = c(1, len * 1.5), ylim = plotrange, col = line.col, lwd = 0.5, ann = FALSE, axes = FALSE) # plot sparkline
  points(x = c(tmin, tmax), y = c(ymin, ymax), pch = 19, col = c("red","blue"), cex = cex) # plot min and max points
  text(x = len, y = ymin, labels = signif(ymin, sigfigs), cex = cex, pos = 4, col = "red") # show minimum value
  text(x = len, y = ymax, labels = signif(ymax, sigfigs), cex = cex, pos = 4, col = "blue") # show maximum value
  text(x = len, y = (ymin + ymax) / 2, labels = yfin, cex = cex, pos = 4) # show most recent value
  if (!is.null(eng))
    text(x = len * 1.3, y = (ymin + ymax) / 2, labels = eng, cex = cex, pos = 4) # show England value
  #par(temppar) # restore graphics defaults
}

