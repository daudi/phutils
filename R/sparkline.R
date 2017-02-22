##' Create sparklines
##'
##' Simple function to create sparklines.
##'   
##' @param ydata Numeric vector of data to display.
##' @param eng A single value for England (or any other comparator). If NULL (the default) this is not shown.
##' @param width The width of the sparkline in inches.
##' @param height The height of the sparkline in inches.
##' @param sigfigs Number of significant figures in the labels.
##' @param line.col The colour of the line.
##' @param cex Character expansion (the size of the font)
##'   
##' @examples
##' temppar <- par(no.readonly = TRUE) # store default graphics parameters
##' 
##' width <- 1
##' height <- 0.5
##' 
##' ## Five sparklines in one png file.
##' png(file = "test.png", width = 150, height = 250)
##' split.screen(c(5,1))
##' spark.mai <- c(0.10, 0.05, 0.10, 0.05)
##' 
##' screen(1)
##' par(mai = spark.mai)
##' sparkline(line.col = "black", cex = 0.7)
##' 
##' screen(2)
##' par(mai = spark.mai)
##' sparkline(line.col = "black", cex = 0.7)
##' 
##' screen(3)
##' par(mai = spark.mai)
##' sparkline(line.col = "black", cex = 0.7)
##' 
##' screen(4)
##' par(mai = spark.mai)
##' sparkline(line.col = "black", cex = 0.7)
##' 
##' screen(5)
##' par(mai = spark.mai)
##' sparkline(line.col = "black", cex = 0.7)
##' 
##' 
##' close.screen(all = TRUE)
##' par(temppar) # restore graphics defaults
##' dev.off()
##' 
##' 
##' ## A single sparkline in a png file
##' temppar <- par(no.readonly = TRUE) # store default graphics parameters
##' png(file = "test.png", width = 150, height = 50)
##' spark.mai <- c(0.10, 0.05, 0.10, 0.05)
##' par(mai = spark.mai)
##' sparkline(line.col = "black", cex = 0.7)
##' par(temppar) # restore graphics defaults
##' dev.off()
##' 
##' 
##' ## Single sparkline with annotation. 
##' ## It seems that height must be >= 140 if using screen() which means
##' ## that it is too large and the font relatively too small.
##' png(file = "test.png", width = 400, height = 140)
##' split.screen(c(1, 2))
##' spark.mai <- c(0.10, 0.05, 0.10, 0.05)
##' 
##' screen(1)
##' par(mai = spark.mai)
##' sparkline(line.col = "black", cex = 0.7)
##' 
##' screen(2)
##' text(0, 0.5, "This is a test", adj = c(0, 0))
##' close.screen(all = TRUE)
##' par(temppar) # restore graphics defaults
##' dev.off()
##' 
##' 
##' ## Nice example using PDF is easier than PNG. Can copy in adobe reader then paste into
##' ## other documents. 
##' 
##' ind.row <- function(n, narrative, rag){
##'screen1 <- ((n - 1) * 3) + 1
##'screen2 <- screen1 + 1
##'screen3 <- screen1 + 2
##'
##'screen(screen1)
##'text(0, 0.8, narrative, adj = c(0, 1), cex = cex.text)
##'
##'screen(screen2)
##'par(mai = spark.mai)
##'sparkline(line.col = "black", cex = cex, eng = "3.2")
##'
##'screen(screen3)
##'par(mai = spark.mai)
##'rag.blocks(rag, blocks=FALSE, symbol.cex = 6)
##'
##'}
##'
##'
##'cex <- 0.5
##'cex.text <- 1
##'spark.mai <- c(0.10, 0.05, 0.10, 0.05)
##'
##'pdf("test.pdf")
##'split.screen(c(10, 3))
##'ind.row(1, "This is a test", c("R", "G", "A", "G"))
##'ind.row(2, "This is a test", c("G", "G", "A", "G"))
##'ind.row(3, "This is a test", c("G", "R", "A", "G"))
##'ind.row(4, "This is a test", c("G", "G", "A", "A"))
##'
##'close.screen(all = TRUE)
##'dev.off()
##' 
##' 
##' 
##' 
##' 
##' 
##' 
##' 
##' 
##' 
##' ## Five sparklines with text in one png file.
##' ## This works nicely, except the resolution is poor.
##' ## 12 pixels per mm.
##' 
##' rag.rate <- function(status) {
##'   ## Add a block of rag-rated rectangles
##'   ## status: a vector of "R", "A", and "G"s.
##'   num.items <- length(status)
##'   rag <- c("R", "A", "G")
##'   rag.colours <- c("red", "orange", "green")
##'   rag.status <- rag.colours[match(status, rag)]
##'   rag.xpos <- seq(from = 0, by = 1 / num.items, length.out = num.items)
##'   rag.xpos2 <- seq(to = 1, by = 1 / num.items, length.out = num.items)
##'   rag.text.xpos <- (rag.xpos + rag.xpos2) / 2
##'   plot.new()
##'   rect(rag.xpos, 0, 1, 1, col = rag.status)
##'   text(rag.text.xpos, 0.5, labels = status, cex = cex)
##' }
##' 
##' ind.row <- function(n, narrative, rag){
##'   screen1 <- ((n - 1) * 3) + 1
##'   screen2 <- screen1 + 1
##'   screen3 <- screen1 + 2
##'   
##'   screen(screen1)
##'   text(0, 0.8, narrative, adj = c(0, 1), cex = cex.text)
##'   
##'   screen(screen2)
##'   par(mai = spark.mai)
##'   sparkline(line.col = "black", cex = cex, eng = "3.2")
##'   
##'   screen(screen3)
##'   par(mai = spark.mai)
##'   rag.rate(rag)
##'   
##' }
##' 
##' 
##' reso <- 300
##' widthmm <- 170 ## 170mm fits nicely in A4
##' heightmm <- 200
##' cex <- 0.5
##' cex.text <- 1
##' png(file = "test.png", width = widthmm, height = heightmm, units = "mm", res = reso)
##' split.screen(c(10, 3))
##' spark.mai <- c(0.10, 0.05, 0.10, 0.05)
##' 
##' 
##' close.screen(all = TRUE)
##' par(temppar) # restore graphics defaults
##' dev.off()
##'   
##'   
##'   
##' @export
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

