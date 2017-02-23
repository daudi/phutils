##' Create population pyramids
##' 
##' @param data A dataframe or matrix or a vetor. If a dataframe or matrix 
##'  the first two columns are numeric values
##'  (e.g. numbers of males and females). The first column will be plotted on the left, the 
##'  second on the right. A third column may contain the names, e.g. age groups, 
##'  otherwise the dataframe row names will be used. If a vector, then this will be the
##'  left values and the right values will be supplied using the Right parameter (see next).
##'  
##' @param Right If not NULL this will be the values to be plotted on the right (the default is NULL)
##' @param Center The values to be plotted in between the left and right portions 
##' of the pyramid. If NULL (the default) then the row names of the dataframe will be used.
##' 
##' @param Laxis A vector of axis for left pyramid. If missing, automatically given.
##' @param Raxis A vector of axis for right pyramid. If missing, Laxis is used.
##' @param AxisFM A format code of formatC for plotting axis. If missing, "g" is used.
##' @param AxisBM A big.mark of formatC for plotting axis. If missing, none.
##' @param AxisBI A big.interval number of formatC for plotting axis. Default is 3
##' @param Cgap The width of center gap (as ratio to each panel) to draw age-class. 
##' Default is 0.3
##' @param Cstep The interval to write the labels of age classes. Default is 1
##' @param Csize The font size factor to write the labels of age classes. Default is 1
##' @param Cadj The vertical adjustment factor for the labels of age classes. 
##' Default is 0
##' @param Llab The label of the left pyramid. Default is "Males".
##' @param cex.Llab Character expansion for the left label
##' @param Rlab The label of the right pyramid. Default is "Females".
##' @param cex.Rlab Character expansion for the right label
##' @param Clab The label of the center age-class. Default is "Ages".
##' @param GL Logical value to draw the vertical dotted lines. Default is TRUE.
##' @param Lcol The color of the left pyramid. Default is "Cyan".
##' @param Ldens The density of hatching lines (/inch) for left pyramid. 
##' Default is -1, when the pyramid will be filled.
##' @param Rcol The color of the right pyramid. Default is "Pink".
##' @param Rdens The density of hatching lines (/inch) for right pyramid. 
##' Default is -1, when the pyramid will be filled.
##' @param main The main title of the pyramid.
##' @param overlay The default (FALSE) creates a new pyramid. If TRUE overlay 
##' the new pyramid on top of a previous pyramid. You MUST use the Laxis values returned
##' by the first pyramid to ensure that the new bars are drawn on the same scale.
##' @param border color for rectangle border(s). The default means par("fg"). Use 
##' border = NA to omit borders. If there are shading lines, border = TRUE 
##' means use the same colour for the border as for the shading lines. You can also 
##' specify colours, e.g. border = "blue" or border = "transparent".
##' @param glCol vertical grid line colour. Default is blue.
##' @param textSrt text rotation for axis labels. Choose 0 for left to right
##' @param ... Other options.
##'  
##' @return A vector of Laxis values. These are needed if an overlay 
##' is created. 
##' @details Still need to work on this to make it look nice.
##' 
##' Use the wrapper pyramids() if you want to use two vectors instead of a dataframe.
##' 
##' @author Original code Minato Nakazawa <minato-nakazawa@umin.net>. 
##' Modified by David Whiting <david.whiting@publichealth.me.uk>
##' @export
##' @examples
##' x <- data.frame(males = 10:1, females = 10:1)
##' pyramid(x)
##' 
##' males <- 20:1
##' females <- 20:1
##' pyramid(males, females)
##' 

pyramid <- function(data, Right = NULL, Center = NULL,
                    Laxis = NULL, Raxis = NULL, 
                    AxisFM="g", AxisBM="", AxisBI=3, Cgap=0.3, Cstep=1, Csize=1, 
                    Llab="Males", Rlab="Females", Clab="Ages", 
                    cex.Llab = 1, cex.Rlab = 1,
                    GL=TRUE, Cadj = 0, 
                    Lcol="Cyan", Rcol="Pink", Ldens=-1, Rdens=-1, main="", 
                    border = NULL, glCol = "blue", textSrt = 0,
                    overlay = FALSE, ...) {
  ## A function to draw population pyramid
  ## rev 1.0: 5th January 2010
  ## rev 1.1: 6th January 2010: Added "Cadj" option with faint modification.
  ## rev 1.2: 11th March 2010: Added "Csize", "AxisFM", "AxisBM", and "AxisBI"
  ##          options, as suggested by Dr. Philippe Guillet.
  ## (C) Minato Nakazawa <minato-nakazawa@umin.net>
  ## Extended by David Whiting <david.whiting@publichealth.me.uk>
  if (!is.null(Right)) {
    Left <- data
    if (length(Left) != length(Right)) warning("Length of left and right data differ")
  } else {
    Left <- data[, 1]
    Right <- data[, 2]
  }
  if (!is.null(Right) & is.null(Center)) {
    Center <- 1:length(Right)
  }
  if (is.null(Right) & is.null(Center)) {
    if (length(data) == 2) { 
      Center <- row.names(data) 
    } else { 
      Center <- data[, 3] 
    }
  }
  if (overlay & is.null(Laxis))
    stop("You MUST use the Laxis information from the previous pyramid. See help page.")
  if (is.null(Laxis)) 
    Laxis <- seq(0, max(c(Left, Right)), len = 5) 
  if (is.null(Raxis)) 
    Raxis <- Laxis 
  ## setting x-y axes
  BX <- c(-1 - Cgap / 2, 1 + Cgap / 2)
  BY <- c(-0.05, 1.1)
  if (!overlay)
    plot(BX, BY, type = "n", axes = FALSE, xlab = "", ylab = "", main = main, ...)
   ## scaling factors
  LL <- max(Laxis)
  LR <- min(Laxis)
  LS <- LL - LR
  LI <- length(Laxis)
  RL <- min(Raxis)
  RR <- max(Raxis)
  RS <- RR - RL
  RI <- length(Raxis)
  ## ticks of axis
  if (!overlay) {
    segments(-(Laxis - LR) / LS - Cgap / 2, -0.01, -(Laxis - LR) / LS - Cgap / 2, 0.01)
    segments( (Raxis - RL) / RS + Cgap / 2, -0.01,  (Raxis - RL) / RS + Cgap / 2, 0.01)
  }
  ## vertical grid lines
  if (GL) {
    segments(-(Laxis-LR)/LS-Cgap/2,0,-(Laxis-LR)/LS-Cgap/2,1,lty=3,col= glCol)
    segments((Raxis-RL)/RS+Cgap/2,0,(Raxis-RL)/RS+Cgap/2,1,lty=3,col= glCol)
  }
  ## axes
  if (!overlay) {
    lines(c(-1-Cgap/2,-Cgap/2),c(0,0),lty=1)
    lines(c(-Cgap/2,-Cgap/2),c(0,1),lty=1)
    lines(c(1+Cgap/2,Cgap/2),c(0,0),lty=1)
    lines(c(Cgap/2,Cgap/2),c(0,1),lty=1)
  }
  ## labels
  if (!overlay) {
    text(-0.5 - Cgap / 2, 1, Llab, pos = 3, cex = cex.Llab)
    text( 0.5 + Cgap / 2, 1, Rlab, pos = 3, cex = cex.Rlab)
    text(0, 1, Clab, pos = 3)
  }
  Ci <- length(Center)
  if (!overlay) {
    for (i in 0:(Ci-1)) { 
      if ((i %% Cstep) == 0) 
        text(0, i / Ci + Cadj, paste(Center[i+1]), pos = 3, cex = Csize)
    }
  }
  # The axis value labels
  if (!overlay) {
    text(-(Laxis - LR) / LS - Cgap / 2, rep(0, LI),
         paste(formatC(Laxis, format = AxisFM, big.mark = AxisBM, big.interval = AxisBI)), pos = 1, srt = textSrt)
    text((Raxis - RL) / RS + Cgap / 2, rep(0, RI),
         paste(formatC(Raxis, format = AxisFM, big.mark = AxisBM, big.interval = AxisBI)), pos = 1, srt = textSrt)
  }
  ## draw rectangles. These are the bars of the pyramid.
  VB <- 0:(Ci-1)/Ci
  VT <- 1:Ci/Ci
  LeftP <- -(Left-LR)/LS-Cgap/2
  rect(LeftP,VB,rep(-Cgap/2,Ci),VT,col=Lcol,density=Ldens, border = border)
  RightP <- (Right-RL)/RS+Cgap/2
  rect(rep(Cgap/2,Ci),VB,RightP,VT,col=Rcol,density=Rdens, border = border)
  
  ## Return some useful values
  invisible(Laxis)
}

