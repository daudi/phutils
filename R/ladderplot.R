##' Create a ladderplot
##' 
##' This function is taken from the package plotrix and adds the parameter lwd.lines
##' 
##' @param x   A matrix or data frame with at least 2 columns.
##' @param scale   Logical, if the original data columns should be scaled to the unit (0-1) interval.
##' @param col 	Color values to use for rows of x. If longer than 1, its value is recycled.
##' @param pch 	Point type to use. If longer than 1, its value is recycled.
##' @param lty 	Line type to use. If longer than 1, its value is recycled.
##' @param lwd.lines Specify the thickness of the lines between the two sets of data.
##' @param xlim Limits for axes.
##' @param ylim	Limits for axes.
##' @param vertical 	Logical, if the orientation of the ladderplot should be vertical or horizontal.
##' @param ordered 	Logical, if the columns in x should be ordered.
##' @param ... 	Other arguments passed to the function stripchart. 
##' 
##' @export
##' @references Original from the package plotrix https://cran.r-project.org/package=plotrix 
##' @keywords utils
##' 
##' @examples
##' 
##' males <- c(0.222, 0.261, 0.131, 0.104, 0.095, 0.166, 0.023)
##' females <- c(0.312, 0.161, 0.203, 0.063, 0.094, 0.165, 0)
##' x <- data.frame(males = males, females = females)
##' x <- x * 100
##' rownames(x) <- c("Circulatory", "Cancer", "Respiratory", "Digestive", "External", "Other", "<28 days")
##' ladderplot(x, yaxt = "n", frame.plot = FALSE)
##' i <- 1:nrow(x)
##' text(1, x[i, 1], paste0(rownames(x[i, ]), " ", round(x[i, 1], 1), "%"), pos = 2, cex = 0.8)
##' text(2, x[i, 2], paste0(rownames(x[i, ]), " ", round(x[i, 2], 1), "%"), pos = 4, cex = 0.8)
##' 


ladderplot <- function (x, scale = FALSE, col = 1, pch = 19, lty = 1, lwd.lines = 1, xlim = c(0.5, 
    ncol(x) + 0.5), ylim = range(x), vertical = TRUE, ordered = FALSE, 
    ...) 
{
    x <- as.data.frame(x)
    if (scale) 
        x <- apply(x, 2, function(x) (x - min(x, na.rm = TRUE))/(max(x, 
            na.rm = TRUE) - min(x, na.rm = TRUE)))
    if (NCOL(x) < 2) 
        stop("'x' must have at least 2 columns")
    nr <- nrow(x)
    if (length(col) < nr) 
        col <- rep(col, nr)[1:nr]
    if (length(pch) < nr) 
        pch <- rep(pch, nr)[1:nr]
    if (length(lty) < nr) 
        lty <- rep(lty, nr)[1:nr]
    if (ordered) 
        x <- x[, order(colnames(x))]
    y <- data.frame(values = array(unlist(x)), ind = factor(rep(1:ncol(x), 
        each = nrow(x)), labels = colnames(x)))
    id <- match(colnames(x), levels(y$ind))
    if (vertical) {
        with(y, stripchart(values ~ ind, pch = pch, ylim = ylim, 
            xlim = xlim, vertical = vertical, col = "white", 
            ...))
        lapply(1:ncol(x), function(i) points(cbind(rep(i, nr), 
            x[, id[i]]), col = col, pch = pch))
        lapply(1:nr, function(i) lines(cbind(id, as.matrix(x)[i, 
            ]), col = col[i], lty = lty[i], lwd = lwd.lines))
    }
    else {
        tmp <- xlim
        xlim <- ylim
        ylim <- tmp
        with(y, stripchart(values ~ ind, pch = pch, ylim = ylim, 
            xlim = xlim, vertical = vertical, col = "white", 
            ...))
        lapply(1:ncol(x), function(i) points(cbind(x[, id[i]], 
            rep(i, nr)), col = col, pch = pch))
        lapply(1:nr, function(i) lines(cbind(as.matrix(x)[i, 
            ], id), col = col[i], lty = lty[i], lwd = lwd.lines))
    }
    invisible(NULL)
}
