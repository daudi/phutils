##' SPC plot
##' 
##' This is a scatter plot with lines showing the mean, and 2 and 3 
##' standard deviations from the mean. Points outside of the 3rd standard deviation
##' are generally considered to be showing unusual variation.
##' 
##' @param x The x-axis values
##' @param y The y-axis values
##' @param ... Other parameters will be passed to plot()
##' @export
##' @return A list containing the mean and 2nd and 3rd standard 
##' deviations above and below the mean.

spc_plot <- function(x, y, ...) {
  
  spc_mean <- mean(y, na.rm = TRUE)
  spc_sd <- sd(y, na.rm = TRUE)
  spc_upper <- spc_mean + (3 * spc_sd)
  spc_lower <- spc_mean - (3 * spc_sd)
  spc_upper2 <- spc_mean + (2 * spc_sd)
  spc_lower2 <- spc_mean - (2 * spc_sd)
  
  plot(x, y, ...)
  
  abline(h = spc_mean, col = "black")
  abline(h = spc_lower, col = "red", lty = "dashed")
  abline(h = spc_upper, col = "red", lty = "dashed")
  abline(h = spc_lower2, col = "pink", lty = "dashed")
  abline(h = spc_upper2, col = "pink", lty = "dashed")
  
  text(min(x), spc_lower, "3 sigma", cex = 0.6, pos = 1)
  text(min(x), spc_lower2, "2 sigma", cex = 0.6, pos = 1)
  
  text(min(x), spc_upper, "3 sigma", cex = 0.6, pos = 1)
  text(min(x), spc_upper2, "2 sigma", cex = 0.6, pos = 1)
  invisible(list(mean = spc_mean,
                 lower2 = spc_lower2,
                 lower3 = spc_lower,
                 upper2 = spc_upper2,
                 upper3 = spc_upper))
}