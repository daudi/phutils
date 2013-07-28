##' Produce a plot of power curves for proportions
##' 
##' @description Produce a plot of power curves for proportions based on sampsi.prop()
##' 
##' @details p1 and p2 are are required for the function to run. 
##' 
##' @param p1 p1 and p2, which are either the null and alternative proportions, respectively, for the one-sample test, 
##' or two proportions for the two-sample test. These arguments are required for the function to run.
##' @param p2 p1 and p2, which are either the null and alternative proportions, respectively, 
##' for the one-sample test, or two proportions for the two-sample test. If p2 is a vector multiple lines will be drawn.
##' @param ratio The ratio of smaller group to larger group (default=1)
##' @param power  Default of β=.80
##' @param alpha  Significance level (default of α=.05)
##' @param cont.corr A logical argument for whether to include the continuity correction (default is TRUE)
##' @param two.sided Whether to perform a two-sided test or one-sided (default is two)
##' @param one.sample Whether to perform the two-sample or one-sample test (defaut is two-sample test)
##' @param legend.adj adj parameter for legend placement. May need to may y value a large negative when there are multiple lines.
##' 
##' @return A dataframe with power and n
##' @author Originally by Slawa Rokicki srokicki@@fas.harvard.edu, extended by david.whiting@@publichealth.me.uk
##' @references http://rforpublichealth.blogspot.co.uk/2013/06/sample-size-calculations-equivalent-to.html
##' @export
##' @seealso sampsi.prop, sampsi.means, samp.clus, graph.power.means
##' @examples
##' ##proportions
##' sampsi.prop(.5, .55, power=.8, one.sample=TRUE)
##' sampsi.prop(.5, .55, ratio=2, two.sided=FALSE)
##' 
##' 
##' ##means
##' sampsi.means(0, 10, sd1=15, power=.8)
##' sampsi.means(10, 30, sd1=15, sd2=20, alpha=.1, ratio=1)
##' 
##' 
##' ##clustering
##' ss<-sampsi.prop(.10, .25, power=.8, two.sided=FALSE)
##' samp.clus(ss, rho=.05, obs.clus=15)
##' samp.clus(ss, rho=.05, num.clus=150)
##' 
##' ss2<-sampsi.means(10, 15, sd1=15, power=.8)
##' samp.clus(ss2, rho=.05, obs.clus=15)
##' samp.clus(ss2, rho=.05, num.clus=15) 
##' 
##' ##graphs
##' graph.power.prop(0.4, .99, p1=0.2, p2=c(0.18, 0.16, 0.10), power = 0.80)
##' graph.power.means(.7, 1, m1=0, m2=10, sd1=15)
##' 


graph.power.prop <- function(from.power, to.power,
                             p1, p2, 
                             ratio=1, 
                             power=.90, alpha=.05, 
                             cont.corr=TRUE, two.sided=TRUE, one.sample=FALSE,
                             legend.adj = c(0, -0.05)){
  
  if (to.power == 1.0)
    warning("to.power == 1.0 This will probably break the plot with Inf xlim")
  seq.p<-seq(from.power, to.power, by=.01)
  
  n <- list()
  for (i in 1:length(p2)) {
    this.p2 <- p2[i]
    n[[i]] <- unlist(lapply(seq.p, function(x) sampsi.prop(p1 = p1, this.p2, power = x, alpha=alpha, ratio=ratio, cont.corr=cont.corr, two.sided=two.sided, one.sample=one.sample)[["min.n"]]))
  }

  n <- matrix(unlist(n), ncol = length(p2))
  my.xlim  <-  c(min(n), max(n))
  plot(n[, 1], seq.p,
       xlim = my.xlim,
       ylab="Power", 
       xlab="n (in smaller arm)", type="n",  
       main=paste("Power graph for p1 =", p1, "and p2 =", paste(p2, collapse = ", ")))
  
  max.n <- max(n)
  for (i in 1:ncol(n)) {
    this.pos <- 4
    this.n <- n[, i]
    lines(this.n, seq.p)
    ## Align the label on the RHS the other way round.
    if(any(this.n == max.n))
      this.pos <- 2
    text(max(this.n), max(seq.p), paste("p2 =", p2[i]), cex = 0.7, pos = this.pos)
  }
  abline(h = power, col = "red")
  ret <- data.frame(power = seq.p, n = n)
  msg <- paste0("For power = ", power, "\n", paste(paste("p2 =", p2, "n =", ret[min(which(ret$power == power)), 2:ncol(ret)]), collapse = "\n"))
  legend("bottomright", msg, bty = "n", adj = legend.adj)
  
  invisible(ret)
}

