##' Sample size for proportions
##' 
##' @description Sample size for proportions
##' 
##' @details Based on sampsi commands in Stata.
##' 
##' @param p1  p1 and p2, which are either the null and alternative proportions, respectively, for the one-sample test, or two proportions for the two-sample test. These arguments are required for the function to run.
##' @param p2  p1 and p2, which are either the null and alternative proportions, respectively, for the one-sample test, or two proportions for the two-sample test. These arguments are required for the function to run.
##' @param ratio The ratio of smaller group to larger group (default=1)
##' @param power  Default of beta = 0.80
##' @param alpha  Significance level (default of alpha = 0.05)
##' @param cont.corr A logical argument for whether to include the continuity correction (default is TRUE)
##' @param two.sided Whether to perform a two-sided test or one-sided (default is two)
##' @param one.sample Whether to perform the two-sample or one-sample test (defaut is two-sample test)
##' @return A list object
##' 
##' @author Slawa Rokicki srokicki@@fas.harvard.edu
##' @references http://rforpublichealth.blogspot.co.uk/2013/06/sample-size-calculations-equivalent-to.html
##' @export
##' @seealso sampsi.means, samp.clus, graph.power.prop, graph.power.means
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





sampsi.prop<-function(p1, p2, 
                      ratio=1, 
                      power=.90, alpha=.05, 
                      cont.corr=TRUE, two.sided=TRUE, one.sample=FALSE){
  
  effect.size <- abs(p2 - p1)
  avg.p <- (p1 + ratio * p2)/(ratio + 1)
  sd <- ifelse(one.sample == FALSE, 
               sqrt(ratio * p1 * (1 - p1) + p2 * (1 - p2)), 
               sqrt(p2 * (1 - p2)))
  
  z.pow <- qt(1 - power, df = Inf, lower.tail = FALSE)
  z.alph <- ifelse(two.sided == TRUE, 
                   qt(alpha/2, df = Inf, lower.tail = FALSE), 
                   qt(alpha, df = Inf, lower.tail = FALSE))
  ct <- (z.pow + z.alph)
  
  n1 <- (z.alph * sqrt((ratio + 1) * avg.p * (1 - avg.p)) + z.pow * sd)^2/(effect.size^2 * ratio)
  n1.cont <- ifelse(cont.corr==FALSE, 
                    n1, 
                    (n1/4) * (1 + sqrt(1 + (2 * (ratio + 1))/(n1 * ratio * effect.size)))^2)
  
  n <- (((z.alph * sqrt(p1 * (1 - p1))) + z.pow * sd)/effect.size)^2
  
  if(one.sample == FALSE){
    n1 <- ceiling(n1.cont)
    n2 <- ceiling(n1.cont * ratio)
    col1 <- c("alpha", "power", "p1", "p2", "effect size", "n2/n1", "n1", "n2")
    col2 <- c(alpha,  power, p1, p2, effect.size, ratio, n1, n2)
    min.n <- min(n1, n2)
  }
  else{
    n <- ceiling(n)
    col1 <- c("alpha", "power", "p", "alternative", "n")
    col2 <- c(alpha, power, p1, p2, n)
    min.n <- n
  }
  ret <- as.data.frame(cbind(col1, col2))
  ret$col2 <- as.numeric(as.character(ret$col2))
  colnames(ret) <- c("Assumptions", "Value")
  
  description <- paste(ifelse(one.sample == FALSE, "Two-sample", "One-sample"), ifelse(two.sided==TRUE, "two-sided", "one-sided"), "test of proportions", ifelse(cont.corr==FALSE, "without", "with"), "continuity correction")
  
  retlist <- list(description = description, results = ret, min.n = min.n)
  
  return(retlist)
}


