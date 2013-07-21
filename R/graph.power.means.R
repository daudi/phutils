##' Produce a power graph for same sizes based on means
##' 
##' @description Produce a graph showing the power of a study based on means, for different sample sizes. 
##' 
##' @param from.power The lower limit of the range of power that you want to consider. Graphs may look better
##' if you go way below what would be considered an acceptable power. 
##' @param to.power The upper limit of the range of power that you want to consider. Must be < 1 (e.g. 0.99)
##' @param m1 Mean in the first sample
##' @param m2 Mean in the second sample
##' @param sd1 Standard deviation in the first sample
##' @param sd2 Standard deviation in the second sample. If NA (the default) the function uses sd1 for both samples.
##' @param ratio The ratio of smaller group to larger group (default=1)
##' @param power Default of β=.80
##' @param alpha Significance level (default of α=.05)
##' @param two.sided Whether to perform a two-sided test or one-sided (default is two)
##' @param one.sample Whether to perform the two-sample or one-sample test (defaut is two-sample test)
##' 
##' @details Studies with power of less than 80% (0.8) are very weak. Sample sizes with power of more than 90% will result in
##' diminishing return on investment and may led to excessive expense in implementation.
##' 
##' @author Slawa Rokicki srokicki@@fas.harvard.edu
##' @references http://rforpublichealth.blogspot.co.uk/2013/06/sample-size-calculations-equivalent-to.html
##' @export
##' @seealso sampsi.prop, sampsi.means, samp.clus, graph.power.prop
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


graph.power.means <- function(from.power, to.power, 
                              m1, m2, 
                              sd1, sd2=NA, 
                              ratio=1, alpha=.05, cont.corr=TRUE, two.sided=TRUE, one.sample=FALSE){
  
  seq.p<-seq(from.power, to.power, by=.01)
  n<-rep(NA, length(seq.p))
  for(i in 1:length(seq.p)){
    ob<-sampsi.means(m1=m1, m2=m2, 
                     sd1=sd1, sd2=sd2, 
                     power=seq.p[i], alpha=alpha, ratio=ratio, one.sample=one.sample, two.sided=two.sided)[[2]]
    n[i]<-as.numeric(ob[9,2])
  }
  plot(n, seq.p, ylab="Power", xlab="n (in smaller arm)", type="l",  main=paste("Power graph for m1=", m1, "and m2=", m2))
}
