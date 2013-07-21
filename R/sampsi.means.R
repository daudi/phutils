##' Sample size calculations for means
##' 
##' @description Sample size calculations for means. 
##' 
##' @details Based on sampsi commands in Stata.
##' 
##' @param m1 Mean in the first sample
##' @param m2 Mean in the second sample
##' @param sd1 Standard deviation in the first sample
##' @param sd2 Standard deviation in the second sample. If NA (the default) the function uses sd1 for both samples.
##' @param ratio The ratio of smaller group to larger group (default=1)
##' @param power Default of β=.80
##' @param alpha Significance level (default of α=.05)
##' @param two.sided Whether to perform a two-sided test or one-sided (default is two)
##' @param one.sample Whether to perform the two-sample or one-sample test (defaut is two-sample test)
##' @return A list object
##' 
##' 
##' @author Slawa Rokicki srokicki@@fas.harvard.edu
##' @references http://rforpublichealth.blogspot.co.uk/2013/06/sample-size-calculations-equivalent-to.html
##' 
##' @export
##' @seealso sampsi.prop, samp.clus, graph.power.prop, graph.power.means
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



sampsi.means<-function(m1, m2, sd1, sd2=NA, ratio=1, power=.90, alpha=.05, two.sided=TRUE, one.sample=FALSE){
  
  effect.size<-abs(m2-m1)
  sd2<-ifelse(!is.na(sd2), sd2, sd1)
  
  z.pow<-qt(1-power, df=Inf, lower.tail=FALSE)
  z.alph<-ifelse(two.sided==TRUE, qt(alpha/2, df=Inf, lower.tail=FALSE), qt(alpha, df=Inf, lower.tail=FALSE))
  ct<-(z.pow+z.alph)
  
  n1<-(sd1^2+(sd2^2)/ratio)*(ct)^2/(effect.size^2)
  n<-(ct*sd1/effect.size)^2
  
  if(one.sample==FALSE){
    col1<-c("alpha", "power", "m1", "m2", "sd1", "sd2", "effect size", "n2/n1", "n1", "n2")
    col2<-c(alpha,  power, m1, m2, sd1, sd2, effect.size, ratio, ceiling(n1), ceiling(n1*ratio))
  }
  else{
    col1<-c("alpha", "power", "null", "alternative", "n")
    col2<-c(alpha, power, m1, m2, ceiling(n))
  }
  ret<-as.data.frame(cbind(col1, col2))
  ret$col2<-as.numeric(as.character(ret$col2))
  colnames(ret)<-c("Assumptions", "Value")
  
  description<-paste(ifelse(one.sample==FALSE, "Two-sample", "One-sample"), ifelse(two.sided==TRUE, "two-sided", "one-sided"), "test of means")
  
  retlist<-list(description, ret)
  
  return(retlist)
}

