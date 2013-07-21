##' Sample size for cluster design
##' 
##' @description Based on the sampclus function in Stata.
##' 
##' @details sampclus takes a sampsi.prop() or sampsi.means() object, along with an interclass correlation 
##' coefficient (ρ) and either the number of observations per cluster or the number of clusters, and 
##' calculates the corresponding effective sample size.
##' 
##' @param sampsi.object
##' @param rho Interclass correlation coefficient
##' @param num.clus The number of clusters. If NA (the default) obs.clus (the number of observations per cluster) must be specified
##' @param obs.clus The number of observations per cluster. If NA (the default) num.clus (the number of clusters) must be specified
##' 
##' 
##' @author Slawa Rokicki srokicki@@fas.harvard.edu
##' @references http://rforpublichealth.blogspot.co.uk/2013/06/sample-size-calculations-equivalent-to.html

##' @export
##' @seealso sampsi.prop, samp.means, graph.power.prop, graph.power.means
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


samp.clus<-function(sampsi.object, rho, num.clus=NA, obs.clus=NA){
  
  if(is.na(num.clus)&is.na(obs.clus)) print("Either num.clus or obs.clus must be identified")
  else{
    
    so<-sampsi.object[[2]]
    n1<-as.numeric(so[so$Assumptions=="n1",2])
    n2<-as.numeric(so[so$Assumptions=="n2",2])
    
    if(!is.na(obs.clus)){
      deff<-1+(obs.clus-1)*rho
      n1.clus<-n1*deff
      n2.clus<-n2*deff
      num.clus<-ceiling((n1.clus+n2.clus)/obs.clus)
    }
    else if(!is.na(num.clus)){
      
      tot<-(n1*(1-rho)+n2*(1-rho))/(1-(n1*rho/num.clus) - (n2*rho/num.clus))
      if(tot<=0) stop("Number of clusters is too small")
      else{
        obs.clus<-ceiling(tot/num.clus)
        deff<-1+(obs.clus-1)*rho
        n1.clus<-n1*deff
        n2.clus<-n2*deff
      }
    }
    
    col1<-c("n1 uncorrected", "n2 uncorrected", "ICC", "Avg obs/cluster", "Min num clusters", "n1 corrected", "n2 corrected")
    col2<-c(n1, n2, rho, obs.clus, num.clus, ceiling(n1.clus), ceiling(n2.clus))
    ret<-as.data.frame(cbind(col1, col2))
    colnames(ret)<-c("Assumptions", "Value")
    return(ret)
  }
}
