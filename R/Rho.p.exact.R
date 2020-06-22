Rho.p.exact<-function(xin, p.param, dist, p1=0, p2=1)
{
  xi.p <-  q.sample(p.param, dist, p1,p2) #q.sample(p.param, dist, p1,p2) #quantile(sample, probs=p.param) #
  lowerlim <- -Inf
  upperlim <- xi.p
  sampleuse<-xin[which(xin<=upperlim )]

  xpoints<-seq(min(sampleuse), max(sampleuse), length=50) #sampleuse[order(sampleuse)]

  pdfest<- d.sample(xpoints, dist, p1,p2)

  cdfest<-p.sample(xpoints, dist, p1,p2)

  d1<-integrate(pdfsqcdf, lower=lowerlim, upper=upperlim, dist, p1, p2)$value
  d2<-integrate(pdfsq, lower=lowerlim, upper=upperlim, dist, p1, p2)$value
  d3<-integrate(pdfthird, lower=lowerlim, upper=upperlim, dist, p1, p2)$value

  psi1<- mean(pdfest * cdfest)  # mean(pdfest * cdfest)
  psi2<-  mean(pdfest )  #mean(pdfest)
  psi3<-  mean(pdfest^2 ) #mean(pdfest^2)
  denom.int<- p.param * d3 - d2^2
  denom.tmp <- p.param * psi3 - psi2^2
  denom.p   <-ifelse(denom.tmp<0, sqrt(denom.int), sqrt(denom.tmp))

  num.p<- 2*sqrt(3)/p.param * ( psi1 - p.param/2 * psi2 )
  rhop <- num.p / denom.p #traditional definition based on true curves - integrals estimated by averages
  rhop
}
