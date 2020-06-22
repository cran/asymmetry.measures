Rhostar.p.exact<-function(xin, p.param, dist, p1=0, p2=1)
{
  xi.1minusp <-  q.sample(1-p.param, dist, p1,p2) #q.sample(1-p.param, dist, p1,p2) #quantile(sample, probs=1-p.param) #
  lowerlim<- xi.1minusp
  upperlim<- +Inf
  sampleuse<-xin[which( xin >= lowerlim)]
  xpoints<- seq(min(sampleuse), max(sampleuse), length=100)#  sampleuse[order(sampleuse)]


  pdfest<- d.sample(xpoints, dist, p1,p2) #kde(sampleuse,xpoints, band)+kde(sampleuse,-xpoints, band) # ksmooth(sampleuse, kernel="normal", bandwidth=band, x.points=xpoints)$y #(ksmooth(sam, kernel="normal", bandwidth=band, x.points=xpoints)$y + ksmooth(-sam,  kernel="normal", bandwidth=band, x.points=xpoints)$y)/2
  cdfest<- p.sample(xpoints, dist, p1,p2) #IntKde(sampleuse,  xpoints, band2) #edf(sampleuse, xpoints)

  d1<-integrate(pdfsqcdfstar, lower=lowerlim, upper=upperlim, dist, p1, p2)$value
  d2<-integrate(pdfsq, lower=lowerlim, upper=upperlim, dist, p1, p2)$value
  d3<-integrate(pdfthird, lower=lowerlim, upper=upperlim, dist, p1, p2)$value

  psi1star<- mean(pdfest * cdfest) # mean(pdfest * cdfest)
  psi2star<-  mean(pdfest ) #mean(pdfest)
  psi3star<-  mean(pdfest^2 ) #mean(pdfest^2)


  denom.int<- p.param * d3 - d2^2

  denom.tmp <- p.param * psi3star - psi2star^2
  denom.p   <-ifelse(denom.tmp<0, sqrt(denom.int), sqrt(denom.tmp))

  num.p<- 2*sqrt(3)/p.param *  ( -psi2star + psi1star + p.param/2 * psi2star )


  rhostarp <- num.p / denom.p
  rhostarp

}
