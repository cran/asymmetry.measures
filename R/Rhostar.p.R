Rhostar.p<-function(xin, p.param, dist, p1, p2)
{
  xi.1minusp <- q.sample(1-p.param, dist, p1,p2)
  lowerlim<- xi.1minusp
  upperlim<- +Inf
  sampleuse<-xin[which( xin >= lowerlim)]
  xpoints<-seq(min(sampleuse), max(sampleuse), length=50)
  pdfest<-    d.sample(xpoints, dist, p1,p2)
  cdfest<-p.sample(xpoints, dist, p1,p2)

  n<-length(xpoints)

  psi1star<- sum(pdfest * cdfest)/n
  psi2star<-  sum(pdfest )/n
  psi3star<-  sum(pdfest^2 )/n

  denom<- p.param * psi3star - psi2star^2
  rhostarp<-ifelse(denom<0, 0, 2*sqrt(3)/p.param *  ( -psi2star + psi1star + p.param/2 * psi2star )/sqrt(p.param * psi3star - psi2star^2))



  rhostarp
}
