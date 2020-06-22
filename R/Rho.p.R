Rho.p<-function(xin, p.param, dist, p1=0, p2=1)
{
  xi.p <- q.sample(p.param, dist, p1,p2)
  lowerlim <- -Inf
  upperlim <- xi.p
  sampleuse<-xin[which(xin<=upperlim )]

  xpoints<-seq(min(sampleuse), max(sampleuse), length=50)
  pdfest<-    d.sample(xpoints, dist, p1,p2)
  cdfest<-p.sample(xpoints, dist, p1,p2)
  n<-length(xpoints)

  psi1<- sum(pdfest * cdfest)/n
  psi2<-  sum(pdfest )/n
  psi3<-  sum(pdfest^2 )/n

  denom<- p.param * psi3 - psi2^2
  rhop<- ifelse(denom<0, 0, 2*sqrt(3)/p.param * ( psi1 - p.param/2 * psi2 )/sqrt(p.param * psi3 - psi2^2))

  rhop
}
