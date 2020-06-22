eta.s.exact<-function(xin, dist, GridLength, p1, p2)
{
  #sample<- r.sample(xin, dist, p1, p2)
  p<-seq(0.5, 0.999, length=GridLength)
  r1<-sapply(1:GridLength,
             function(i, xin, p, dist, p1, p2)
             {
               ro1<-Rho.p.exact(xin, p[i], dist, p1, p2)
               ro2<-Rhostar.p.exact(xin, p[i], dist, p1, p2)
               ifelse((ro1==0) || (ro2==0), 0, abs(ro1+ro2) )
             },
             xin, p, dist, p1,p2)
  - 0.5 * sign(Rho.p.exact(xin, 1, dist, p1, p2)) * max(r1)
}
