eta.w.tilde.bc <- function(xin,kfun)
{
  s2<-length(xin)
  h<- bw.nrd(xin)
  Ui2.1 <- kde(xin, xin, h, kfun)
  Ui2.2 <- kde(xin, -xin, h, kfun)
  Ui2<-( Ui2.1 + Ui2.2)/2
  Vi2<-IntKde(xin, xin, h, IntEpanechnikov)
  MeanUi2<-mean(Ui2 )
  MeanVi2<-mean(Vi2)
  etatilde.bc<- -(sum(Ui2 * Vi2) - s2 * MeanUi2 * 0.5)/sqrt(s2 * (sum(Ui2^2) - s2 *MeanUi2^2) * 1/12 )
  etatilde.bc
}
