eta.w.hat<-function(xin,kfun)
{
  s1<-length(xin)
  h<- bw.nrd(xin)
  Vi1 <-IntKde(xin, xin, h, IntEpanechnikov)
  Ui1.1<-kde(xin, xin, h, kfun)
  Ui1.2<-kde(xin, -xin, h, kfun)
  Ui1<- (Ui1.1+ Ui1.2)/2
  MeanUi1<-mean(Ui1 )
  MeanVi1<-mean(Vi1)
  etahat<- -(sum(Ui1 * Vi1) - s1 * MeanUi1 * MeanVi1)/sqrt( (sum(Ui1^2) - s1 *MeanUi1^2) * (sum(Vi1^2) - s1 * MeanVi1^2))
  etahat
}
