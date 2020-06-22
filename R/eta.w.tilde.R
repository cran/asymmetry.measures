eta.w.tilde<- function(xin,kfun)
{
  s1<-length(xin)
  h<- bw.nrd(xin)
  Ui1.1<-kde(xin, xin, h, kfun)
  Ui1.2<-kde(xin, -xin, h, kfun)
  Ui1<- (Ui1.1+ Ui1.2)/2

  Vi1 <-IntKde(xin, xin, h, IntEpanechnikov)
  MeanUi1<-mean(Ui1 )
  etatilde<- -(sum(Ui1 * Vi1) - s1 * MeanUi1 * 0.5)/sqrt(s1 * (sum(Ui1^2) - s1 *MeanUi1^2) * 1/12 )
  etatilde
  }
