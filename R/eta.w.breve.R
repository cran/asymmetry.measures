eta.w.breve <- function(xin,kfun)
{
  s1<-length(xin)

  h<- bw.nrd(xin)

  Ui1.1<-kde(xin, xin, h, kfun)
  Ui1.2<-kde(xin, -xin, h, kfun)
  Ui1<- (Ui1.1+ Ui1.2)/2

  Wi1<-edf(xin, xin)
  MeanWi1<-mean(Wi1)
  MeanUi1<-mean(Ui1 )

  etabreve<- -(sum(Ui1 * Wi1) - s1 * MeanUi1 * MeanWi1)/sqrt((sum(Ui1^2 )- s1 * MeanUi1^2) * (sum(Wi1^2) - s1 * MeanWi1^2))
  etabreve
}
