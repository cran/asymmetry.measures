eta.w.breve.bc <- function(xin,kfun)
  {
  s2<-length(xin)
  h<- bw.nrd(xin)
  Ui2.1 <- kde(xin, xin, h, kfun)
  Ui2.2 <- kde(xin, -xin, h, kfun)
  Ui2<-( Ui2.1 + Ui2.2)/2
  Wi2<-edf(xin, xin)

  MeanUi2<-mean(Ui2 )
  MeanWi2<-mean(Wi2)

  eta.w.breve.bc<- -(sum(Ui2 * Wi2) - s2 * mean(Ui2 ) * MeanWi2)/sqrt((sum(Ui2^2) - s2 *mean(Ui2 )^2) * (sum(Wi2^2) - s2 * MeanWi2^2))
  eta.w.breve.bc
}
