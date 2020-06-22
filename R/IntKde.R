"IntKde"<-function(xin, xout, h=3.572*sd(xin)*length(xin)^{-1/3}, kfun)
{
  n<- length(xin)
  arg1<-(sapply(xout, "-", xin))/h
  arg2<-kfun(arg1)
  arg3<-colSums(arg2)
  arg3/n
}
