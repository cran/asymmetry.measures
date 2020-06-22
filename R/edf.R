edf<-function(xin, xout)
{
  n<-length(xout)
  nn<-length(xin)
  xin.use<-sort(xin)
  out<-sapply(1:n, function(i, xin.use, xout) length(which(xin.use <= xout[i])), xin.use, xout)
  out/nn
}
