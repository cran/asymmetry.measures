"IntEpanechnikov"<-function(x)
{
  ifelse(x< -2.236068, 0, ifelse(x> 2.236068, 1, .5- (2.236068*x*(x^2-15))/100))
}
