pdfsqcdf<-function(s, dist, p1, p2)
{
  d.sample(s, dist, p1,p2)^2 *  p.sample(s, dist, p1,p2)
}
