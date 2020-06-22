pdfsqcdfstar<-function(s, dist, p1, p2)
{
  d.sample(s, dist, p1,p2)^2 * (1- p.sample(s, dist, p1,p2))
}
