"Epanechnikov"<- function(x){
  ifelse(abs(x)< 2.236068, (3/4)*(1-((x^2)/5 ))/2.236068, ifelse(abs(x)>2.236068, 0,0))
}
