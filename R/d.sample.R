d.sample<-function(s,dist, p1,p2)
{
  switch(dist, weib = dweibull(s, shape=p1, scale=p2),
         lognorm = dlnorm(s, meanlog=p1, sdlog=p2),
         norm = dnorm(s, mean=p1, sd=p2),
         uni = dunif(s,min=p1, max=p2),
         cauchy = dcauchy(s, p1, p2)	,
         fnorm = ifelse(s<0, 0,  2*dnorm(s, mean=0, sd=sqrt(pi/2)/p1)),
         normmixt = p1 * dnorm(s,p2[1],p2[2])+(1-p1)*dnorm(s,p2[3],p2[4]),
         skewnorm = dsn(s, alpha=p1),
         fas = dskt(s, df=p1,gamma=p2),
         shash = dSHASHo(s, nu=-p1,sigma=p2,mu=0,tau=1)
  )

}
