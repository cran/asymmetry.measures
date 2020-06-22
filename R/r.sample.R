r.sample<-function(s,dist, p1=0,p2=1)
{
  switch(dist, weib = rweibull(s, shape=p1,scale=p2),
         lognorm = rlnorm(s,meanlog=p1,sdlog=p2),
         norm = rnorm(s, mean=p1, sd=p2),
         uni = runif(s, min=p1, max=p2),
         cauchy = rcauchy(s, p1, p2)	,
         fnorm = abs(rnorm(s, mean=0, sd=sqrt(pi/2)/p1)),
         normmixt = c(rnorm(ceiling(p1*s), 0, 1),rnorm(floor((1-p1)*s), 2,2)),
         skewnorm = rsn(s, alpha=p1),
         fas = rskt(s, df=p1,gamma=p2),
         shash = rSHASHo(s, nu=-p1,sigma=p2,mu=0,tau=1)
  )
}
