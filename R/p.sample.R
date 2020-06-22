p.sample<-function(s,dist, p1,p2)
{
  switch(dist, weib = pweibull(s, shape=p1, scale=p2),
         lognorm = plnorm(s,meanlog=p1,sdlog=p2),
         norm = pnorm(s, mean=p1, sd=p2),
         uni = punif(s, min=p1, max=p2),
         cauchy = pcauchy(s, p1, p2)	,
         fnorm = ifelse(s < 0, 0, 2*pnorm(s, mean=0, sd=sqrt(pi/2)/p1)-1),
         normmixt = p1 * pnorm(s,0,1)+(1-p1)*pnorm(s,2,2),
         skewnorm = psn(s, alpha=p1),
         fas = pskt(s, df=p1,gamma=5),
         shash = pSHASHo(s, nu=-p1,sigma=p2,mu=0,tau=1)
  )
}
