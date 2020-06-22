q.sample<-function(s,dist, p1=0,p2=1)
{
  switch(dist, weib = qweibull(s, shape=p1,scale=p2),
         lognorm = qlnorm(s,meanlog=p1,sdlog=p2),
         norm = qnorm(s, mean=p1, sd=p2),
         uni = qunif(s, min=p1, max=p2),
         cauchy = qcauchy(s, p1, p2)	,
         fnorm =ifelse(s < 0, NaN, qnorm((s+1)/2, mean=0, sd=sqrt(pi/2)/p1)),
         normmixt = quantile(c(rnorm(ceiling(p1*1000), 0, 1),rnorm(floor((1-p1)*1000), 2,2)), s),
         skewnorm = qsn(s, alpha=p1),
         fas = qskt(s, df=p1,gamma=p2),
         shash = qSHASHo(s, nu=-p1,sigma=p2,mu=0,tau=1)
  )

}
