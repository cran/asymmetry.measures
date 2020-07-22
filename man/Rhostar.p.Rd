\name{Rhostar.p}
\alias{Rhostar.p}


\title{Calculates \eqn{\rho_p^*}, used in the implementation of the strong asymmetry measure \eqn{\eta(X)}.}

\description{
Estimates \eqn{\rho_p^*}, used in the calculation of the strong asymetry measure \eqn{\eta(X)}.}

\usage{
  Rhostar.p(xin, p.param, dist, p1, p2)
}

\arguments{
  \item{xin}{A vector of data points - the available sample.}
    \item{p.param}{A parameter with the value greater than or equal to 1/2 and less than 1.}
  \item{dist}{Character string, specifies selected distribution function.}
  \item{p1}{A scalar. Parameter 1 (vector or object) of the selected distribution.}
  \item{p2}{A scalar. Parameter 2 (vector or object) of the selected distribution.}
  }

\details{
Implements the quantity

\deqn{
\frac{ 2\sqrt{3}}{p} \frac{-\int_{\xi_{1-p}}^{\infty}{f^2(x)(1-F(x))\,dx}+\frac{p}{2}\int_{\xi_{1-p}}^{\infty} f^2(x)\,dx}{ \left \{ p\int_{\xi_{1-p}}^{\infty}f^3(x)\,dx-(\int_{\xi_{1-p}}^{\infty}f^2(x)\,dx)^2 \right \}^{1/2} } }

 defined on page 6 \href{https://link.springer.com/article/10.1007/s13171-013-0034-z}{Patil, Bagkavos and Wood (2014)}, see also (5) in \href{https://link.springer.com/chapter/10.1007/978-3-319-41582-6_1}{Bagkavos, Patil and Wood (2016)}. Estimation of the p.d.f. and c.d.f. functions is currently performed by maximum likelihood as e.g. kernel estimates inherit large amount of variance to \eqn{\rho_p^*}.
}

\value{ Returns a scalar, the value of \eqn{\rho_p^*}.}

\references{
  \itemize{
    \item   \href{https://link.springer.com/article/10.1007/s13171-013-0034-z}{Patil P.N., Bagkavos D. and Wood A.T.A., (2014). A measure of asymmetry based on a new necessary and sufficient condition for symmetry, Sankhya A, 76, 123--145.}
    \item  \href{https://link.springer.com/chapter/10.1007/978-3-319-41582-6_1}{{Bagkavos D., Patil P.N., Wood A.T.A. (2016), A Numerical Study of the Power Function of a New Symmetry Test. In: Cao R., Gonzalez Manteiga W., Romo J. (eds) Nonparametric Statistics. Springer Proceedings in Mathematics and Statistics, vol 175, Springer.}
  }}}

\author{
  Dimitrios Bagkavos and Lucia Gamez Gallardo

R implementation and documentation: Dimitrios Bagkavos <dimitrios.bagkavos@gmail.com>, Lucia Gamez Gallardo <gamezgallardolucia@gmail.com>
  }

  %\note{
    %%  ~~further notes~~
      %}

 \seealso{
  \code{\link{Rho.p}, \link{Rhostar.p.exact}, \link{Rho.p.exact}}
    }

  \examples{
set.seed(1234)

selected.r <- "weib" #select Weibull as the distribution
shape <- 1 # specify shape parameter
scale <- 1  # specify scale parameter
n <- 100    # specify sample size
param <- 0.9 # specify parameter
xout<-r.sample(n,selected.r,shape,scale) # specify sample
Rhostar.p(xout,param,selected.r,shape,scale)  # calculate Rhostar.p
#-0.08936363  # returns the result

selected.r2 <- "norm" #select Normal as the distribution
n <- 100    # specify sample size
mean <- 0 # specify the mean
sd <- 1 # specify the variance
param <- 0.9 # specify parameter
xout <-r.sample(n,selected.r2,mean,sd) # specify sample
Rhostar.p(xout,param,selected.r2,mean,sd) # calculate Rhostar.p
#-0.02302223 # returns the result


selected.r3 <- "cauchy" #select Cauchy as the distribution
n <- 100    # specify sample size
location <- 0 # specify the location parameter
scale <- 1 # specify the scale parameter
param <- 0.9 # specify parameter
xout<-r.sample(n,selected.r3,location,scale) # specify sample
Rhostar.p(xout,param,selected.r3,location,scale) # calculate Rhostar.p
#0.02043852  # returns the result

    }
