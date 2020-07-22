\name{p.sample}
\alias{p.sample}


\title{Switch between a range of available cumulative distribution functions.}

\description{Returns the value of the selected cumulative distribution function at user supplied grid points.}

\usage{p.sample(s,dist, p1,p2)}

\arguments{
  \item{s}{A scalar or vector: the x-axis grid points where the cumulative distribution function is be evaluated.}
  \item{dist}{Character string, used as a switch to the user selected distribution function (see details below).}
  \item{p1}{A scalar. Parameter 1 (vector or object) of the selected distribution.}
  \item{p2}{A scalar. Parameter 2 (vector or object) of the selected distribution.}
}

\details{Based on the user-specified argument \code{dist}, the function returns the value of the cumulative distribution function at \code{s}.

Supported distributions (along with the corresponding \code{dist} values) are:

  \itemize{
  \item{weib: }{The Weibull distribution is implemented as
  \deqn{F(s) = 1 - \exp \left \{- \left ( \frac{s}{p_2} \right )^{p_1} \right \} }  with \eqn{s > 0} where \eqn{p_1} is the shape parameter and \eqn{p_2} the scale parameter.}

  \item{lognorm: }{The lognormal distribution is implemented as
  \deqn{F(s)=\Phi \left ( \frac{\ln s-p_1 }{p_2} \right )} where \eqn{p_1} is the mean,\eqn{p_2} is the standard deviation   and \eqn{\Phi} is the cumulative distribution function of the standard normal distribution.}

  \item{norm: } {The normal distribution is implemented as \deqn{\Phi(s)={\frac {1}{\sqrt {2\pi}p_2 }}\int_{-\infty }^s e^{-\frac{(t-p_1)^2}{2p_2^2}}\,dt} where \eqn{p_1} is the mean and the \eqn{p_2} is the standard deviation.}

  \item{uni: }{The uniform distribution is implemented as
  \deqn{F(s)=\frac{s-p_1}{p_2-p_1}}  for   \eqn{p_1 \le s \le p_2.}}

  \item{cauchy: }{The cauchy distribution is implemented as
  \deqn{F(s;p_1,p_2)=\frac{1}{\pi}\arctan \left ( \frac{s-p_1}{p_2} \right ) + \frac{1}{2} }
  where \eqn{p_1} is the location parameter  and \eqn{p_2} the scale parameter.}

  \item{fnorm: }{The half normal distribution is implemented as
  \deqn{F_S(s;\sigma)=\int_0^s \frac{\sqrt{2/\pi}}{\sigma}  \exp \left \{ -\frac{x^2}{2\sigma^2} \right \} \,dx } where  \eqn{mean=0} and \eqn{sd=\sqrt{\pi/2}/p_1}.}

  \item{normmixt: }{The normal mixture distribution is implemented as
  \deqn{F(s)=p_1\frac{1}{p_2[2]\sqrt{2\pi}}\int_{-\infty }^{s}e^{-\frac{(t - p_2[1])^2}{2p_2[2]^2}}\,dt + (1-p_1) \frac{1}{p_2[4]\sqrt{2\pi}} \int_{-\infty }^s e^{-\frac{(t - p2[3])^2}{2p_2[4]^2}}\,dt}
  where \eqn{p_1} is a mixture component(scalar) and \eqn{p_2} a vector of parameters for the mean and variance of the two mixture components \eqn{p_2=c(mean1,sd1,mean2,sd2)}}.

\item{skewnorm: }{The skew normal distribution is implemented as
  \deqn{F(y; p_1) = \Phi \left ( \frac{y-\xi}{\omega} \right )-2 T \left ( \frac{y-\xi}{\omega},p_1 \right ) }
  where \eqn{location=\xi=0}, \eqn{scale=\omega=1}, \eqn{parameter=p_1} and \eqn{T(h, a)} is the Owens T function,    defined by
\deqn{T(h,a) = \frac{1}{2\pi}\int_{0}^{a} \exp \left \{ \frac{- 0.5 h^2 (1+x^2) }{1+x^2} \right \} \,dx, -\infty \le h, a \le \infty }
  }

  \item{fas: }{The Fernandez and Steel distribution is implemented as
  \deqn{F(s;p_1,p_2) = \frac{2}{p_1+\frac{1}{p_1}} \left \{ \int_{-\infty}^s f_t(x/p_1; p_2)I_{\{x \ge 0\}} \,dx + \int_{-\infty}^s f_t(p_1 x; p_2)I_{\{x<0\}}\, dx \right \} }
  where \eqn{f_t(x; \nu)} is the p.d.f. of the t distribution with \eqn{\nu = 5} degrees of freedom.\eqn{p_1} controls the skewness of the distribution with values between \eqn{(0, +\infty)} and \eqn{p_2} is the degrees of freedom.}

  \item{shash: }{The Sinh-Arcsinh distribution is implemented as
  \deqn{F(s;\mu, p_2, p_1, \tau) =\int_{-\infty}^s  \frac{ce^{-r^2/2}}{\sqrt{2\pi }} \frac{1}{p_2} \frac{1}{2} \sqrt{1+z^2}\,dz }

  where \eqn{r=\sinh(\sinh(z)- p_1)}, \eqn{c=\cosh(\sinh(z)- p_1)} and \eqn{z=(s-\mu)/p_2}. \eqn{p_1} is the vector of skewness, \eqn{p_2} is the scale parameter, \eqn{\mu=0} is the location parameter and \eqn{\tau=1} the kurtosis parameter.}
  }}

\value{A vector containing the cumulative distribution function values at the user specified points \code{s}.}

\references{
  \href{https://link.springer.com/chapter/10.1007/978-3-319-41582-6_1}{Bagkavos D., Patil P.N., Wood A.T.A. (2016), A Numerical Study of the Power Function of a New Symmetry Test. In: Cao R., Gonzalez Manteiga W., Romo J. (eds) Nonparametric Statistics. Springer Proceedings in Mathematics and Statistics, vol 175, Springer.}
}

\author{
Dimitrios Bagkavos and Lucia Gamez Gallardo

R implementation and documentation: Dimitrios Bagkavos <dimitrios.bagkavos@gmail.com> , Lucia Gamez Gallardo <gamezgallardolucia@gmail.com>
}
\seealso{
     \code{\link{r.sample}, \link{q.sample}, \link{d.sample}
      }}

\examples{
selected.d <- "weib" #select Weibull as the CDF
shape <- 2  # specify shape parameter
scale <- 1 # specify scale parameter
xout <- seq(0.1,5,length=50)  #design point where the CDF is evaluated
p.sample(xout,selected.d,shape,scale)  # calculate CDF at xout
}

