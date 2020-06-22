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
  \item{weib:}{The Weibull distribution is implemented as \deqn{F(s) = 1 - exp(- (s/p2)^p1) with s > 0} where \eqn{p1} is the shape parameter and \eqn{p2} the scale parameter.}
  \item{lognorm:}{The lognormal distribution is implemented as \deqn{F(s)=\Phi({\frac {(\ln s)-p1 }{p2}})} where \eqn{p1} is the mean,\eqn{p2} is the sd parameters and \eqn{\Phi} is the cumulative distribution function of the standard normal distribution, i.e.,\deqn{\Phi(s)={\frac {1}{\sqrt {2\pi*p2 }}}\int_{-\infty }^{s}e^{-(t-p1)^2/2*p2^2}*dt }.}
  \item{norm:} {The normal distribution is implemented as \deqn{\Phi(s)={\frac {1}{\sqrt {2\pi}p2 }}\int_{-\infty }^{s}e^{-(t-p1)^2/2*p2^2}*dt} where \eqn{p1} is the mean and the \eqn{p2} is the sd parameters.}
  \item{uni:}{The uniform distribution is implemented as \deqn{F(s)=(s-p1)/(p2-p1)} for two constants \eqn{p1=min} and \eqn{p2=max} such that \eqn{p1 < s < p2}.}
  \item{cauchy:}{The cauchy distribution is implemented as \deqn{F(s;p1,p2)=(1/\pi)*\arctan((s-p1)/p2)+1/2} where \eqn{p1} is the location parameter  and \eqn{p2} the scale parameter.}
  \item{fnorm:}{The half normal distribution is implemented as \deqn{F_S(s;\sigma)=\int _{0}^{s}(1/\sigma)*\sqrt(2/\pi)*\exp(-(x^2)/(2*\sigma^2))*dx} where  \eqn{mean=0} and \eqn{sd=sqrt(pi/2)/p1}.}
  \item{normmixt:}{The normal mixture distribution is implemented as \deqn{F(s)=p1*(1/(\sqrt(2*\pi)*p2[2])*\int_{-\infty }^{s}{e^-((t - p2[1])^2/(2*p2[2]^2))dt})+(1-p1)*(1/(\sqrt(2*\pi)*p2[4])*\int_{-\infty }^{s}{e^-((t - p2[3])^2/(2*p2[4]^2))dt})} where \eqn{p1}is a mixture component(scalar) and \eqn{p2} a vector of parameters for the mean and variance of the two mixture components \eqn{p2=c(mean1,sd1,mean2,sd2)}}.
  \item{skewnorm:}{The skew normal distribution is implemented as \deqn{F(y;p1) = \Phi((y-\xi)/\omega)-2*T((y-\xi)/\omega,p1)} where \eqn{location=\xi=0}, \eqn{scale=\omega=1}, \eqn{parameter=p1} and T(h, b) is the Owens T function.It is defined by \deqn{T(h,a) = \frac{1}{2*\pi}\int_{0}^{a}{(e^-(0.5*h^2*(1+x^2))))/(1+x^2) dx}}.}
  \item{fas:}{The Fernandez and Steel distribution is implemented as \deqn{F(s;p1,p2) = \int_{-\infty}^{s}{(2/(p1+1/p1))*[f_t(x/p1;p2)*I{x>=0} + f_t(p1*x;p2)*I{x<0}]*dx}} where \eqn{f_t(x;\nu)} is the p.d.f. of the t distribution with \eqn{\nu = 5} degrees of freedom.\eqn{p1} control the skewness of the distribution with values between \eqn{(0,+\infty)} and \eqn{p2} is the degrees of freedom.}
  \item{shash:}{The Sinh-Arcsinh distribution is implemented as \deqn{F(s;\mu,p2,-p1,\tau) =\int_{-\infty}^{s}{ (c*e^{-r^2/2}/(\sqrt(2*\pi)))*(1/p2)*(1/2*(1+z^2)^(1/2))*dz}} where \eqn{r=sinh(sinh(z)-(-p1))}
  , \eqn{c=cosh(sinh(z)-(-p1))} and \eqn{z=((s-\mu)/p2)}.\eqn{-p1} is the vector of skewness,\eqn{p2}is the scale parameter,\eqn{\mu=0} is the location parameter and \eqn{\tau=1} the kurtosis parameter.}
  }}

\value{A vector containing the cumulative distribution function values at the user specified points \code{s}.}

\references{
   \href{https://link.springer.com/chapter/10.1007\%2F978-3-319-41582-6_1}{D. Bagkavos, P.N. Patil and A.T.A. Wood, "A Numerical Study of the Power Function of a New Symmetry Test"}
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

