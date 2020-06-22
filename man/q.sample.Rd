\name{q.sample}
\alias{q.sample}


\title{Switch between a range of available quantile functions.}

\description{
Returns the quantiles of selected distributions at user specified locations.}

\usage{q.sample(s,dist, p1=0,p2=1)}

\arguments{
  \item{s}{A scalar or vector: the probabilities where the  quantile function will be evaluated.}
  \item{dist}{Character string, used as a switch to the user selected distribution function (see details below).}
  \item{p1}{A scalar. Parameter 1 (vector or object) of the selected distribution.}
  \item{p2}{A scalar. Parameter 2 (vector or object) of the selected distribution.}
}

\details{
 Based on user-specified argument \code{dist}, the function returns the value of the quantile function at \code{s}.

Supported distributions (along with the corresponding \code{dist} values) are:

  \itemize{
  \item{weib:}{The weibull distribution with \eqn{\zeta_0} the treshold parameter has quantile function implemented as\deqn{Q(s) = \zeta_0 + p1*(Q(s))^1/p2} where \eqn{p1} is the shape parameter and \eqn{p2} the scale parameter.}
  \item{lognorm:}{The lognormal distribution has quantile function implemented as \deqn{F^{-1}(s)=(\Phi({\frac {(\ln s)-p1 }{p2}}))^{-1}}  where \eqn{p1} is the mean,\eqn{p2} is the sd parameters and \eqn{\Phi} is the cumulative distribution function of the standard normal distribution, i.e.,\deqn{\Phi(s)={\frac {1}{\sqrt {2\pi*p2 }}}\int_{-\infty }^{s}e^{-(t-p1)^2/2*p2^2}*dt} and \eqn{F(x)^{-1}} returns the probability quantile function,i.e., the inverse of the cumulative distribution function.}
    \item{norm:}{The normal distribution has quantile function implemented as \deqn{Q(p)=\Phi^{-1}(s)=({\frac {1}{\sqrt {2\pi}p2 }}\int_{-\infty }^{s}e^{-(t-p1)^2/2*p2^2}*dt)^{-1}} where \eqn{p1} is the mean and the \eqn{p2} is the sd parameters.}
  \item{uni:}{The uniform distribution has quantile function implemented as \deqn{Q(s;p1,p2)=s(p2-p1)+p1} for two constants \eqn{p1=min} and \eqn{max=p2} such that \eqn{p1 < s < p2}.}
  \item{cauchy:}{The cauchy distribution has quantile function implemented as \deqn{Q(s)=p1 + p2 * \tan(\pi*(s-(1/2)))}where \eqn{p1} is the location parameter and \eqn{p2} the scale parameter.}
  \item{fnorm:}{The half normal distribution has quantile function implemented as \deqn{Q(p)=\Phi^{-1}(s)=({\frac {1}{\sqrt {2\pi}sd }}\int_{-\infty }^{(s+1)/2}e^{-(t)^2/2*sd^2}*dt)^{-1}} where  \eqn{mean=0} and \eqn{sd=sqrt(pi/2)/p1}.}
  \item{normmix:}{The normal mixture distribution has quantile function implemented as \deqn{Q(s) = (1 - p1)s + p1*s[j+1]} where \eqn{(j-m)/n <= p < (j-m+1)/n}, \eqn{s[j]} is the jth order statistic, \eqn{n} is the sample size,\eqn{p1} is a function of \eqn{j = floor(ns + m) and g = np + m - j}, and \eqn{m} is a constant determined by the sample quantile type.}
  \item{skewnorm:}{The skew normal distribution has quantile function implemented as \deqn{Q(s)=F^{-1}(y;p1) = (\Phi((y-\xi)/\omega)-2*T((y-\xi)/\omega,p1))^{-1}} where \eqn{location=\xi=0}, \eqn{scale=\omega=1}, \eqn{parameter=p1} and T(h, b) is the Owens T function.It is defined by \deqn{T(h,a) = \frac{1}{2*\pi}\int_{0}^{a}{(e^-(0.5*h^2*(1+x^2))))/(1+x^2) dx}}.}
  \item{fas:}{The Fernandez and Steel distribution is implemented as \deqn{Q(s)=F^{-1}(s;p1,p2) = (\int_{-\infty}^{s}{(2/(p1+1/p1))*[f_t(x/p1;p2)*I{x>=0} + f_t(p1*x;p2)*I{x<0}]*dx})^{-1}} where \eqn{f_t(x;\nu)} is the p.d.f. of the t distribution with \eqn{\nu = 5} degrees of freedom.\eqn{p1}  control the skewness of the distribution with values between \eqn{(0,+\infty)} and \eqn{p2} is the degrees of freedom.}
  \item{shash:}{The Sinh-Arcsinh distribution is implemented as \deqn{Q(s)=F^{-1}(s;\mu,p2,-p1,\tau) =(\int_{-\infty}^{s}{ (c*e^{-r^2/2}/(\sqrt(2*\pi)))*(1/p2)*(1/2*(1+z^2)^(1/2))*dz})^{-1}} where \eqn{r=sinh(sinh(z)-(-p1))}, \eqn{c=cosh(sinh(z)-(-p1))} and \eqn{z=((s-\mu)/p2)}.\eqn{-p1} is the vector of skewness,\eqn{p2} the scale parameter,\eqn{\mu=0} is the location parameter and \eqn{\tau=1} the kurtosis parameter.}
}}

\value{A vector containing the quantile values at the user specified points \code{s}.}

\references{
   \href{https://link.springer.com/chapter/10.1007\%2F978-3-319-41582-6_1}{D. Bagkavos, P.N. Patil and A.T.A. Wood, "A Numerical Study of the Power Function of a New Symmetry Test"}
    }

\author{
  Dimitrios Bagkavos and Lucia Gamez Gallardo

R implementation and documentation: Dimitrios Bagkavos <dimitrios.bagkavos@gmail.com> , Lucia Gamez Gallardo <gamezgallardolucia@gmail.com>
  }

\seealso{
    \code{\link{r.sample}, \link{d.sample}, \link{p.sample}}
    }

\examples{
selected.q <- "norm" #select Normal as the distribution
shape <- 2  # specify shape parameter
scale <- 2 # specify scale parameter
xout <- seq(0.1,1,length=50)  #design point where the quantile function is evaluated
q.sample(xout,selected.q,shape,scale)  # calculate quantiles at xout
  }
