\name{r.sample}
\alias{r.sample}


\title{Switch between a range of  available random number generators.}

\description{Generate a random sample of size \eqn{n} out of a range of available distributions.}

\usage{r.sample(s, dist, p1=0, p2=1)}

\arguments{
  \item{s}{A scalar which specifies the size of the random sample drawn.}
  \item{dist}{Character string, used as a switch to the user selected distribution function (see details below).}
  \item{p1}{A scalar. Parameter 1 (vector or object) of the selected distribution.}
  \item{p2}{A scalar. Parameter 2 (vector or object) of the selected distribution.}

}

\details{ Based on user-specified argument \code{dist}, the function returns a random sample of size \eqn{s} from the corresponding distribution.

Supported distributions (along with the corresponding \code{dist} values) are:

  \itemize{
  \item{weib:}{Supports random number generation from the weibull distribution, the density is implemented as \deqn{f(s;p1,p2)= (p1/p2) (s/p2)^(p1-1) exp(- (s/p2)^p1) with s>=0} where \eqn{p1} is the shape parameter and \eqn{p2} the scale parameter.}
  \item{lognorm:}{Supports random number generation from the lognormal distribution,the density is implemented as \deqn{f(s) = 1/(\sqrt(2*\pi)*p2*s)*e^-((log s -p1)^2 /(2*p2^2))} where \eqn{p1} is the mean and \eqn{p2} is the sd parameters.}
  \item{norm:}{Supports random number generation from the normal distribution with density implemented as\deqn{f(s) = 1/(\sqrt(2*\pi)*p2)*e^-((s - p1)^2/(2*p2^2))} where \eqn{p1} is the mean and the \eqn{p2} is the sd parameters.}
  \item{uni:}{Supports random number generation from the uniform distribution with density implemented as \deqn{f(s) = 1/(p2-p1) for p1 <= s <= p2}where \eqn{min=p1} and \eqn{max=p2}.}
   \item{cauchy:}{Supports random number generation from the cauchy distribution, the density is implemented as \deqn{f(s)=(1/\pi*p2)*(1+((s-p1)/p2)^2)^{-1}} where \eqn{p1} is the location parameter  and \eqn{p2} the scale parameter.}
  \item{fnorm:}{Supports random number generation from the half normal distribution which is implemented as \deqn{2*f(s)-1} where \deqn{f(s) = 1/(\sqrt(2*\pi)*sd)*e^-((s)^2/(2*sd^2))}, \eqn{mean=0} and \eqn{sd=sqrt(pi/2)/p1}.}
    \item{normmixt:}{Supports random number generation from the normal mixture distribution, the density is implemented as \deqn{f(s)=p1*(1/(\sqrt(2*\pi)*p2[2])*e^-((s - p2[1])^2/(2*p2[2]^2)))+(1-p1)*(1/(\sqrt(2*\pi)*p2[4])*e^-((s - p2[3])^2/(2*p2[4]^2))} where \eqn{p1} a mixture component(scalar) and \eqn{p2} a vector of parameters for the mean and variance of the two mixture components \eqn{p2=c(mean1,sd1,mean2,sd2)}.}
    \item{skewnorm:}{Supports random number generation from the skew normal distribution with parameter \eqn{p1} and density implemented as \deqn{f(s)=2\phi(s)\Phi(p1 s)}.}
    \item{fas:}{Supports random number generation from the Fernandez and Steel distribution, the density is implemented as\deqn{f(s;p1,p2) = (2/(p1+1/p1)*[f_t(s/p1;p2)*I{s>=0} + f_t(p1*s;p2)*I{s<0}]} where \eqn{p1} control the skewness of the distribution with values between \eqn{(0,+\infty)} and \eqn{p2} is the degrees of freedom.}
  \item{shash:}{Supports random number generation from the Sinh-Arcsinh distribution, the density is implemented as \deqn{f(s;\mu,p2,-p1,\tau) = (c*e^{-r^2/2}/(\sqrt(2*\pi)))*(1/p2)*(1/2*(1+z^2)^(1/2))} where \eqn{r=sinh(sinh(z)-(-p1))}
  , \eqn{c=cosh(sinh(z)-(-p1))} and \eqn{z=((s-\mu)/p2)}. \eqn{-p1} is the vector of skewness,\eqn{p2}is the scale parameter,\eqn{\mu=0} is the location parameter and \eqn{\tau=1} the kurtosis parameter.}
  }
  }

\value{A vector of random values at the user specified points \code{s}.}

\references{
   \href{https://link.springer.com/chapter/10.1007\%2F978-3-319-41582-6_1}{D. Bagkavos, P.N. Patil and A.T.A. Wood, "A Numerical Study of the Power Function of a New Symmetry Test"}
    }

\author{
Dimitrios Bagkavos and Lucia Gamez Gallardo

R implementation and documentation: Dimitrios Bagkavos <dimitrios.bagkavos@gmail.com> , Lucia Gamez Gallardo <gamezgallardolucia@gmail.com>
}
\seealso{
  \code{\link{d.sample}, \link{q.sample}, \link{p.sample}}
    }

\examples{
selected.r <- "norm" #select Normal as the distribution
shape <- 2  # specify shape parameter
scale <- 1  # specify scale parameter
n <- 100    # specify sample size
r.sample(n,selected.r,shape,scale)  # calculate CDF at the designated point
}
