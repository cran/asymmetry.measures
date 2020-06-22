\name{pdfsqcdfstar}
\alias{pdfsqcdfstar}


\title{Calculate \eqn{f^2(x)(1-F(x))}.}

\description{Return the product \eqn{f^2(x)(1-F(x))}.}

\usage{pdfsqcdfstar(s,dist, p1,p2)}

\arguments{
  \item{s}{A scalar or vector: the x-axis grid points where the probability density function will be evaluated.}
  \item{dist}{Character string, used as a switch to the user selected distribution function (see details below).}
  \item{p1}{A scalar. Parameter 1 (vector or object) of the selected density.}
  \item{p2}{A scalar. Parameter 2 (vector or object) of the selected density.}
}

\details{Based on user-specified argument \code{dist}, the function returns the value of
\eqn{f^2(x)(1-F(x))dx}, used in the definitions of \eqn{\rho_p^*}, \eqn{\rho_p} and their exact versions.

Supported distributions (along with the corresponding \code{dist} values) are:

  \itemize{
  \item{weib:}{The weibull distribution is implemented as \deqn{f(s;p1,p2)= (p1/p2) (s/p2)^(p1-1) exp(- (s/p2)^p1) with s>=0} where \eqn{p1} is the shape parameter and \eqn{p2} the scale parameter.}
  \item{lognorm:}{The lognormal distribution is implemented as \deqn{f(s) = 1/(\sqrt(2*\pi)*p2*s)*e^-((log s -p1)^2 /(2*p2^2))} where \eqn{p1} is the mean and \eqn{p2} is the sd parameters.}
  \item{norm:}{The normal distribution is implemented as \deqn{f(s) = 1/(\sqrt(2*\pi)*p2)*e^-((s - p1)^2/(2*p2^2))} where \eqn{p1} is the mean and the \eqn{p2} is the sd parameters.}
  \item{uni:}{The uniform distribution is implemented as \deqn{f(s) = 1/(p2-p1) for p1 <= s <= p2} where \eqn{min=p1} and \eqn{max=p2}.}
   \item{cauchy:}{The cauchy distribution is implemented as \deqn{f(s)=(1\pi*p2)*(1+((s-p1)/p2)^2)^{-1}} where \eqn{p1} is the location parameter  and \eqn{p2} the scale parameter.}

  \item{fnorm:}{The half normal distribution is implemented as \deqn{2*f(s)-1} where \deqn{f(s) = 1/(\sqrt(2*\pi)*sd)*e^-((s)^2/(2*sd^2))}, \eqn{mean=0} and \eqn{sd=sqrt(pi/2)/p1}.}

  \item{normmixt:}{The normal mixture distribution is implemented as \deqn{f(s)=p1*(1/(\sqrt(2*\pi)*p2[2])*e^-((s - p2[1])^2/(2*p2[2]^2)))+(1-p1)*(1/(\sqrt(2*\pi)*p2[4])*e^-((s - p2[3])^2/(2*p2[4]^2))} where \eqn{p1}is a mixture component(scalar) and \eqn{p2} a vector of parameters for the mean and variance of the two mixture components \eqn{p2=c(mean1,sd1,mean2,sd2)}.}
    \item{skewnorm:}{The skew normal distribution with parameter \eqn{p1} is implemented as \deqn{f(s)=2\phi(s)\Phi(p1*s)}.}
    \item{fas:}{The Fernandez and Steel distribution is implemented as \deqn{f(s;p1,p2) = (2/(p1+1/p1)*[f_t(s/p1;p2)*I{s>=0} + f_t(p1*s;p2)*I{s<0}]} where \eqn{f_t(x;\nu)} is the p.d.f. of the t distribution with \eqn{\nu = 5} degrees of freedom.\eqn{p1} control the skewness of the distribution with values between \eqn{(0,+\infty)} and \eqn{p2} is the degrees of freedom.}
  \item{shash:}{The Sinh-Arcsinh distribution is implemented as \deqn{f(s;\mu,p2,-p1,\tau) = (c*e^{-r^2/2}/(\sqrt(2*\pi)))*(1/p2)*(1/2*(1+z^2)^(1/2))} where \eqn{r=sinh(sinh(z)-(-p1))}
  , \eqn{c=cosh(sinh(z)-(-p1))} and \eqn{z=((s-\mu)/p2)}. \eqn{-p1} is the vector of skewness,\eqn{p2}is the scale parameter,\eqn{\mu=0} is the location parameter and \eqn{\tau=1} the kurtosis parameter.}
  }}
\value{A vector containing the user selected density values at the user specified points \code{s}.}
\references{
  \href{https://link.springer.com/chapter/10.1007\%2F978-3-319-41582-6_1}{D. Bagkavos, P.N. Patil and A.T.A. Wood, "A Numerical Study of the Power Function of a New Symmetry Test"}
}

\author{
  Dimitrios Bagkavos and Lucia Gamez Gallardo

  R implementation and documentation: Dimitrios Bagkavos <dimitrios.bagkavos@gmail.com> ,Lucia Gamez Gallardo <gamezgallardolucia@gmail.com>
}


\seealso{ \code{\link{r.sample}, \link{q.sample}, \link{p.sample} }  }

\examples{
  selected.dens <- "weib" #select Weibull
  shape <- 2  # specify shape parameter
  scale <- 1 # specify scale parameter
  xout <- seq(0.1,5,length=50)  #design point
  pdfsqcdfstar(xout,selected.dens,shape,scale)  #return f^2(xout)F(xout)
}
