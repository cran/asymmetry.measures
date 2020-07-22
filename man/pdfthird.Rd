\name{pdfthird}
\alias{pdfthird}


\title{Calculate \eqn{f^3(x)}}

\description{Return the value of \eqn{f^3(x)}.}

\usage{pdfthird(s,dist, p1,p2)}

\arguments{
  \item{s}{A scalar or vector: the x-axis grid points where the probability density function will be evaluated.}
  \item{dist}{Character string, used as a switch to the user selected distribution function (see details below).}
  \item{p1}{A scalar. Parameter 1 (vector or object) of the selected density.}
  \item{p2}{A scalar. Parameter 2 (vector or object) of the selected density.}
}

\details{Based on user-specified argument \code{dist}, the function returns the value of \eqn{f^3(x)dx}, used in the definitions of \eqn{\rho_p^*}, \eqn{\rho_p} and their exact versions.


Supported distributions (along with the corresponding \code{dist} values) are:

 \itemize{
\item{weib: }{The weibull distribution is implemented as \deqn{f(s;p_1,p_2)= \frac{p_1}{p_2} \left (\frac{s}{p_2}\right )^{p_1-1} \exp \left \{- \left (\frac{s}{p_2}\right )^{p_1} \right \} } with \eqn{ s \ge 0} where \eqn{p_1} is the shape parameter and \eqn{p_2} the scale parameter.}

 \item{lognorm: }{The lognormal distribution is implemented as \deqn{f(s) = \frac{1}{p_2s\sqrt{2\pi}}e^{-\frac{(log s -p_1)^2}{2p_2^2}}} where \eqn{p_1} is the mean and \eqn{p_2} is the standard deviation of the distirbution.}

\item{norm: }{The normal distribution is implemented as \deqn{f(s) = \frac{1}{p_2\sqrt{2 \pi}}e^{-\frac{ (s - p_1)^2 }{ 2p_2^2 }}} where \eqn{p_1} is the mean and the \eqn{p_2} is the standard deviation of the distirbution.}

\item{uni: }{The uniform distribution is implemented as \deqn{f(s) = \frac{1}{p_2-p_1}} for \eqn{ p_1 \le s \le p_2}.}

\item{cauchy: }{The cauchy distribution is implemented as
\deqn{f(s)=\frac{1}{\pi p_2 \left \{1+( \frac{s-p_1}{p_2})^2\right \} } }
where \eqn{p_1} is the location parameter  and \eqn{p_2} the scale parameter.}

\item{fnorm: }{The half normal distribution is implemented as \deqn{2 f(s)-1} where \deqn{f(s) = \frac{1}{sd\sqrt{2 \pi} }e^{-\frac{s^2}{2 sd^2 }},} and \eqn{sd=\sqrt{\pi/2}/p_1}.}

\item{normmixt:}{The normal mixture distribution is implemented as

\deqn{f(s)=p_1\frac{1}{p_2[2] \sqrt{2\pi} }  e^{- \frac{ (s - p_2[1])^2}{2p_2[2]^2}} +(1-p_1)\frac{1}{p_2[4]\sqrt{2\pi}} e^{-\frac{(s - p_2[3])^2}{2p_2[4]^2 }} }

where \eqn{p1} is a mixture component(scalar) and \eqn{p_2} a vector of parameters for the mean and variance of the two mixture components \eqn{p_2= c(mean1, sd1, mean2, sd2)}.}

\item{skewnorm: }{The skew normal distribution with parameter \eqn{p_1} is implemented as \deqn{f(s)=2\phi(s)\Phi(p_1s)}.}

\item{fas: }{The Fernandez and Steel distribution is implemented as
\deqn{f(s; p_1, p_2) = \frac{2}{p_1+\frac{1}{p_1}} \left \{ f_t(s/p_1; p_2) I_{\{s \ge 0\}} + f_t(p_1s; p_2)I_{\{s<0 \}}\right \} }
where \eqn{f_t(x;\nu)} is the p.d.f. of the \eqn{t} distribution with \eqn{\nu = 5} degrees of freedom. \eqn{p_1} controls the skewness of the distribution with values between \eqn{(0, +\infty)} and \eqn{p_2} denotes the degrees of freedom.}

\item{shash: }{The Sinh-Arcsinh distribution is implemented as
\deqn{f(s;\mu, p_1, p_2, \tau) = \frac{ce^{-r^2/2}}{\sqrt{2\pi }} \frac{1}{p_2} \frac{1}{2} \sqrt{1+z^2} }
where \eqn{r=\sinh(\sinh(z)-(-p_1))}, \eqn{c=\cosh(\sinh(z)-(-p_1))} and \eqn{z=((s-\mu)/p2)}. \eqn{p_1} is the vector of skewness, \eqn{p_2} is the scale parameter, \eqn{\mu=0} is the location parameter and \eqn{\tau=1} the kurtosis parameter.}
}

  }


\value{A vector containing the user selected density values at the user specified points \code{s}.}

\references{
  \href{https://link.springer.com/chapter/10.1007\%2F978-3-319-41582-6_1}{{Bagkavos D., Patil P.N., Wood A.T.A. (2016), A Numerical Study of the Power Function of a New Symmetry Test. In: Cao R., Gonzalez Manteiga W., Romo J. (eds) Nonparametric Statistics. Springer Proceedings in Mathematics and Statistics, vol 175, Springer.}
}}

\author{
  Dimitrios Bagkavos and Lucia Gamez Gallardo

  R implementation and documentation: Dimitrios Bagkavos <dimitrios.bagkavos@gmail.com> ,Lucia Gamez Gallardo <gamezgallardolucia@gmail.com>
}


\seealso{ \code{\link{pdfsq}, \link{pdfsqcdf}, \link{pdfsqcdfstar} }  }

\examples{
  selected.dens <- "weib" #select Weibull
  shape <- 2  # specify shape parameter
  scale <- 1 # specify scale parameter
  xout <- seq(0.1,5,length=50)  #design point
  pdfthird(xout,selected.dens,shape,scale)  # calculate density to the cube at xout
}
