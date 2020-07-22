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
\item{weib: }{The quantile function for the weibull distribution is implemented as \deqn{Q(s) =   p_1 (-\log(1-s))^{1/{p_2}}}
where \eqn{p_1} is the shape parameter and \eqn{p_2} the scale parameter.}

\item{lognorm: }{The lognormal distribution has quantile function implemented as
\deqn{Q(s)= \exp\left \{ p_1 +\sqrt{2p_2^2} \mathrm{erf}^{-1} (2s-1) \right \} }
where \eqn{p_1} is the mean, \eqn{p_2} is the standard deviation and \eqn{\mathrm{erf}} is the Gauss error function.}

\item{norm: }{The normal distribution has quantile function implemented as \deqn{Q(p)=\Phi^{-1}(s; p_1, p_2)} where \eqn{p_1} is the mean and the \eqn{p_2} is the standard deviation.}

  \item{uni: }{The uniform distribution has quantile function implemented as \deqn{Q(s; p_1, p_2)=s(p_2-p_1)+p_1} for  \eqn{p_1 < s < p_2}.}

  \item{cauchy: }{The cauchy distribution has quantile function implemented as
  \deqn{Q(s)=p_1 + p_2   \tan \left \{ \pi \left (s- \frac{1}{2} \right ) \right \}}
  where \eqn{p_1} is the location parameter and \eqn{p_2} the scale parameter.}

  \item{fnorm: }{The half normal distribution has quantile function implemented as
  \deqn{Q(s)= p_1\sqrt{2} \mathrm{erf}^{-1}(s) }
  where   and \eqn{p_1} is the standard deviation of the distribution.}

  \item{normmix: }{The quantile function normal mixture distribution is estimated numericaly, based on the built in quantile function.}

  \item{skewnorm: }{There is no closed form expression for the quantile function of the skew normal distribution. For this reason, the quantiles are calculated through the \code{qsn} function of the  sn  package.
  }

  \item{fas:}{There is no closed form expression for the quantile function of the Fernandez and Steel distribution.  For this reason, the quantiles are calculated through the \code{qskt} function of the skewt  package.}

  \item{shash:}{There is no closed form expression for the quantile function of the  Sinh-Arcsinh distribution. For this reason, the quantiles are calculated through the \code{qSHASHo} function of the   gamlss package.}
}}

\value{A vector containing the quantile values at the user specified points \code{s}.}

\references{
  \href{https://link.springer.com/chapter/10.1007/978-3-319-41582-6_1}{Bagkavos D., Patil P.N., Wood A.T.A. (2016), A Numerical Study of the Power Function of a New Symmetry Test. In: Cao R., Gonzalez Manteiga W., Romo J. (eds) Nonparametric Statistics. Springer Proceedings in Mathematics and Statistics, vol 175, Springer.}
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
