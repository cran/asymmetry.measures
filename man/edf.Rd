\name{edf}
\alias{edf}


\title{Empirical cummulative distribution function}

\description{Empirical (nonparametric) cummulative distribution function for given a random sample.}

\usage{ edf(xin, xout) }

\arguments{
  \item{xin}{A vector of data points - the available sample.}
  \item{xout}{A vector of design points where the distribution function will be estimated.}
}

\details{ The empirical distribution function estimator at \eqn{x} is defined as the number of observations up to \eqn{x}, divided by \eqn{n}, i.e.
    \deqn{ F_{n}(x) = \frac{\# of X_{1},..,X_{n}<= x}{n}}.
}


\value{A vector with the estimated distribution function at \code{xout}.}

\references{
  \href{https://onlinelibrary.wiley.com/doi/pdf/10.1002/9781119196037}{Hollander, M. & Wolfe,D.A. (1999) Nonparametric Statistical Methods, 2nd edition}
  }


\author{
 Dimitrios Bagkavos and Lucia Gamez Gallardo

R implementation and documentation: Dimitrios Bagkavos <dimitrios.bagkavos@gmail.com> , Lucia Gamez Gallardo <gamezgallardolucia@gmail.com>
}


\examples{
x.in <- rexp(200)
x.out <- seq(0.1,5,length=60)
dist.est <- edf(x.in,x.out)
plot(x.out,dist.est,col="blue",main="Empirical c.d.f.",xlab="x",yla ="probability")
}
