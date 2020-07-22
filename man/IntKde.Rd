\name{IntKde}
\alias{IntKde}


\title{Integrated Kernel density estimator}

\description{Classical univariate integrated kernel density estimator
}

\usage{
  IntKde(xin, xout, h, kfun)
}

\arguments{
  \item{xin}{ A vector of data points - the available sample size.}
  \item{xout}{grid points where the distribution function will be estimated.}
   \item{h}{The bandwidth parameter. Defaults to \eqn{3.572*\sigma*n^{-1/3}} according to Bowman et al.(1998).}
  \item{kfun}{The kernel to use in the distribution function estimate.}

}

\details{
 It implements the classical density integrated kernel estimator.

 Let \eqn{X_1,X_2,\dots, X_n} be a univariate independent and identically distributed sample drawn from some unknown distribution function  \eqn{F}.  Its kernel density estimator is

 \deqn{\hat{F}(x)=  n^{-1}\sum_{i=1}^n K\left \{ (x-X_i)h^{-1}\right \}}


where \eqn{K} is an integrated kernel, and \eqn{h > 0}  is a smoothing parameter called the bandwidth.

}

\value{
Returns a vector with the estimate of the distribution function at the user specified grid points.
}

\references{
  \href{https://academic.oup.com/biomet/article/85/4/799/251730}{Bowman, A., Hall, P., and Prvan, T., (1998), Bandwidth Selection for the Smoothing of Distribution Functions,  Biometrika, 799-808.}
  }

  \author{
    Dimitrios Bagkavos and Lucia Gamez Gallardo

    R implementation and documentation: Dimitrios Bagkavos <dimitrios.bagkavos@gmail.com>, Lucia Gamez Gallardo <gamezgallardolucia@gmail.com>
  }



 \seealso{
    \code{\link{bw.nrd}}, \code{\link{bw.nrd0}}, \code{\link{bw.ucv}}, \code{\link{bw.bcv}}
    }

\examples{
x.in <- rnorm(100)
x.out <- seq(-3.4,3.4,length=60)
kernel <- IntEpanechnikov
dist.est <- IntKde(xin=x.in,xout=x.out,kfun=kernel)
plot(x.out,dist.est, type="l", col="red", main="Kernel c.d.f. estimator")
}
