\name{kde}
\alias{kde}


\title{Kernel density estimator.}

\description{Classical univariate kernel density estimator.
}

\usage{
  kde(xin, xout, h, kfun)
}

\arguments{
 \item{xin}{A vector of data points.  Missing values not allowed.}
 \item{xout}{A vector of grid points at which the estimate will be calculated. }
 \item{h}{A scalar, the bandwidth to use in the estimate, e.g. \code{bw.nrd(xin)}.}
 \item{kfun}{Kernel function to use.}

}

\details{

Implements the classical density kernel estimator based on a sample \eqn{X_1,X_2,.., X_n} of i.i.d observations  from a distribution  \eqn{F} with density \eqn{h}. The estimator is defined by

 \deqn{\hat{f}(x)= n^{-1}\sum_{i=1}^n  K_h(x-X_{i}) }
where \eqn{h} is determined by  a bandwidth selector such as Silverman's default plug-in rule and   \eqn{K}, the kernel, is a non-negative probability density function.

}

\value{
A vector with the density estimates at the designated points xout. }

\references{
  \href{https://www.routledge.com/Density-Estimation-for-Statistics-and-Data-Analysis-1st-Edition/Silverman-Cox-Reid-Isham-Tibshirani-Louis-Tong-Keiding/p/book/9780412246203}{Silverman,B.W.(1986) Density Estimation for Statistics and Data Analysis.}
  }


  \author{
    Dimitrios Bagkavos and Lucia Gamez Gallardo

    R implementation and documentation: Dimitrios Bagkavos <dimitrios.bagkavos@gmail.com> , Lucia Gamez Gallardo <gamezgallardolucia@gmail.com>
  }

  %\note{
    %%  ~~further notes~~
      %}

  \seealso{
    \code{\link{bw.nrd}}, \code{\link{bw.nrd0}}, \code{\link{bw.ucv}}, \code{\link{bw.bcv}}
    }

\examples{
x.in <- rnorm(100)
x.out <- seq(-3.4,3.4,length=60)
bandwidth <- bw.nrd(x.in)
kernel <- Epanechnikov
dens.est <- kde(x.in,x.out,bandwidth,kernel)
plot(x.out,dens.est,col="red",main="Kernel density estimator")
}
