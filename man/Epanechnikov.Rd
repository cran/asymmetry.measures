\name{Epanechnikov}
\alias{Epanechnikov}


\title{Epanechnikov kernel}

\description{Implementation of the Epanechnikov kernel.}

\usage{
  Epanechnikov(x)
}

\arguments{
  \item{x}{A vector of data points between \eqn{-\sqrt{5}} and \eqn{\sqrt{5}}  where the kernel will be evaluated.}

}

\details{
  Implements:

\deqn{K(u)= \frac{3}{4\sqrt{5}} \left (1-\frac{x^2}{5} \right )  }
for \eqn{|x| \le \sqrt{5}}
}

\value{
 The value of the kernel at \eqn{x}}

\references{
  \href{https://en.wikipedia.org/wiki/Kernel_(statistics)}{Kernel Statistics}
  }


  \author{
    Dimitrios Bagkavos and Lucia Gamez Gallardo

    R implementation and documentation: Dimitrios Bagkavos <dimitrios.bagkavos@gmail.com> , Lucia Gamez Gallardo <gamezgallardolucia@gmail.com>
  }



 \seealso{
    \code{\link{IntEpanechnikov}}
    }


