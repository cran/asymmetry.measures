\name{IntEpanechnikov}
\alias{IntEpanechnikov}


\title{Integrated Epanechnikov function}

\description{
 Implements the Integrated Epanechnikov kernel.}

\usage{
  IntEpanechnikov(x)
}

\arguments{
  \item{x}{A vector of design points with values from \eqn{-\sqrt{5}} to \eqn{\sqrt{5}}.}

}

\details{


  Implements:

\deqn{K(u)= \int_{-Inf}^{u}{(3/4)*(1-((x^2)/5 ))/\sqrt{5} dx}}
for \eqn{|x|<=\sqrt{5}}

}

\value{

  The value of the integrated kernel function at the user designated points.
}

\references{
  \href{https://en.wikipedia.org/wiki/Kernel_(statistics)}{Kernel Statistics}
  }


  \author{
    Dimitrios Bagkavos and Lucia Gamez Gallardo

    R implementation and documentation: Dimitrios Bagkavos <dimitrios.bagkavos@gmail.com> , Lucia Gamez Gallardo <gamezgallardolucia@gmail.com>
  }

  %\note{
    %%  ~~further notes~~
      %}

  \seealso{
    \code{\link{Epanechnikov}
      }
  }

  %\examples{
    %
    %}
