\name{eta.s.exact}
\alias{eta.s.exact}


\title{Strong asymmetry measure \eqn{eta(X)}.}

\description{Returns the strong asymmetry measure \eqn{eta(X)} of \href{https://link.springer.com/article/10.1007/s13171-013-0034-z}{Patil, Bagkavos and Wood (2014)}.
}

\usage{
  eta.s.exact(xin, dist, GridLength, p1, p2)
}

\arguments{
  \item{xin}{A vector of data points - the available sample.}
  \item{dist}{Character string, specifies selected distribution function.}
  \item{GridLength}{A non-negative number, which will be rounded up if fractional.Desired length of the sequence. }
  \item{p1}{A scalar. Parameter 1 (vector or object) of the selected distribution.}
  \item{p2}{A scalar. Parameter 2 (vector or object) of the selected distribution.}
}

\details{
  Implements
  \deqn{\eta(X)= -0.5 sign(\rho_1)\max|\rho_p + \rho_p^*|} with \eqn{1/2 \le p \le 1}
  This version uses exact p.d.f. and c.d.f. evaluation and not estimates of the unknown functionals.}


\value{Returns a scalar, the value of the strong asymmetry measure \eqn{\eta(X)}.}

\references{
  \itemize{
    \item   \href{https://link.springer.com/article/10.1007/s13171-013-0034-z}{Patil P.N., Bagkavos D. and Wood A.T.A., (2014). A measure of asymmetry based on a new necessary and sufficient condition for symmetry, Sankhya A, 76, 123--145.}
    \item  \href{https://link.springer.com/chapter/10.1007/978-3-319-41582-6_1}{{Bagkavos D., Patil P.N., Wood A.T.A. (2016), A Numerical Study of the Power Function of a New Symmetry Test. In: Cao R., González Manteiga W., Romo J. (eds) Nonparametric Statistics. Springer Proceedings in Mathematics and Statistics, vol 175, Springer.}
  }}}


\author{
  Dimitrios Bagkavos and Lucia Gamez Gallardo

  R implementation and documentation: Dimitrios Bagkavos <dimitrios.bagkavos@gmail.com> , Lucia Gamez Gallardo <gamezgallardolucia@gmail.com>
}



\seealso{
  \code{\link{eta.w.hat.bc}, \link{eta.w.hat}, \link{eta.w.breve},\link{eta.w.breve.bc}, \link{eta.w.tilde},\link{eta.w.tilde.bc},\link{eta.s}
  }}

\examples{

selected.dist <- "norm" #select norm as the distribution
m.use <- 2
sd.use<- 2
grid <- 50
s.use<- rnorm(100)
eta.s.exact(s.use,selected.dist,grid,m.use,sd.use)  # calculate eta.s at xout

}
