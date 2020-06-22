\name{eta.w.breve}
\alias{eta.w.breve}


\title{Asymmetry coefficient \eqn{\breve{\eta}}}

\description{
  Implements the asymmetry coefficient \eqn{\breve{\eta}} of \href{https://link.springer.com/article/10.1007/s00362-011-0401-6}{Patil, Patil and Bagkavos (2012)}. }

\usage{
  eta.w.breve(xin, kfun)
}

\arguments{
  \item{xin}{A vector of data points - the available sample.}
  \item{kfun}{The kernel to use in the density estimate.}
}

\details{Given a sample \eqn{X_1, X_2, . . . , X_n} from a continuous density function \eqn{f(x)} and distribution function \eqn{F(x)}. \eqn{\breve{\eta}} is defined by \deqn{\breve{\eta}=-\frac{\sum^{n}_{i=1}{U_i*W_i}-n*\bar{U}*\bar{W}}{\sqrt{(\sum^{n}_{i=1}{U_i^2-n*\bar{U^2}})(\sum^{n}_{i=1}{W_i^2-n*\bar{W^2}})}}}

  where

 \eqn{U_i = \hat{f}(X_i)}, \eqn{W_i =F_n(X_i)}, \eqn{\bar{U}=\sum^{n}_{i=1}{\frac{U_i}{n}}}
  and \eqn{\bar{W}=\sum^{n}_{i=1}{\frac{W_i}{n}}}
}


\value{
 Returns a scalar, the estimate of \eqn{\breve{\eta}}.}


\references{
  \href{https://link.springer.com/article/10.1007/s00362-011-0401-6}{Patil, Patil and Bagkavos (2012): A measure of asymmetry}
    }


\author{
  Dimitrios Bagkavos and Lucia Gamez Gallardo

  R implementation and documentation: Dimitrios Bagkavos <dimitrios.bagkavos@gmail.com> , Lucia Gamez Gallardo <gamezgallardolucia@gmail.com>
}

%\note{
  %%  ~~further notes~~
    %}

\seealso{
  \code{\link{eta.w.hat.bc}, \link{eta.w.hat}, \link{eta.w.breve.bc}, \link{eta.w.tilde},\link{eta.w.tilde.bc}
  }
}

\examples{

eta.w.breve(GDP.Per.head.dist.1995,Epanechnikov)
0.329707 #estimate of etabreve

  }
