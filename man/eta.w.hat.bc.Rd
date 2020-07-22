\name{eta.w.hat.bc}
\alias{eta.w.hat.bc}


\title{Asymmetry coefficient \eqn{\hat{\eta}} using boundary correction}

\description{
Implements the asymmetry coefficient \eqn{\hat{\eta}} of \href{https://link.springer.com/article/10.1007/s00362-011-0401-6}{Patil, Patil and Bagkavos (2012)}}

\usage{
  eta.w.hat.bc(xin, kfun)
}

\arguments{
  \item{xin}{A vector of data points - the available sample.}
  \item{kfun}{The kernel to use in the density estimate.}
}

\details{
Given a sample \eqn{X_1, X_2, \dots, X_n} from a continuous density function \eqn{f(x)} and distribution function \eqn{F(x)},  \eqn{\hat{\eta}} is defined by
\deqn{\hat{\eta}=-\frac{\sum_{i=1}^{n} {U_i V_i}-n\bar{U}\bar{V}}{\sqrt{(\sum_{i=1}^n{U_i^2-n\bar{U^2}})(\sum_{i=1}^n {V_i^2-n\bar{V^2}})}}}

  where

  \deqn{U_i = \hat{f}(X_i), \; V_i =\hat{F}(X_i), \; \bar{U}=n^{-1}\sum_{i=1}^n  U_i, \; \bar{V}=n^{-1}\sum_{i=1}^n V_i.}

\code{eta.w.hat.bc} uses reflection to correct the boundary bias issue of the kernel estimate \code{kde}.
}


\value{
 Returns a scalar, the estimate of \eqn{\hat{\eta}}.
 }


\references{
   \href{https://link.springer.com/article/10.1007/s00362-011-0401-6}{Patil, P.N., Patil, P.P. and Bagkavos, D., (2012), A measure of asymmetry. Stat. Papers, 53, 971-985.}
    }

\author{
  Dimitrios Bagkavos and Lucia Gamez Gallardo

  R implementation and documentation: Dimitrios Bagkavos <dimitrios.bagkavos@gmail.com>, Lucia Gamez Gallardo <gamezgallardolucia@gmail.com>
}

%\note{
  %%  ~~further notes~~
    %}

\seealso{
  \code{\link{eta.w.hat}, \link{eta.w.breve}, \link{eta.w.breve.bc}, \link{eta.w.tilde},\link{eta.w.tilde.bc}
  }
}

\examples{
eta.w.hat.bc(GDP.Per.head.dist.1995,Epanechnikov)
0.3463025 #estimate of etahat.bc
  }
