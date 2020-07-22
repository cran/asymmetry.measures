\name{SimpsonInt}
\alias{SimpsonInt}


\title{Simpson integration}

\description{
Implements simpson's extended integration rule.}

\usage{
  SimpsonInt(xin,h)
}

\arguments{
  \item{xin}{A vector of design points where the integral will be evaluated.}
  \item{h}{Assuming a<b and n is a positive integer. \eqn{h = (b-a)/n}.}

}

\details{

Simpson's extended numerical integration rule is implemented for \eqn{n+1} equally spaced subdivisions (where \eqn{n} is even) of \eqn{[a, b]} as

\deqn{\int_{a}^{b} f(x)\, dx = \frac{h}{3} \left \{ f(a) + 4f(x_1) + 2f(x_2) + 4f(x_3) + 2f(x_4) + ... + 4f(x_{n-1}) + f(b)\right \}}


where \eqn{hx=(b-a)/n} and \eqn{x_i=a+ihx}. Simpson's rule will return an exact result when the polynomial in question has a degree of three or less. For other functions, Simpson's Rule only gives an approximation.
}

\value{
A scalar, the approximate value of the integral.
}

\references{
\href{https://en.wikipedia.org/wiki/Simpson\%27s_rule}{Simpson's Rule}
}


\author{
    Dimitrios Bagkavos and Lucia Gamez Gallardo

    R implementation and documentation: Dimitrios Bagkavos <dimitrios.bagkavos@gmail.com> , Lucia Gamez Gallardo <gamezgallardolucia@gmail.com>
  }

  %\note{
    %%  ~~further notes~~
      %}

  %\seealso{
    %\code{\link{alfa.profile}, \link{alfa}, \link{alfainv}, \link{alpha.mle}
      %}
    %}

\examples{
x.in<- seq(0,pi/4,length=5)
h.out <- pi/8
SimpsonInt(x.in,h.out)
}
