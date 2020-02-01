\name{beta0}
\alias{beta0}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Particular incomplete beta function
}
\description{
Particular incomplete beta function
}
\usage{
beta0(x0, x1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x0}{
%%     ~~Describe \code{x} here~~
}
 \item{x1}{
%%     ~~Describe \code{x} here~~
}

}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
DEBlab, http://http://www.bio.vu.nl/thb/deb/deblab/

Kooijman SALM (2010) Dynamic Energy Budget theory for metabolic organisation, Third edn. Cambridge University Press, Cambridge, 509 pp.

Lika K, Kearney MR, Freitas V et al. (2011) The covariation method for estimating the parameters of the standard Dynamic Energy Budget model I: Philosophy and approach. Journal of Sea Research, 66, 270_277.

Lika K, Kearney MR, Kooijman SALM (2011) The covariation method for estimating the parameters of the standard Dynamic Energy Budget model II: Properties and preliminary patterns. Journal of Sea Research, 66, 278_288.
}
\author{
Antonio Aguera. Danish Shellfish Center, DTU-aqua, Nykoebing Mors, Denmark
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
beta0(0.1, 0.2)

## The function is currently defined as
function (x0, x1)
{
    x03 = x0^(1/3)
    x13 = x1^(1/3)
    a3 = sqrt(3)
    f1 = -3 * x13 + a3 * atan((1 + 2 * x13)/a3) - log(as.complex(x13 -
        1)) + log(1 + x13 + x13^2)/2
    f0 = -3 * x03 + a3 * atan((1 + 2 * x03)/a3) - log(as.complex(x03 -
        1)) + log(1 + x03 + x03^2)/2
    f = f1 - f0
    return(f)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

