\name{get_CR}
\alias{get_CR}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get clearance rates for Xi sources
}
\description{
Get clearance rate for several suspended particles, either edible (algae) or non-edible (particulate inorganic matter)
}
\usage{
get_CR(p, Xi)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p}{
A named list resulting from the importing of DEB parameters using the function. Alternatively a vector containing the DEB parameters (g, k, v_Hb, v_Hj and v_Hp (optional)) in the same order. For symbol meaning check code and documentation of
}
  \item{Xi}{
A vector containing the food sources in the same order that J_Xi_Fm.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
a vector containiong clearance rates
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
me
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x)
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ clearance rate }% use one of  RShowDoc("KEYWORDS")
\keyword{ foods }% __ONLY ONE__ keyword per line
