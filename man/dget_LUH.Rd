\name{dget_LUH}
\alias{dget_LUH}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Structural length, reserves and maturation during embryonic development
}
\description{
Calculates the development of structure and the changes in reserves and maturity during the embryonic development (before feeding, i.e. birth)
}
\usage{
dget_LUH(a, LUH, p)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{a}{
a vector witht the time points were the state variables will be evaluated along the interval integrated
}
  \item{LUH}{
a 3-vector containing the starting values for the structural length, scaled reserves and scaled maturity of the embryo
}
  \item{p}{
a list containing all the DEB parameters for the species. Called by the get_DEB_pars function.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
 \item{LUH}{
a list with 4-vectors: L, U and H at the time points provided by a
}
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
p <- get_DEB_pars(system.file('data_examples_templates/results_Odontaster_validus.mat', package='DEB'))

}
