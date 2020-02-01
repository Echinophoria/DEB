\name{get_uE0}
\alias{get_uE0}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get Initial Scaled Reserves
}
\description{
This function calculates the initial scaled reserves, i.e. the scaled reserves of the egg considering the mother scaled energy reserves and the length at birth
}
\usage{
get_uE0(p, e)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p}{
a list containing all the DEB parameters for the species. Called by the get_DEB_pars function.
}
  \item{e}{
scaled energy reserves for the mother.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\item{uE0}{
  d cm^2, a scalar with the scaled initial reserve of the egg. It is un-scaled by multiplying it for {p_Am}
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
e<-1
U_E0 <- get_uE0(p, e)   # d.cm^2, initial scaled reserve
Wd_0 <- 1e6 * U_E0 * p$p_Am * p$w_E / p$mu_E   # ug, initial (=egg) dry weight
}
