\name{get_tj}
\alias{get_tj}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Life cycle key variables for the model "abj"
}
\description{
This function calculates values for variables related to key events during the life cycle of the organism (birth, metamorphosis, maturity, growth). This case is restricted to the particular DEB model case 'abj', that considers metabolism acceleration during the larval stage which grows exponentially until reaching a metamorphosis event (see Kooijman, 2010)
}
\usage{
get_tj(p, f)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p }{
A named list resulting from the importing of DEB parameters using the function \code{\link{get_DEB_pars}}. Alternatively a vector containing the DEB parameters (g, k, v_Hb, v_Hj and v_Hp (optional)) in the same order. For symbol meaning check code and documentation of \code{\link{get_DEB_pars}}
}
  \item{f }{
Scaled functional response at which the variables are to be calculated (see Kooijman 2010 for a definition of f). It is a value between 0 and 1.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
This function return a named list containing scaled lengths and ages at the different key stages (birth, metamorphosis, puberty, etc)
\item{lb }{scaled length at birth, start of exogenous feeding}
\item{lj }{scaled length at metamorphosis}
\item{li }{scaled ultimate length (von Bertalanffy growth parameter)}
\item{tb }{scaled age at birth, start of exogenous feeding}
\item{tj }{scaled age at metamorphosis}
\item{ti }{scaled age at which animal reach li}
\item{s_M }{acceleration factor, see Kooijman, 2010}
\item{rj }{scaled growth rate at exponential phase (feeding larvae)}
\item{rB }{scaled von Bertalanffy growth rate}

Additionally if v_Hp is provided

\item{lp }{scaled length at puberty (moment when allocation to gonads starts)}
\item{tp }{scaled age at puberty}

}
\references{
Aguera A, Collard M, Jossart Q, Moreau C, Danis B (2015) Parameter Estimations of Dynamic Energy Budget (DEB) Model over the Life History of a Key Antarctic Species: The Antarctic Sea Star Odontaster validus Koehler, 1906 (ed Thuesen E V.). Plos One, 10, e0140078.

DEBlab, http://http://www.bio.vu.nl/thb/deb/deblab/

Kooijman SALM (2010) Dynamic Energy Budget theory for metabolic organisation, Third edn. Cambridge University Press, Cambridge, 509 pp.

Lika K, Kearney MR, Freitas V et al. (2011) The covariation method for estimating the parameters of the standard Dynamic Energy Budget model I: Philosophy and approach. Journal of Sea Research, 66, 270_277.

Lika K, Kearney MR, Kooijman SALM (2011) The covariation method for estimating the parameters of the standard Dynamic Energy Budget model II: Properties and preliminary patterns. Journal of Sea Research, 66, 278_288.
}
\author{
Antonio Aguera. BioMar, ULB, Bruxelles, Belgium
}
\note{
Scaled values obtained here can be unscaled for further use. See examples. For further info on temperature corrections see Kooijman 2010 and \code{\link{t_corr}}
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{t_corr}}
\code{\link{get_DEB_pars}}
}
\examples{
pars <- get_DEB_pars(system.file('data_examples_templates/results_Odontaster_validus.mat', package='DEB'))
f <- 1
ltr <- get_tj(pars, f)
  }


