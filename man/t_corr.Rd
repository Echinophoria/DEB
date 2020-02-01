\name{t_corr}
\alias{t_corr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Temperature Correction factor
%%  ~~function to do ... ~~
}
\description{
calculates de correction factor for the parameters and variables that are temperature dependent at the given temperature/s. DEB theory uses the Arrhenius concept of enzyme activation to incorporate the effect of temperature on the metabolic processes. The DEB model uses alternative variations of Arrhenius / van 't Hoff equations to calculate a curve for temperature sensitivity given by 5, 3 or only 1 parameter.
}
\usage{
t_corr(p, Ti)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p}{
a list containing a vector t.pars, as the one yielded using the function 'get_DEB_pars'. Alternative a vector containing 2 (T_ref, T_A), 4 (T_ref, T_A, T_L, T_AL) or 6 (T_ref, T_A, T_L, T_H, T_AL, T_AH) parameters
}
  \item{Ti}{
a single value or a vector containing the aimed temperature or temperature/s in Kelvins
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
 \item{TC }{A single value or a vector containing the correction factor/s for the given temperature/s}


}
\references{
DEBlab, http://http://www.bio.vu.nl/thb/deb/deblab/

Kooijman SALM (2010) Dynamic Energy Budget theory for metabolic organisation, Third edn. Cambridge University Press, Cambridge, 509 pp.

Lika K, Kearney MR, Freitas V et al. (2011) The covariation method for estimating the parameters of the standard Dynamic Energy Budget model I: Philosophy and approach. Journal of Sea Research, 66, 270_277.

Lika K, Kearney MR, Kooijman SALM (2011) The covariation method for estimating the parameters of the standard Dynamic Energy Budget model II: Properties and preliminary patterns. Journal of Sea Research, 66, 278_288.
}
\author{
Antonio Aguera. BioMar, ULB, Bruxelles, Belgium
}
\note{
%%  ~~further notes~~
}

\section{Warning }{Temperature parameters should be given in the same order referred in the argument section}

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
## with single input and list from get_DEB_pars()
#pars = get_DEB_pars('results_Odontaster_validus.mat')
#Ti = 273    #in Kelvins
#TC = t_corr(pars, Ti)  # only one value for TC

## with several temperatures and using a 3 parameter correction
#pars (293, 5303, 269, 120200)  # (T_ref, T_A, T_L, T_AL)
#Ti = c(273,284,290,278)    # in Kelvins
#TC = t_corr(pars, Ti)      # one TC value for each Ti
f=1
}
