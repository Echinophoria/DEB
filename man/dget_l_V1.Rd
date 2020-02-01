\name{dget_l_V1}
\alias{dget_l_V1}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Growth for v1 morph between maturity thresholds
}
\description{
Calculates the growth assuming a v1 morph. Exponential growth during the accelerated stage in the case of abj models. This function can be used for the calculation of the length at metamorphosis.
}
\usage{
dget_l_V1(vH, l, parms)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
   \item{vH}{
a vector with the maturity levels at different stages thresholds to calculate growth between them.
}
  \item{l}{
initial scaled length at the starting maturity level.
}
  \item{parms}{
a vector containing the parameters: k: -, maintenance ratio; g: -, energy investment ratio; f: -, scaled functional response; lref: -, reference scaled length, also the length at the begining of the exponential growth, in many cases equal to l.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
l, scaled length during the maturity interval evaluated. The last value will be the length at the ending maturity threshold.
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
f = 1
ltR <- get_tj(p, f)  # this function already makes uses of dget_l_ISO
vH = c(p$v_Hb, p$v_Hj) # growth from birth to metamorphosis
parms = c(p$k, p$g, f, ltR$lb)
require(deSolve)
l = ode(ltR$lb, vH, dget_l_ISO, parms, method='ode45')
lj =l[2,2]


}
