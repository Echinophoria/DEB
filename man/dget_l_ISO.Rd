\name{dget_l_ISO}
\alias{dget_l_ISO}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Growth for isometric adult between maturity thresholds.
}
\description{
Growth for isometric adult between maturity thresholds. It can be used to get length at puberty. Assumes isomorphic growth (VB)
}
\usage{
dget_l_ISO(vH, l, parms)
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
a vector containing the parameters: k: -, maintenance ratio; g: -, energy investment ratio; f: -, scaled functional response; s_M: -, acceleration factor for abj models;
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
l, scaled length during the maturity interval evaluated.
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
vH = c(p$v_Hj, p$v_Hp) # growth from metamorphosis to puberty
parms = c(p$k, p$g, f, ltR$s_M)
require(deSolve)
l = ode(ltR$lj, vH, dget_l_ISO, parms, method='ode45')
lp =l[2,2]


}
