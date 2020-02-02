\name{get_powers_j}
\alias{get_powers_j}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
scaled powers for abj model
}
\description{
This function calculates scaled powers assimilation, mobilisation, somatic maintenance, maturity maintenance, growth, reproduction and dissipation as function of length and scaled reserves
}
\usage{
get_powers_j(p, L, e, s_M)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p }{
A named list resulting from the importing of DEB parameters using the function \code{\link{get_DEB_pars}}. Alternatively a vector containing the DEB parameters (g, k, v_Hb, v_Hj and v_Hp (optional)) in the same order. For symbol meaning check code and documentation of \code{\link{get_DEB_pars}}
}
  \item{L}{
Structural length of the organism for which the powers are being calculated (it can be a vector)
}
  \item{e}{
Scaled reserves at which the variables are to be calculated (see Kooijman 2010 for a definition of e). It is a value between 0 and 1.
}
  \item{s_M}{
Acceleration factor for abj model.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
This function return a vector/matrix containing s scaled powers p_A, p_C, p_S, p_J, p_G, p_R, p_D
They can be unscaled by * L_m^2 {p_Am}
p_A = assimilation power;
p_C = mobilisation power;
p_S = somatic manteinance power;
p_J = maturity mainenance power;
p_G = growth power;
p_R = reproduction power;
p_D = dissipation power;
}
\references{
Kooijman SALM (2010) Dynamic Energy Budget theory for metabolic organisation, Third edn. Cambridge University Press, Cambridge, 509 pp.

Lika K, Kearney MR, Freitas V et al. (2011) The covariation method for estimating the parameters of the standard Dynamic Energy Budget model I: Philosophy and approach. Journal of Sea Research, 66, 270_277.

Lika K, Kearney MR, Kooijman SALM (2011) The covariation method for estimating the parameters of the standard Dynamic Energy Budget model II: Properties and preliminary patterns. Journal of Sea Research, 66, 278_288.
}

\author{
Antonio Aguera. BioMar, ULB, Bruxelles, Belgium (for R version)
}
\note{
Scaled values obtained here can be unscaled for further use. See examples. For further info on temperature corrections see Kooijman 2010 and \code{\link{t_corr}}
}

\seealso{
\code{\link{t_corr}}
\code{\link{get_DEB_pars}}
}
\examples{
## get the parameters for the species
p <- get_DEB_pars(system.file('data_examples_templates/results_Odontaster_validus.mat', package='DEB'))

e <- 1   # maximum scaled reserve (no food limitation)
L <- 5 * p$del_M   # structural length from physical length (5cm arm length)
pACSJGRD <- get_powers_j(p, L, e)    # if s_M is not given it will be yielded by the function.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
