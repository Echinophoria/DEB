\name{dget_VRE}
\alias{dget_VRE}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate changes on state variables over a time interval
}
\description{
This function calculates the growth and the changes in the 3 state variables during the time period specified by a. When T or f or both are functions, it also integrates as function of temperature ad f considerig their variation during the time interval
}
\usage{
dget_VRE(a, VRE, pars)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{a}{
a vector containing the time point from the start at which the function has to be evaluated. Should be in days.
}
  \item{VRE}{
a 3 vector with the starting values of V (structure volume, cm^3), R (accumulated reproductive buffer, J) and E (energy reserves, J)
}
  \item{pars}{
a list containing:
    p: a list containing all the DEB parameters for the species. Called by the get_DEB_pars function.
    Lp = structural length at puberty, for the function to determine when the accumulation of R starts.
    s_M = acceleration factor.
    T: a scalar with the temperature in K or a regression model (T = f(a)) to obtain the temperature (in K) at time a for the correction during the integration.
     f: scaled functional response during a. It can be a scalar between 0 and 1 when food is constant during the integrated period or a function/regression model as a function of time.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  \item{VRE}{
  a list with three vectors corresponding to the values of V, R and E at the time points (a) at which the function is evaluated.
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
a <- seq(1,365,1) # to evaluate de changes daily
Lw <- 5 # cm, physical length of the individual to be evaluated.
e <- 0.8 # -, scaled reserves at the start of the evaluation.
ltR <- get_tj(p, e) # calculates key events for the initial conditions.
Lp <- ltR$lp * p$L_m
V <- (Lw*p$del_M)^3  # cm^3, initial structural volume
R <- 0 # onset of gametogenesis no reproductive buffer accumulated yet.
E <- e * p$E_m * V   # J, initial reserve.
VRE_0 <- c(V, R, E)
pars <- list(p, Lp, ltR$s_M, 283, 0.5)
require(deSolve)
t_VRE <- ode(VRE_0, a, dget_VRE, pars, method='ode45')
t_VRE <- as.data.frame(t_VRE)
names(t_VRE)<-c('Days',"V", "Er","E")
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Dynamic Energy Budget }
\keyword{ Parameter }% __ONLY ONE__ keyword per line
