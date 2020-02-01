\name{get_food}
\alias{get_food}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
get filtration and ingestion rates for Xi food items
}
\description{
This function calculates clearance rates or area searching rates considering several sinthetizing units and food sources. It follows the criteria by Saravia et al. 2011. Where mussels make no distinction between food items before filtration/capture
}
\usage{
get_food(p, Xi)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p}{
A named list resulting from the importing of DEB parameters using the function \code{\link{get_DEB_pars}}. Alternatively a vector containing the DEB parameters (g, k, v_Hb, v_Hj and v_Hp (optional)) in the same order. For symbol meaning check code and documentation of \code{\link{get_DEB_pars}}
}
  \item{Xi}{
A vector with the different potential food items concentrations (per volume) or densities (per area).
Food concentrations should be given by the area or volume unit of {Fm}!!!
For each food source X and J_X_Fm should share the same units. For example if food concentration is C-mol/m3, J_X_Fm should be C-mol/d/cm2 !!!! Howeverthese units can differ between food sources with a different unit for X0 (c-mol/m3) and X1(g/m3). Provide them following the alphabetical or numerical order of the "i" names of each food item
}
}
\details{
For each food source there should be 3 parameters:
  # J_Xi_Fm <- mass units/d.cm^2, maximum surface area-specific filtration rate
  # rho_Xi  <- -, binding probability for the food item, i.e. the proportion of the food that its retained in the filtration system
  # J_Xi_Im <- mass units/d.cm^2, maximum surface area-specific ingestion rate


Xi will ordered from X0, X1, X2 or in alphabetic order if other names are used. The *i* can be changed by number or a name, but parameters name should always preserve the same structure. So Xi should provide the different food sources considering that order.


## UNITS !!! Food concentrations should be given by the area or volume unit of its {Fm}!!!

For each food source X and J_X_Fm should share the same units. For example if food concentration is C-mol/m3, J_X_Fm should be C-mol/d/cm2 !!!! However these units can differ between food sources with a different unit for X0 (c-mol/m3) and X1(g/m3)
}
\value{
The function return a list with three values for each of the food sources
 \item{J_Xi_F}{Clearance / area searching rates for each food item}
 \item{J_Xi_I}{Ingestion rates for each food item}
 \item{J_Xi_P}{Egestion rates for each food item}
}
\references{
Saraiva, S., van der Meer, J., Kooijman, S.A.L.M., Sousa, T., 2011. Modelling feeding processes in bivalves: A mechanistic approach. Ecol. Modell. 222, 514–523. https://doi.org/10.1016/j.ecolmodel.2010.09.031
}
\author{
Antonio Agüera,
Havforskningsinstituttet, Norway
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
## each substrate requires three parameters:

p <- get_DEB_pars(system.file('data_examples_templates/parameters_Mytilus_edulis.csv', package='DEB'))
p$J_Xpoc_Fm <- 4.8e-04 # C-mol/d.cm2, maximum retention rate for substrate POC
p$rho_Xpoc  <- 0.99  # -, probability of binding of substrate POC.
p$J_Xpoc_Im <- 1.4e-04 # C-mol/d.cm2, maximum ingestion rate

Xi= 2.06e-05  # C-mol/l, available food

J_Xi <- get_food(p, Xi)
}

