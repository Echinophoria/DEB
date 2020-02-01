get_JXip <- function(p, Xi, J_Xi_F){
  # function to calculate ingestion and pseudofaeces production considering
  # captured food amounts and different food sources. sinthetizing units and
  # food sources. it follows the criteria by Saravia et al. 2011. Where mussels
  # make no distinction between food items before filtration/capture

  # For each food source there should be a J_Xi_F (food source filtration rate).
  # As calculated with the function get_CR() Also a {J_Xi_Im} (Maximum surface
  # area-specific ingestion rate) for each food source. Also a probability
  # (rho_Xi) of binding for each food source

  # J_Xi_Fm will ordered from X0, X1, X2 or in alphabetic order if other names
  # are used. They should always contain _Fm at the end. Xi should be provided
  # in the same order

  ## UNITS !!! ###############################################################
  # Food concentrations should be given by the area or volume unit of {Fm}!!!

  # for each food source X and J_X_lm should share the same units. For example
  # if food concentration is C-mol/m3, J_X_Fm should be C-mol/d/cm2 !!!! However
  # these units can differ between food sources with a different unit for X0
  # (c-mol/m3) and X1(g/m3)

  sources <- length(Xi)  # determining the number of food sources

  # obtaining J_Xi_lm from p
  J_Xi_Im <- p[which(grepl('_Im', names(p), fixed=TRUE),  arr.ind = TRUE)]
  J_Xi_Im <- as.numeric(unlist(list(J_Xi_Im[sort(names(J_Xi_Im))])))

  # obtaining rho_Xi from p
  rho_Xi <- p[which(grepl('rho_', names(p), fixed=TRUE),  arr.ind = TRUE)]
  rho_Xi <- as.numeric(unlist(list(rho_Xi[sort(names(rho_Xi))])))

  J_Xi_I <- c()   # surface area-specific ingestion
  J_Xi_p <- c()   # surface area-specific pseudofaeces production

  for (i in 1:sources){
    J_Xi_I[i] <- rho_Xi[i]*J_Xi_F[i]/(1+sum((rho_Xi*J_Xi_F)/J_Xi_Im))
    J_Xi_p[i] <- J_Xi_F[i]- J_Xi_I[i]
  }



  return(list(J_Xi_I=J_Xi_I, J_Xi_p=J_Xi_p)) # these are all surface area-specific rates. J_Xi_I(p) = food_units d-1 cm-2

}
