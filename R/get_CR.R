get_CR <- function(p, Xi){
  # function to calculate clearance rates or searching rates considering several
  # sinthetizing units and food sources. it follows the criteria by Saravia et
  # al. 2011. Where mussels make no distinction between food items before
  # filtration/capture

  # For each food source there should be a J_Xi_Fm (maximum surface
  # area-specific filtration rate) co

  # J_Xi_Fm will ordered from X0, X1, X2 or in alphabetic order if other names
  # are used. They should always contain _Fm at the end. Xi should be provided
  # in the same order


  ## UNITS !!! ###############################################################
  # Food concentrations should be given by the area or volume unit of {Fm}!!!

  # for each food source X and J_X_Fm should share the same units. For example
  # if food concentration is C-mol/m3, J_X_Fm should be C-mol/d/cm2 !!!! However
  # these units can differ between food sources with a different unit for X0
  # (c-mol/m3) and X1(g/m3)

  sources <- length(Xi)  # determining the number of food sources

  # obtaining J_Xi_Fm from p
  J_Xi_Fm <- p[which(grepl('_Fm', names(p), fixed=TRUE),  arr.ind = TRUE)]
  J_Xi_Fm <- as.numeric(unlist(list(J_Xi_Fm[sort(names(J_Xi_Fm))])))

  Cr <- p$F_m / (1+ sum(Xi * p$F_m / J_Xi_Fm))  #

  J_Xi_F = c()   # store retention of each type of food
  for (i in 1:sources){
    J_Xi_F[i] <- Xi[i]*Cr
  }
  return(list(CR=Cr, J_Xi_F=J_Xi_F))  # these are all surface area-specific rates. CR = l d-1 cm-2, J_Xi_F = food_units d-1 cm-2
}
