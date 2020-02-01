#'@export
get_food <- function(p, Xi){

  # function description

  # function to calculate clearance rates or searching rates considering several
  # sinthetizing units and food sources. it follows the criteria by Saravia et
  # al. 2011. Where mussels make no distinction between food items before
  # filtration/capture

  # For each food source there should be 3 parameters:
  # J_Xi_Fm <- mass units/d.cm^2, maximum surface area-specific filtration rate
  # rho_Xi  <- -, binding probability for the food item, i.e. the proportion of the food that its retained in the filtration system
  # J_Xi_Im <- mass units/d.cm^2, maximum surface area-specific ingestion rate


  # Xi will ordered from X0, X1, X2 or in alphabetic order if other names
  # are used. The *i* can be changed by number or a name, but parameters name should always preserve the same structure


  ## UNITS !!! ###############################################################
  # Food concentrations should be given by the area or volume unit of {Fm}!!!

  # for each food source X and J_X_Fm should share the same units. For example
  # if food concentration is C-mol/m3, J_X_Fm should be C-mol/d/cm2 !!!! However
  # these units can differ between food sources with a different unit for X0
  # (c-mol/m3) and X1(g/m3)

  # The function return a list with three values for each of the food sources
  # Clearance rates for each food item, J_Xi_F
  # Ingestion rates for each food item, J_Xi_I
  # Egestion rates for each food item, J_Xi_P

  ## CLEARANCE RATES

  sources <- length(Xi)  # determining the number of food sources

  # obtaining J_Xi_Fm from p
  J_Xi_Fm <- p[which(grepl('_Fm', names(p), fixed=TRUE),  arr.ind = TRUE)]
  nm <- names(J_Xi_Fm)

  for (i in 1:length(names)){
    a <- nm[[i]]
    nm[[i]] <- gsub(".*X(.+)_Fm.*", "\\1", nm)
  }

  J_Xi_Fm <- as.numeric(unlist(list(J_Xi_Fm[sort(names(J_Xi_Fm))])))

  Cr <- p$F_m / (1+ sum(Xi * p$F_m / J_Xi_Fm))  #

  J_Xi_F = c()   # store retention of each type of food
  for (i in 1:sources){
    J_Xi_F[i] <- Xi[i]*Cr
  }


  ### INGESTION & EGESTION

  # obtaining J_Xi_lm from p
  J_Xi_Im <- p[which(grepl('_Im', names(p), fixed=TRUE),  arr.ind = TRUE)]
  J_Xi_Im <- as.numeric(unlist(list(J_Xi_Im[sort(names(J_Xi_Im))])))

  # obtaining rho_Xi from p
  rho_Xi <- p[which(grepl('rho_', names(p), fixed=TRUE),  arr.ind = TRUE)]
  rho_Xi <- as.numeric(unlist(list(rho_Xi[sort(names(rho_Xi))])))

  J_Xi_I <- c()   # surface area-specific ingestion
  J_Xi_P <- c()   # surface area-specific pseudofaeces production

  for (i in 1:sources){
    J_Xi_I[i] <- rho_Xi[i]*J_Xi_F[i]/(1+sum((rho_Xi*J_Xi_F)/J_Xi_Im))
    J_Xi_P[i] <- J_Xi_F[i]- J_Xi_I[i]
  }


  # listing and name parameters
  J_Xi_F <- as.list(J_Xi_F)
  J_Xi_I <- as.list(J_Xi_I)
  J_Xi_P <- as.list(J_Xi_P)

  names(J_Xi_F) <- paste('J_X',nm,'_F', sep ='')  # clearance rates by food type.

  names(J_Xi_I) <- paste('J_X',nm,'_I', sep ='')  # Ingestion rates by food type.

  names(J_Xi_P) <- paste('J_X',nm,'_P', sep ='')  # Egestion rates by food type.


  return(list(J_Xi_F,J_Xi_I,J_Xi_P))

}
