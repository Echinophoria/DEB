#'@export
get_food_lrv <- function(p, Xi){

  # [f CR JX] = <../get_CR.m *get_CR*>(CR_m, Xk, X)

  ## Description

  # Calculates the area specific clearance rate at food concentration.
  # Based on a Hollings type III function with a linear increase at low
  # concentrations of food and decrease from Xk towards infinity. Depending
  # on the DEB model tipe feeding flux. Filtration and feeding fluxes may need
  # to be corrected by the acceleration factor (s_M).

  # Input
  # * p a vector including the feeding parameters
  #     - CR_m: maximum specific clearance rate for X (l/d/cm2)
  #     - Xk: food concentration at which CR = CR_m (J/l)
  #     - rho_X: energy density of food item J/mass (depend of X units)

  # * X: a scalar or a vector with food concentrations for which
  #      to calculate clearance rates. (mass/l)
  #
  # Output
  # * f: a scalar or a vector with scaled functional response corresponding
  #       to each X
  # * CR: a scalar or a vector (depending of X) with the surface specific
  #       clearance rates corresponding to each X
  # * JX: a scalar or a vector with the surface specific ingestion rates
  #       for each X
  X = Xi * p$rho_POC # J/l, convert food concentration as mass to joules
  JXAm = p$CR_m_lrv * 2 * p$Xk_lrv # J/d cm^2, maximum surface specific feeding flux. (Maximum ingestion rate)
  f = p$f_lrv * (X^2/(X^2 + p$Xk_lrv^2)) # -,  scaled functional response at X for ingestion and clearance
  JX = JXAm * f # J/d cm^2, surface specific ingestion rate
  CR = JXAm * X / (X^2 + p$Xk_lrv^2)  #  l/d cm^2; clearance rate at X

  out = list(f, JX, CR)
  return(out)

}
