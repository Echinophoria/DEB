#'@export

VHE_larva <- function (a, VHE) {
  L = V^(1/3)  # structural length
  e = p$v * E/p$p_Am/V  # scaled reserves

  s_M = L/Lb # acceleration occurs during this stage and s_M needs to be updated

  # acceleration factor to change it increases during larval development and it
  # links the embryo metabolism to the adults. This acceleration explains the
  # exponential growth during the larval stage and it is determined by food,
  # hence larvae at different food levels will produce adults with different
  # metabolism

  pA = TC * p$p_Am * L^2 * s_M * f  # Joules, assimilation power, it depends on available food, abundance and type
  pC = TC * p$p_Am * L^2 * ((p$g + p$L_T/p$L_m) * s_M + L/p$L_m)/(1 + p$g/e) # Joules, mobilisation power, depends on animal condition (reserves, e)
  pS = TC * p$p_Am * p$kap * L^2 * (L + p$L_T/p$L_m)  # Joules, cost of somatic maintenance, depends on the size of the animal, V (structural volume)
  pJ = TC * p$k_J * H  # Joules, cost of maintaining maturity, it is a small cost related to maintain complexity, but it comes from the reproductive investment

  if (p$kap * pC >= pS) {  # there is energy for somatic maintenance and maybe for growth, kappa rule fully applies
    dV = (pC * p$kap - pS)/p$E_G
    dH = ((1 - p$kap) * pC - pJ)
  }
  else { # kap*pC cannot cover somatic maintenance, kappa rule no longer applies and priority S>J>R>G is used. There is no growth and there may be no investment on maturity
    dH = (pC - pS - pJ)
    dV = 0
    if (dH < 0) {
      # when there isn't energy enough to cover maintenance, the animal either
      # dies or uses its tissue. In the case of mussels tissue is used, first
      # the reproductive tissue, when exausted the structure is used. At this
      # stage there is no reproductive tissue. So only energy from structure can
      # be used to keep the animal alive.
      dV = dR/p$E_G   # Start of using tissue. Animal shrink! shell cannot shrink so is only the somatic tissue that is shrinking. That is why we follow V and L and not physical length.
      dH = 0    # there is no energy to keep maturing.
    }
  }
  dE = pA - pC  # update the energy in the reserves (assimilated - mobilised)

  # updating state variables
  H <- H + dH/24
  V <- V + dV/24
  E <- E + dE/24

  return(c(V,H,E))
  }
