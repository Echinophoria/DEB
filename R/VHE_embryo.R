#'@export

VHE_embryo <- function (a, VHE) {
  ## Embryo stage, H < Hb
  # there is no feeding in this stage, this part of the code may need to be applied in steps shorter than days... for the mussel is 0.2 days at 18C.
    L = V^(1/3)  # cm, structura length
    e = p$v * E/p$p_Am/V  # scaled reserves
    s_M = 1  # during embryo development there is no acceleration
    pA = 0 # Joules, there is no assimilation, it is a non feeding stage.
    pC = TC * p$p_Am * L^2 * ((p$g + p$L_T/p$L_m) * s_M + L/p$L_m)/(1 + p$g/e) # Joules, mobilisation power, depends on animal condition (reserves, e)
    pS = TC * p$p_Am * p$kap * L^2 * (L + p$L_T/p$L_m) # Joules, cost of somatic maintenance, depends on the size of the animal, V (structural volume)
    pJ = TC * p$k_J * H  # Joules, cost of maintaining maturity, it is a small cost related to maintain complexity, but it comes from the reproductive investment

    if (p$kap * pC >= pS) {  # there is energy for somatic maintenance and maybe for growth, kappa rule fully applies
      dV = (pC * p$kap - pS)/p$E_G
      dH = ((1 - p$kap) * pC - pJ)
    }else { # kap*pC cannot cover somatic maintenance, kappa rule no longer applies and priority S>J>R>G is used. There is no growth and there may be no investment on maturity
      dH = (pC - pS - pJ)
      dV = 0
      if (dH<0){ # this is mortality relative to the condition of the egg being to bad to produce a feeding larvae
        warning('embryo cannot develop to metamorphosis')
        break()
      }
    }
    dE = - pC  # update the energy in the reserves (assimilated - mobilised)


    # updating state variables
    H <- H + dH/24  # all DEB parameters involving rates are by day. This is so we reduce the step to hour. Is done is all stages
    V <- V + dV/24
    E <- E + dE/24


    if (E<=0){
      warning('embryo cannot develop to metamorphosis')
      break()
    }
    return(c(V,H,E))
  }
