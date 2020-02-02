#'@export

dget_VRE <- function (a, VRE, pars) {

  # This function calculates the growth and the changes in the 3 state variables
  # during the time period specified by a. When T or f or both are functions, it also
  # integrates as function of temperature ad f considerig their variation during the time
  # interval

  # a: scalar with time since start of the period.
  # VRe: 3-vector with starting values of V, Er and E.
  # dVRE: 3-vector with (dV/da, dR/da, dE/da)
  # p: a list containing all DEB parameters for the species
  # T: a scalar with the temperature in K or a regression model (T = f(a)) to obtain the temperature (in K) at time a for the correction during the integration
  # f: scaled functional response during a. It can be a scalar between 0 and 1 when food is constant during the integrated period or a function/regression model as a function of time.

  V = VRE[1]   # cm3, intial structural volume
  R = VRE[2]   # Joules, initial reproductive
  E = VRE[3]   # J, reserves at the start of the interval

  p=pars[[1]]
  Lp = pars[[2]]
  s_M = pars[[3]]
  T = pars[[4]]
  ft = pars[[5]]


  if (length(T)==1){
    TC=t_corr(p, T);
  }else{
    t=list(time=a)
    y.name=names(T$var.summary)
    names(t)=y.name
    T1 = predict(T, t, type='response')
    TC=t_corr(p, T1);
  }

  if (length(ft==1)){
    f=ft;
  }else{
    t=list(time=a)
    y.name=names(ft$var.summary)
    names(t)=y.name
    f = predict(ft, t, type='response')
  }


  L = V^(1/3)             # cm, structural length
  e = p$v * E / p$p_Am / V        # -, scaled reserves

  pA = TC * p$p_Am * L ^ 2 * s_M * f                                                # J, asimilation
  pC = TC * p$p_Am * L ^ 2 * ((p$g + p$L_T/ p$L_m) * s_M + L/ p$L_m)/(1 + p$g/e)    # J, mobilisation
  pS = TC * p$p_Am * p$kap * L^2 * (L + p$L_T/p$L_m)                              # J, somatic  maint
  pJ = TC * p$p_Am * p$k_J * p$U_Hp                                             # J, maturity  maint

  # generate dL/dt, dR/dt, de/dt
  if (p$kap*pC>=pS){    #kappa rule
    dV = (pC * p$kap - pS) / p$E_G;             # cm^3 of structure
    dR = ((1 - p$kap) * pC - pJ)*(L>=Lp);             # J, maturation/reproduction dR only happens when the animal has reached minimum size.
  } else {            # priority order maintenance > reproduction > growth
    dR = (pC - pS - pJ)*(L>=Lp);                    # J, maturation/reproduction
    dV = 0;                                 # cm^3 of structure
    if (dR < 0){
      if  (R > 0){    # maintenance will be covered by the reproduction buffer until this is exhausted
        if (R > abs(dR)){
          dV = 0
        }else{
          dV = (R+dR)/E_G
          dR = -R
        }
      }else{
        dV = dR / p$E_G                         # cm^3 of structure
        dR = 0;                               # J, maturation/reproduction
      }
    }
  }

  dE = pA - pC                 # -, change in reserves

  return(list(c(dV,dR,dE)))
}
