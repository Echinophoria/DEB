## DEB model for Mussel
library(devtools)
install_github('https://github.com/Echinophoria/DEB')  # to make sure we load the last version
library(DEB)

## load DEB parameters and calculate auxiliary parameters. To incorporate
## larvae, besides the parameters we need the thresholds of maturity to assess
## when the animals switch from one stage to the next. This model has 3 maturity
## thresholds: Hb (birth, start of exogenous feeding), Hj (metamorphosis, when
## the larvae settle), Hp (puberty, when the animal starts accumulating energy
## for reproduction)
p <- get_DEB_pars(system.file('data_examples_templates/parameters_Mytilus_edulis.csv', package='DEB'))

## Initial stage,  here we say from where to start. We can start already with adults (H>Hp) or at any other stage. In this case we start from fertilised egg t0.
e_mother <- 1
# this is the scaled energy reserves of the mother, DEB assumes that the egg energy depends
# on the mother feeding and condition, that is not always the case, because there is always a
# tradeoff between number of eggs and their quality, and different species have
# different strategies for this. We go here with the generality.

UE0 <- get_uE0(p, e_mother)*p$p_Am   # Joules,  initial reserve of the egg. THERE IS A ROUTINE CALLED WITHIN get_uE0, beta0.
V0 <- (1e-4)^3 # cm^3, structural volum, we cannot start with 0 but we do use a very small value. This is structural volume, i.e. this is related to somatic growth
H0 <- 0 # Joules, maturity, it is a counter for development. Energy is invested into development. When the animal is fully developed it stops and this energy is used for reproduction
R0 <- 0 # Joules, this is the reproduction buffer. It is accumulated when H>Hp,


V <- V0
E <- UE0
H <- H0
R <- R0
JX <- 0 # C-mol, feeding rate ingested food




## environmental conditions (temperature and food)
<<<<<<< HEAD
Ti <- 285.15 # K,temperature
=======
Ti <- 291 # K,temperature
>>>>>>> 27d771c07c513fb03410bf037f659f219a4168d6
TC <- t_corr(p, Ti)  # correction for temperature, Ti is temperature in Kelvins


#### FUNCTIONAL RESPONSE
# Functional response, provides a scaling for the assimilation and a feeding
# rate as ingestion per unit of time and cm2. For filter feeders the
# synthetising unit concept is used. This easily separates clearance rate from
# ingestion rate and allows to add n number of substrates and food selectivity.
# for Norway and at this stage we start with one substrate --> particulate organic carbon (C-mol/liter)

<<<<<<< HEAD
Xi= 8.33e-05  # C-mol/l, available food

lrv <- get_food_lrv(p, Xi) # calculate feeding related rates: Clearance, Ingestion and Egestion.
adt <- get_food(p, Xi)
=======
## each substrate requires three parameters:
p$J_Xpoc_Fm <- 4.8e-04 # C-mol/d.cm2, maximum retention rate for substrate POC
p$rho_Xpoc  <- 0.99  # -, probability of binding of substrate POC.
p$J_Xpoc_Im <- 1.4e-04 # C-mol/d.cm2, maximum ingestion rate

Xi= 2.06e-05  # C-mol/l, available food

JX <- get_food(p, Xi) # calculate feeding related rates: Clearance, Ingestion and Egestion.

f_Xi <- unlist(JX[[2]])*p$kap_X*p$mu_X/p$p_Am   # equivalent to scaled functional response (assimilation rate / max. assimilation rate)
>>>>>>> 27d771c07c513fb03410bf037f659f219a4168d6

# if there are several food sources that will be assimilated, then we will need
# to calculate individual pA for each and add them together. If all ingested is
# assimilated with same efficiency then we can add all and calculate one pA.
# Right now only one food source, max two, but the second one it is not
# assimilated!

<<<<<<< HEAD
flrv = lrv[[1]]
fadt = adt[[1]]
age = 0
=======
f = f_Xi


>>>>>>> 27d771c07c513fb03410bf037f659f219a4168d6

## Embryo stage, H < Hb
while (H < p$E_Hb){  # there is no feeding in this stage, this part of the code may need to be applied in steps shorter than days... for the mussel is 0.2 days at 18C.
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
<<<<<<< HEAD
      warning('embryo cannot develop to birth')
=======
      warning('embryo cannot develop to metamorphosis')
>>>>>>> 27d771c07c513fb03410bf037f659f219a4168d6
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
<<<<<<< HEAD
  age=age+1/24
}

Lb <- V^(1/3)  # cm, structural length at birth (needed for following stage)
Lwb <- Lb / p$del_lrv # cm, physical shell length at birth (just for check up)
# if we count the steps we can get age too.


f = flrv
=======
}

Lb <- V^(1/3)  # cm, structural length at birth (needed for following stage)
Lwb <- Lb / p$del_M # cm, physical shell length at birth (just for check up)
# if we count the steps we can get age too.



>>>>>>> 27d771c07c513fb03410bf037f659f219a4168d6
## Feeding larva stage, Hb < H < Hj
while (H > p$E_Hb  & H < p$E_Hj){
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
<<<<<<< HEAD
  age=age+1/24
  }

Lj <- V^(1/3)
Lwj <- Lj / p$del_lrv
=======

  }


>>>>>>> 27d771c07c513fb03410bf037f659f219a4168d6
## After settlement, Hj < H

# PRIORITY IN DEB: in DEB pC*Kap is used for somatic costs and (1-kap)*pC is
# used for reproductive/maturity investment/costs. Priority is given to somatic
# maintenance, followed by maturity maintenance, reproduction and ultimately
# growth. This can change for specific organisms. This code follow these priority assumption

<<<<<<< HEAD
s_M <- Lj/Lb
## Juvenile stage Hj < H < Hp
f = fadt
=======

## Juvenile stage Hj < H < Hp
>>>>>>> 27d771c07c513fb03410bf037f659f219a4168d6
while (H > p$E_Hj & H < p$E_Hp){ # animal is still not mature, (1-kap)*pC is used to increase maturity (H) towards the next stage.

  L = V^(1/3)  # structural length
  e = p$v * E/p$p_Am/V  # scaled reserves

  pA = TC * p$p_Am * L^2 * s_M * f  # Joules, assimilation power, it depends on available food, abundance and type
  pC = TC * p$p_Am * L^2 * ((p$g + p$L_T/p$L_m) * s_M + L/p$L_m)/(1 + p$g/e) # Joules, mobilisation power, depends on animal condition (reserves, e)
  pS = TC * p$p_Am * p$kap * L^2 * (L + p$L_T/p$L_m)  # Joules, cost of somatic maintenance, depends on the size of the animal, V (structural volume)
  pJ = TC * p$p_Am * p$k_J * H  # Joules, cost of maintaining maturity, it is a small cost related to maintain complexity, but it comes from the reproductive investment

  if (p$kap * pC >= pS) {  # there is energy for somatic maintenance and maybe for growth, kappa rule fully applies
    dV = (pC * p$kap - pS)/p$E_G
    dH = ((1 - p$kap) * pC - pJ)
    }
  else {
    # kap*pC cannot cover somatic maintenance, kappa rule no longer applies and
    # priority S>J>R>G is used. There is no growth and there may be no
    # investment on maturity
    dH = (pC - pS - pJ)
    dV = 0
    if (dH < 0) {
      # when there isn't energy enough to cover maintenance, the animal either
      # dies or uses its tissue. In the case of mussels tissue is used, first
      # the reproductive tissue, when exausted the structure is used. At this
      # stage there is no reproductive tissue. So only energy from structure can
      # be used to keep the animal alive.
      dV = dH/p$E_G   # Start of using tissue. Animal shrink! shell cannot shrink so is only the somatic tissue that is shrinking. That is why we follow V and L and not physical length.
      dH = 0    # there is no energy to keep maturing.
      }
      }
  dE = pA - pC  # update the energy in the reserves (assimilated - mobilised)

  # updating state variables
  H <- H + dH/24
  V <- V + dV/24
  E <- E + dE/24
<<<<<<< HEAD
  age = age + 1/24
}

Lp <- V^(1/3)
Lwp <- Lp/p$del_M
=======
}

Lwp <- V^(1/3)/p$del_M
>>>>>>> 27d771c07c513fb03410bf037f659f219a4168d6

## Adult (reproducing adult) H > Hp. Energy is no longer alocated to H, but instead that energy is used for reproduction.
while (H > p$E_Hp){

  L = V^(1/3)  # structural length
  e = p$v * E/p$p_Am/V  # scaled reserves

  pA = TC * p$p_Am * L^2 * s_M * f  # Joules, assimilation power, it depends on available food, abundance and type
  pC = TC * p$p_Am * L^2 * ((p$g + p$L_T/p$L_m) * s_M + L/p$L_m)/(1 + p$g/e) # Joules, mobilisation power, depends on animal condition (reserves, e)
  pS = TC * p$p_Am * p$kap * L^2 * (L + p$L_T/p$L_m)  # Joules, cost of somatic maintenance, depends on the size of the animal, V (structural volume)
  pJ = TC * p$k_J * H # Joules, cost of maintaining maturity, it is a small cost related to maintain complexity, but it comes from the reproductive investment

  if (p$kap * pC >= pS) {  # there is energy for somatic maintenance and maybe for growth, kappa rule fully applies
    dV = (pC * p$kap - pS)/p$E_G
    dR = ((1 - p$kap) * pC - pJ)
  }  else { # kap*pC cannot cover somatic maintenance, kappa rule no longer applies and priority S>J>R>G is used. There is no growth and there may be no investment on maturity
    dR = (pC - pS - pJ)
    dV = 0
    if (dR < 0) {
      # when there isn't energy enough to cover maintenance, the animal either
      # dies or uses its tissue. In the case of mussels tissue is used, first
      # the reproductive tissue, when exausted the structure is used. At this
      # stage there is no reproductive tissue. So only energy from structure can
      # be used to keep the animal alive.
      dV = dR/p$E_G   # Start of using tissue. Animal shrink! shell cannot shrink so is only the somatic tissue that is shrinking. That is why we follow V and L and not physical length.
      dR = 0    # there is no energy to keep maturing.
    }
  }
  dE = pA - pC  # update the energy in the reserves (assimilated - mobilised)

  # updating state variables
  R <- R + dR/24
  V <- V + dV/24
  E <- E + dE/24

  # I have to stop it somewhere, can ignore this when using a limited number of steps.
  Li <-  1.13 # cm, maximum structural length at feeding level. This is a von Bertalanffy parameter
  if (V^(1/3)>Li){
    break()
  }
<<<<<<< HEAD
  age<-age+1/24
=======
>>>>>>> 27d771c07c513fb03410bf037f659f219a4168d6
}

