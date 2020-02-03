## DEB model for Mussel
library(devtools)
install_github('https://github.com/Echinophoria/DEB')  # to make sure we load the last version
library(DEB)
library(ncdf4)

## load DEB parameters and calculate auxiliary parameters. To incorporate
## larvae, besides the parameters we need the thresholds of maturity to assess
## when the animals switch from one stage to the next. This model has 3 maturity
## thresholds: Hb (birth, start of exogenous feeding), Hj (metamorphosis, when
## the larvae settle), Hp (puberty, when the animal starts accumulating energy
## for reproduction)
p <- get_DEB_pars('parameters_Mytilus_edulis.csv')


# load NORWECOM.e2e output/input
V = (3*p$del_M)^3
E = p$E_m*V  # this assumes f=1
R = 0


Ti <- 291 # K,temperature
TC <- t_corr(p, Ti)  # correction for temperature, Ti is temperature in Kelvins
f = 1
Lmax = 5*p$del_M
L = 3*p$del_M
VRE = c(V, R, E)

while (L < Lmax){
  VRE <- VRE_adult(p, VRE)

}


## Initial stage,  here we say from where to start. We can start already with adults (H>Hp) or at any other stage. In this case we start from fertilised egg t0.




## environmental conditions (temperature and food)
Ti <- 291 # K,temperature
TC <- t_corr(p, Ti)  # correction for temperature, Ti is temperature in Kelvins


#### FUNCTIONAL RESPONSE
# Functional response, provides a scaling for the assimilation and a feeding
# rate as ingestion per unit of time and cm2. For filter feeders the
# synthetising unit concept is used. This easily separates clearance rate from
# ingestion rate and allows to add n number of substrates and food selectivity.
# for Norway and at this stage we start with one substrate --> particulate organic carbon (C-mol/liter)

## each substrate requires three parameters:
p$J_Xpoc_Fm <- 4.8e-04 # C-mol/d.cm2, maximum retention rate for substrate POC
p$rho_Xpoc  <- 0.99  # -, probability of binding of substrate POC.
p$J_Xpoc_Im <- 1.4e-04 # C-mol/d.cm2, maximum ingestion rate

Xi= 2.06e-05  # C-mol/l, available food

JX <- get_food(p, Xi) # calculate feeding related rates: Clearance, Ingestion and Egestion.

f_Xi <- unlist(JX[[2]])*p$kap_X*p$mu_X/p$p_Am   # equivalent to scaled functional response (assimilation rate / max. assimilation rate)

# if there are several food sources that will be assimilated, then we will need
# to calculate individual pA for each and add them together. If all ingested is
# assimilated with same efficiency then we can add all and calculate one pA.
# Right now only one food source, max two, but the second one it is not
# assimilated!

f = f_Xi





Lb <- V^(1/3)  # cm, structural length at birth (needed for following stage)
Lwb <- Lb / p$del_M # cm, physical shell length at birth (just for check up)
# if we count the steps we can get age too.




## After settlement, Hj < H

# PRIORITY IN DEB: in DEB pC*Kap is used for somatic costs and (1-kap)*pC is
# used for reproductive/maturity investment/costs. Priority is given to somatic
# maintenance, followed by maturity maintenance, reproduction and ultimately
# growth. This can change for specific organisms. This code follow these priority assumption


## Juvenile stage Hj < H < Hp
while (H > p$E_Hj & H < p$E_Hp){ # animal is still not mature, (1-kap)*pC is used to increase maturity (H) towards the next stage.


}

Lwp <- V^(1/3)/p$del_M

## Adult (reproducing adult) H > Hp. Energy is no longer alocated to H, but instead that energy is used for reproduction.
while (H > p$E_Hp){



  # I have to stop it somewhere, can ignore this when using a limited number of steps.
  Li <-  1.13 # cm, maximum structural length at feeding level. This is a von Bertalanffy parameter
  if (V^(1/3)>Li){
    break()
  }
}

