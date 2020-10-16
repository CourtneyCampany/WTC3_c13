# read the data
phl <- read.csv('phl_Ci.csv')
# a is 13C diffusion fractionation through the stomata in permil
a <- 4.4
# b is 13C combined fractionation during carboxylation by Rubisco and PEP-K
b <- 29
# f is fractionation factor in permil of photorespiration according to Evans & VonCaemmerer 2013
f <- 16.2
# ai is diffusion through water in permil
ai <-  1.8
# eResp is respiration fractionation in permil
eResp <- 3.38

# calculate Delta-obs
phl$DELTAobs <- (phl$d13chMD - phl$d13Cph) * 1000/(1000 + phl$d13Cph)
# following the equations and terminology in Ubierna & Farquhar 2014 PCE
# (except as here is a (4.4) and am is ai (1.8))
# for ALL calculations it is assumed that boundary layer resistance is negligible and alpha = 1
# calculate Ci from Delta assuming: mesophyll cond. is infinite and that
# photorespiration, respiaration and ternary effects are negligible
phl$Ci1 <- phl$CO2chMD * (phl$DELTAobs - a)/(b-a)
phl$Ci1b <- phl$CO2chMD * (phl$DELTAobs - a)/(27-a)
# incorporate gmes limitation
phl$A.gm <- phl$A_MD/phl$gmesMD
phl$Ci2 <- (phl$CO2chMD*(phl$DELTAobs-a)+(b-ai)*phl$A.gm)/(b-a)
# incorporate photorespiration
phl$Ci3 <- (phl$CO2chMD*(phl$DELTAobs-a)+(b-ai)*phl$A.gm+f*phl$gammaStar_MD)/(b-a)
# incorporate respiration
phl$Rd.AplusRd <- phl$Rd_MD/(phl$Rd_MD + phl$A_MD)
phl$Ci4 <- (phl$CO2chMD*(phl$DELTAobs-a)+(b-ai)*phl$A.gm+f*phl$gammaStar_MD-
              eResp*phl$Rd.AplusRd*(phl$A.gm+phl$gammaStar_MD))/
  (b-a-eResp*phl$Rd.AplusRd)
# incorporate ternarny correction (the quadratic solution), Eq. 10-12 Stagnl et al. 2019
phl$A.E <- phl$A_MD/phl$E_MD
phl$I <- a*(eResp*0.001*phl$Rd.AplusRd-b*0.001-1)
phl$IIa <- phl$DELTAobs*phl$CO2chMD*(-2-a*0.001)
phl$IIb <- -a*(phl$A.gm*(ai*0.001-b*0.001+eResp*0.001*phl$Rd.AplusRd)+(eResp*0.001*phl$Rd.AplusRd-f*0.001)*phl$gammaStar_MD+2*phl$A.E)
phl$IIc <- (2*phl$CO2chMD+2*phl$A.E+a*0.001*phl$CO2chMD)*(b-eResp*phl$Rd.AplusRd)
phl$II <- phl$IIa + phl$IIb + phl$IIc
phl$IIIa <- a*phl$CO2chMD*(phl$CO2chMD+2*phl$A.E)
phl$IIIb <- -phl$DELTAobs*phl$CO2chMD*(2*phl$A.E-a*0.001*phl$CO2chMD)
phl$IIIc <- (phl$CO2chMD*(2+a*0.001)+2*phl$A.E)*(phl$A.gm*(ai-b+eResp*phl$Rd.AplusRd)+(eResp*phl$Rd.AplusRd-f)*phl$gammaStar_MD)
phl$III <- phl$IIIa + phl$IIIb + phl$IIIc
phl$Ci5 <- (-phl$II + sqrt(phl$II^2 - 4 * phl$I * phl$III))/(2*phl$I)

# correlations between Ci estimates and Ci from gas exchange
summary(lm(CiMD ~ Ci1, data = phl))
#  no significant correlation with Ci estimated from the simple discrimination model with b = 29
summary(lm(CiMD ~ Ci1b, data = phl))
#  no significant correlation with Ci estimated from the simple discrimination model with b = 27
summary(lm(CiMD ~ Ci2, data = phl))
# significant correlation between Ci from gas-exchange and Ci estimated from d13C-phloem incorporating the effect of gm
summary(lm(CiMD ~ Ci3, data = phl))
# significant correlation between Ci from gas-exchange and Ci estimated from d13C-phloem incorporating the effect of 
#gm and photorespration but worse R2
summary(lm(CiMD ~ Ci4, data = phl))
# significant correlation between Ci from gas-exchange and Ci estimated from d13C-phloem incorporating the effect of 
#gm, photorespration and respiration but worse R2
summary(lm(CiMD ~ Ci5, data = phl))
# significant correlation between Ci from gas-exchange and Ci estimated from d13C-phloem incorporating the effect of 
# gm, photorespration, respiration and ternary correction but worse R2

# calculated iWUE from d13C of the phloem incorporating the effect of gm on 13C-discrimination

# calcule iWUE with the simplest discrimination model with b = 27
phl$iWUEph_uncorrMD <- (phl$CO2chMD/1.6)*((27-phl$DELTAobs)/(27-a))
# linear relationship between iWUE from gas-exchange and iWUE from Delta-phloem
summary(lm(iWUEgeMD ~ iWUEph_uncorrMD, data=phl))
# no significant correlation between iWUE estimates when gm limitation is not incorporated

# calculate iWUE from Delta incorporating gm and b = 29. Eq. 9 in the main text
phl$iWUEph_corrMD <- (phl$CO2chMD*(b-phl$DELTAobs)+(ai-b)*(phl$A_MD/phl$gmesMD))/(1.6*(b-a))
# linear relationship between iWUE from gas-exchange and iWUE from Delta-phloem incorporating the effect of gm
summary(lm(iWUEgeMD ~ iWUEph_corrMD, data=phl))
# significant correlation between iWUE estimates when gm limitation is not incorporated

# incorporate the presumed effect of post-photosynthetic fractionation. 2.5 permil
# this is the diference betwen d13C of the phloem and d13C of photosynthesis (from d13C online measurements)
postPhoto <- 2.5
phl$d13Cph_corr <- phl$d13Cph + postPhoto
phl$DELTAph_corr <- (phl$d13chMD - phl$d13Cph_corr) * 1000/(1000 + phl$d13Cph_corr)
# calculate iWUE from Delta incorporating the effect of post-photosynthetic fractionation, gm and b = 29. 
phl$iWUEph_corrMDmyAv <- (phl$CO2chMD*(b-phl$DELTAph_corr)+(ai-b)*(phl$A_MD/phl$gmesMD))/(1.6*(b-a))
# linear relationship between iWUE from gas-exchange and iWUE from Delta-phloem incorporating the effect of gm
summary(lm(iWUEgeMD ~ iWUEph_corrMDmyAv, data=phl))
# the slope is the same, but the intercept now is not signficantly different from cero.
