source('scripts/canopy_gmes_more3.R')
source('master_scripts/phloem_plotting.R')
phl$fchamber <- as.factor(phl$chamber)
phl$month <- factor(phl$month, levels=c('Oct','Dec','Jan','Feb','Mar','Apr'))
# calculate mid-day mean for each chamber and campaign for:
# gmes, Ci, d13C-chamber and CO2 chamber concentration
# select only midday values with high light, positive A and E and reasonable iWUE
del13CcampAvgMD <- dplyr::summarise(dplyr::group_by(setDT(subset(allPaired, A_area > 0 & E_area > 0 &
                                                                   PAR >= 800 & iWUE < 400 & midday=='yes')),
                                                    month, chamber), 
                                    CiMD=mean.na(Ci), d13chMD=mean.na(del13Cch), E_MD = mean.na(E_area)*0.001,
                                    CO2chMD=mean.na(CO2sampleWTC), A_MD=mean.na(A_area), gsMD =mean.na(gs_area),
                                    Rd_MD=mean.na(Rd_corrT), gammaStar_MD=mean.na(gammaStar), gmesMD = mean.na(gmes_area)
                                    #, CiMD_se = s.err.na(Ci), d13chMD_se = s.err.na(del13Cch),
                                    #CO2chMD_se = s.err.na(CO2sampleWTC), A_MD_se = s.err.na(A_area),
                                    #Cc_MD_se = s.err.na(Cc)
                                    )
phl <- merge(del13CcampAvgMD, phl, by=c('month','chamber'), all = F)
chambs <- read.csv('data/trtkey2.csv')
phl <- merge(phl, chambs, by=c('chamber', 'month'), all=T)
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
