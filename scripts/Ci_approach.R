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
                                    CiMD=mean.na(Ci), d13chMD=mean.na(del13Cch),
                                    CO2chMD=mean.na(CO2sampleWTC), A_MD=mean.na(A_area), gsMD =mean.na(gs_area),
                                    Rd_MD=mean.na(Rd_corrT), gammaStar_MD=mean.na(gammaStar), gmesMD = mean.na(gmes_area)
                                    #, CiMD_se = s.err.na(Ci), d13chMD_se = s.err.na(del13Cch),
                                    #CO2chMD_se = s.err.na(CO2sampleWTC), A_MD_se = s.err.na(A_area),
                                    #Cc_MD_se = s.err.na(Cc)
                                    )
del13CcampAvgMD$gComb <- del13CcampAvgMD$gsMD/(1.6*del13CcampAvgMD$gmesMD)
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
# incorporate gmes limitation
phl$Ci2 <- phl$CO2chMD*(phl$DELTAobs-a+(b-ai)*phl$gComb)/
  (b-a+phl$gComb*(b-ai))
# discard netaive Ci
phl[which(phl$Ci2 < 0), 'Ci2'] <- NA
# incorporate photorespiration
phl$Ci3 <- phl$CO2chMD*(phl$DELTAobs-a)


