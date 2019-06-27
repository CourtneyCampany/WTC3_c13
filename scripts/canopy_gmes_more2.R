#calculate CO2 conc. in the substomtal cavity for the canopy (Ci)
getCifromE <- function(E, VPD, ChamberCO2, Photo){
  #calculate stomtal conductance to water (gsw) from transpiration for a well-coupled canopy:
  gs <- E/VPD
  # I assume boundary layer conductance is neglibile (well mixed canopy inside the WTC constantly blown!)
  # Thus, gsw is equivalent to total conductance to water: gtw=gsw (Eq. 1-19 in LI-6400 manual)
  # To convert from gsw to conductance to CO2 (gc), I divide by the diffusivity ratio of CO2/H2O in air
  gc <- gs/1.6
  # from Eq. 1-18 in the LI-COR 6400 manual
  Ci <- ((gc-E*0.5)*ChamberCO2-Photo)/(gc+E*0.5)
  return(Ci)
}

# a is 13C diffusion fractionation through the stomata in permil
a <- 4.4
# b is 13C combined fractionation during carboxylation by Rubisco and PEP-K
b <- 29
# f is fractionation factor in permil of photorespiration according to Evans & VonCaemmerer 2013
f <- 16.2
# ai is diffusion through water in permil
ai <-  1.8

# DELTAi is photosynthetic discrimination against 13C excluding boundary layer, 
# internal transfer, respiration and photorespirataion (NO TERNARY)
calcDELTAi <- function(a, b, Ci.Ca){
  DELTAi <- a+(b-a)*(Ci.Ca)
  return(DELTAi)
}

# Greek leter xi, ratio of the CO2 entering the well mixed leaf cuvette to the CO2 draw down
# in the gas stream by the leaf
# you can also calculate xi from photo and flow (Warren et al. 2003)
getXi <- function(chamberCO2, refCO2){
  xi <- refCO2/(refCO2-chamberCO2)
  return(xi)
}

# Observed photosynthetic discrimination (DELTAobs)
# Equation (2) in supplementary methods Campany et al. 2016 PCE
calcDELTAobs <- function(xi, deltaSample, deltaRef){
  DELTAobs <- xi*(deltaSample-deltaRef)/(1000+deltaSample-xi*(deltaSample-deltaRef))
  return(DELTAobs*1000)
}

# eResp is respiration fractionation assuming delta13Csubstrate = delta13CAnet
source('scripts/calculate_eResp.R')
eResp <- mean(dplyr::summarise(dplyr::group_by(eResp, month), eMean=mean.na(eRespPh))$eMean, na.rm = T)

# DELTAe is discrimination due to respiration (NO TERNARY)
calcDELTAe <- function(eResp, Rd, Photo, CO2sample, Ci, gammaStar){
  DELTAe <- (eResp*Rd/((Photo+Rd)*CO2sample))*(Ci-gammaStar)
  return(DELTAe)
}
# DELTAf is photoresiration fractionation (NO TERNARY)
calcDELTAf <- function(f, gammaStar, CO2cuv){
  DELTAf <- f*(gammaStar/CO2cuv)
  return(DELTAf)
}

# CO2 compensation point in the absence of Rlight at 25 C
# according to von Caemmerer et al. 1996 in ppm
gamma25 <- 36.9
# calculate Gamma Star for a given leaf temp temperature
# eq. 4 in Crous et al. 2012 PCE
calcGammaStar <- function(gamma25, temp){
  gammaStar <- gamma25 + 1.88*(temp-25) + 0.036*(temp)
  return(gammaStar)
}

# Calculate gmes taking into account ternary effects, respiration and photorespiration fractionation
# equation 11 in Sup. Info in Campany et al. 2016
gmesComplete <- function(b, ai, eResp, Rd, Photo, refCO2, DELTAi, DELTAobs, DELTAe, DELTAf){
  gmes <- (b-ai-(eResp*Rd/(Photo+Rd)))*(Photo/refCO2)/(DELTAi-DELTAobs-DELTAe-DELTAf)
  return(gmes)
}

allPaired$diffConc <- allPaired$Cin - allPaired$CO2sampleWTC
allPaired$diffDelPre <- allPaired$Corrdel13C_Avg - allPaired$del13C_theor_ref
allPaired$xi <- getXi(chamberCO2=allPaired$CO2sampleWTC, refCO2=allPaired$Cin)
allPaired$xi <- ifelse(allPaired$xi <= 0 | allPaired$condAlert == 'yes', NA, allPaired$xi)
allPaired$DELTAobsPre <- calcDELTAobs(allPaired$xi, deltaSample=allPaired$Corrdel13C_Avg,
                                   deltaRef=allPaired$del13C_theor_ref)
# source('scripts/plotDELTAobsVSdiffConc.R')
allPaired$DELTAobsPre <- ifelse(allPaired$diffConc < 35, NA, allPaired$DELTAobsPre)
DELTAobs <- dplyr::summarise(dplyr::group_by(setDT(allPaired), datetimeFM, chamber), diffDel=mean.na(diffDelPre),
                             DELTAobs = mean.na(DELTAobsPre), DELTAoSD = sd(DELTAobsPre, na.rm = T),
                             del13Cch=mean.na(Corrdel13C_Avg), del13Csd=sd(Corrdel13C_Avg),
                             del13Camb=mean.na(Corrdel13C_Avg_ref), del13CambSD=sd(Corrdel13C_Avg_ref))
DELTAobs$DELTAobs <- ifelse(DELTAobs$DELTAoSD >= 3, NA, DELTAobs$DELTAobs)
DELTAobs$del13Cmean <- ifelse(DELTAobs$del13Csd >= 5, NA, DELTAobs$del13Cch)
DELTAobs$del13Camb <- ifelse(DELTAobs$del13CambSD >= 5, NA, DELTAobs$del13Camb)
allPaired <- dplyr::left_join(DELTAobs[, c('datetimeFM','chamber','DELTAobs','diffDel','del13Cch', 'del13Camb')],
                              deltaPaired[,-c('totalCO2', 'totalCO2_ref',  'del13C_theor_ref',
                                                        'Corrdel13C_Avg', 'Corrdel13C_Avg_ref')],
                              by=c('datetimeFM', 'chamber'))

allPaired$month <- as.factor(lubridate::month(allPaired$datetimeFM, label=T))
allPaired$month <- factor(allPaired$month, levels=c('Oct','Dec','Jan','Feb','Mar','Apr'))
allPaired$midday <- ifelse(allPaired$Time >= 10.30 & allPaired$Time <= 13.30, 'yes', 'no')
allPaired$Date <- as.Date(allPaired$datetimeFM)
# get leaf area for each chamber and date
source('scripts/leafArea.R')
allPaired <- merge(allPaired, treeLeaf, by=c('chamber','Date'), all.x=T, all.y=F)
allPaired$A_area <- allPaired$FluxCO2*1000/allPaired$leafArea
allPaired$VPDmol <- allPaired$VPDair/allPaired$Patm
allPaired$E_area <- allPaired$FluxH2O*1000/allPaired$leafArea
allPaired$gsc_area <- allPaired$E_area*0.001/(1.6 * allPaired$VPDmol)
allPaired$iWUE <- allPaired$A_area/allPaired$gsc_area # in mumol/mol
allPaired$WUE <- allPaired$A_area/allPaired$E_area # in mumol/mmol
allPaired$gammaStar <- calcGammaStar(gamma25, temp=allPaired$Tair_al)
source('scripts/calcRd25.R')
allPaired <- merge(allPaired, Rdark, by=c('month','T_treatment'), all=T)
# calculate respiration in the light at a given temp from Rd at 25 C
# these parameters are those for BlEvTemp in Table S3 of Heskel et al. 2016 PNAS
allPaired$Rd_corrT <- allPaired$Rd25*exp(0.0518*(allPaired$Tair_al-25)+0.00047*(allPaired$Tair_al^2-25^2))
allPaired[which(allPaired$condAlert=='yes'), c('gsc_area','E_area','A_area','iWUE','WUE','gammaStar',
                                               'Rd_corrT')] <- NA
# calculate gms
allPaired$Ci <- getCifromE(E=allPaired$E_area*0.001, VPD=allPaired$VPDmol,
                           ChamberCO2=allPaired$CO2sampleWTC, Photo=allPaired$A_area)
allPaired[which(allPaired$E_area <= 0 | allPaired$A_area <= 0 | allPaired$Ci < 0),'Ci'] <- NA
allPaired$Ci.Ca <- allPaired$Ci/allPaired$CO2sampleWTC
allPaired[which(allPaired$Ci.Ca > 1), 'Ci.Ca'] <- NA
allPaired$diff_Ca.Ci <- allPaired$CO2sampleWTC - allPaired$Ci
allPaired[which(allPaired$diff_Ca.Ci <= 0), 'diff_Ca.Ci'] <- NA
allPaired$DELTAi <- calcDELTAi(a=a, b=b, Ci.Ca=allPaired$Ci.Ca)
allPaired$DELTAe <- calcDELTAe(eResp=eResp, Rd=allPaired$Rd_corrT,
                               Photo=allPaired$A_area, CO2sample=allPaired$CO2sampleWTC,
                               Ci=allPaired$Ci, gammaStar=allPaired$gammaStar)
allPaired$DELTAf <- calcDELTAf(f, gammaStar=allPaired$gammaStar,
                               CO2cuv=allPaired$CO2sampleWTC)
allPaired$gmes_area <- gmesComplete(b, ai, eResp=eResp, Rd=allPaired$Rd_corrT,
                                    Photo=allPaired$A_area, refCO2=allPaired$CO2sampleWTC, 
                                    DELTAi=allPaired$DELTAi, DELTAobs=allPaired$DELTAobs, 
                                    DELTAe=allPaired$DELTAe, DELTAf=allPaired$DELTAf)
allPaired[which(allPaired$A_area <= 0), c('gmes_area', 'DELTAi', 'DELTAobs', 'xi','iWUE','WUE')] <- NA
allPaired[which(allPaired$E_area <= 0), c('gmes_area','Ci', 'DELTAi', 'DELTAobs',
                                          'gsc_area', 'iWUE','WUE')] <- NA
allPaired$gmes_area <- ifelse(allPaired$gmes_area < 0  | allPaired$gmes_area > 1.1 |
                                allPaired$diffDel < 0 , NA, allPaired$gmes_area)
allPaired$Cc <- allPaired$Ci - (allPaired$A_area/allPaired$gmes_area)
allPaired[which(allPaired$Cc <= 0),'Cc'] <- NA
allPaired$diff_Ci.Cc <- allPaired$Ci - allPaired$Cc
allPaired[which(allPaired$diff_Ci.Cc <= 0), 'diff_Ci.Cc'] <- NA
allPaired$iWUEge_corr <- allPaired$iWUE + allPaired$diff_Ci.Cc
allPaired$fchamber <- as.factor(allPaired$chamber)

rm(Rdark, treeLeaf, deltaPaired)
