#A first attempt to calculate mesophyll conductance (gmes)

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

# DELTAi is photosynthetic discrimination against 13C excluding boundary layer, 
# internal transfer, respiration and photorespirataion
calcDELTAi <- function(a, b, Ci.Ca){
  DELTAi <- a+(b-a)*(Ci.Ca)
  return(DELTAi)
}
# a is 13C diffusion fractionation in permil
a <- 4.4
# b is 13C combined fractionation during carboxylation by Rubisco and PEP-K
b <- 30

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
  return(DELTAobs)
}

# Calculate gmes ignoring ternary effects, respiration and photorespiration fractionation
# equation 8 in Urbiena & Marshall 2011 PCE
gmesW <- function(Photo, b, ai, DELTAi, DELTAobs, refCO2){
  gmesW <- Photo*(b-ai)/((DELTAi-DELTAobs*1000)*refCO2)
  return(gmesW)
}
# ai (or am) is 13C fractionation during internal (mesophyll) transfer in permil (including tranfer into water)
ai <- 1.8

# first attempt to calculate gmes

# This script calculates the 'CO2 concentration entering the cuvette' (sum of ambient and injection)
# and the d13C of the CO2 entering taking into account the d13C of the injected CO2
source('scripts/calculateCin.R')
allPaired <- deltaPaired
allPaired$VPDmol <- allPaired$VPDair/101.3 #101.3 kPa is the standard atmospheric pressure
allPaired$Date <- as.Date(allPaired$datetimeFM)
# get leaf area for each chamber from the final harvest
source('scripts/leafArea.R')
allPaired <- merge(allPaired, treeLeaf, by=c('chamber','Date'), all.x=T, all.y=F)
allPaired$A_area <- allPaired$FluxCO2*1000/allPaired$leafArea
allPaired$E_area <- allPaired$FluxH2O*1000/allPaired$leafArea
allPaired$gsc_area <- allPaired$E_area*0.001/(1.6 * allPaired$VPDmol)
allPaired[which(allPaired$condAlert=='yes'), c('gsc_area','E_area','A_area',
                                               'Corrdel13C_Avg', 'Corrdel13C_Avg_ref', "del13C_theor_ref")] <- NA
allPaired$iWUE <- allPaired$A_area/(allPaired$gsc_area*1.6)
allPaired$iWUE2 <- allPaired$A_area/allPaired$E_area
allPaired$diffConc <- allPaired$Cin - allPaired$CO2sampleWTC
allPaired$diffDel <- allPaired$Corrdel13C_Avg - allPaired$del13C_theor_ref
# calculate gms
allPaired$Ci <- getCifromE(E=allPaired$E_area*0.001, VPD=allPaired$VPDmol,
                           ChamberCO2=allPaired$CO2sampleWTC, Photo=allPaired$A_area)
allPaired[which(allPaired$Ci < 0), 'Ci'] <- NA
allPaired$Ci.Ca <- allPaired$Ci/allPaired$CO2sampleWTC
allPaired[which(allPaired$Ci.Ca > 1), 'Ci.Ca'] <- NA
allPaired$DELTAi <- calcDELTAi(a=a, b=b, Ci.Ca=allPaired$Ci.Ca)
allPaired$xi <- getXi(chamberCO2=allPaired$CO2sampleWTC, refCO2=allPaired$Cin)
allPaired$xi <- ifelse(allPaired$xi <= 0 | allPaired$condAlert == 'yes', NA, allPaired$xi)
allPaired$DELTAobs <- calcDELTAobs(allPaired$xi, deltaSample=allPaired$Corrdel13C_Avg,
                                   deltaRef=allPaired$del13C_theor_ref)
allPaired$gmes_area <- gmesW(Photo = allPaired$A_area, b, ai, allPaired$DELTAi,
                        allPaired$DELTAobs, refCO2 = deltaPaired$CO2sampleWTC)
allPaired[which(allPaired$A_area <= 0), c('gmes_area','Ci', 'DELTAi', 'DELTAobs', 'xi')] <- NA
allPaired[which(allPaired$E_area <= 0), c('gmes_area','Ci', 'DELTAi', 'DELTAobs', 'gsc_area')] <- NA
allPaired$gmes_area <- ifelse(allPaired$gmes_area < 0  | allPaired$diffConc < 10 |
                                allPaired$diffDel < 0 , NA, allPaired$gmes_area)
allPaired$month <- as.factor(lubridate::month(allPaired$datetimeFM, label=T))

gmesL <- list()
chambs <- c(paste0('C0', 1:9), paste0('C', 10:12))
for (i in 1:length(levels(as.factor(allPaired$chamber)))){
  gmesL[[i]] <- subset(allPaired, chamber==chambs[i])
}
lapply(gmesL, function(x) write.csv(x, file=paste0('calculated_data/indv_chambs/', x[1,'chamber'], '.csv'), row.names = F))
