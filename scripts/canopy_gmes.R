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
calcDELTAi <- function(a, b, Ci, Ca){
  DELTAi <- a+(b-a)*(Ci/Ca)
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
# read flux data cleaned and averaged for 15-min intervals
WTCflux <- read.csv('calculated_data/chamflux_fm.csv')
WTCflux$datetimeFM <- lubridate::ymd_hms(as.character(WTCflux$datetimeFM))
WTCflux$chamber <- as.character(WTCflux$chamber)
# This script calculates the 'CO2 concentration entering the cuvette' (sum of ambient and injection)
# and the d13C of the CO2 entering taking into account the d13C of the injected CO2
source('scripts/calculateCin.R')
allPaired <- merge(WTCflux, deltaPaired, by=c('datetimeFM','chamber'), all.x=F, all.y=T)
allPaired$VPDmol <- allPaired$VPDair/101.3 #101.3 kPa is the standard atmospheric pressure
allPaired$Date <- as.Date(allPaired$datetimeFM)
allPaired$chamber2 <- as.character(allPaired$chamber)
allPaired$chamber <- ifelse(nchar(allPaired$chamber2)==2, paste0('C',allPaired$chamber2), 'x')
allPaired$chamber <- ifelse(nchar(allPaired$chamber2)==1, paste0('C0',allPaired$chamber2), allPaired$chamber)
keysChamber <- read.csv('data/treatment_key.csv')
allPaired <- merge(allPaired, keysChamber, by=c('chamber','Date'), all.x=T, all.y=F)
# get leaf area for each chamber from the final harvest
source('scripts/leafArea.R')
allPaired <- merge(allPaired, treeLeaf, by='chamber', all=T)
allPaired$A_area <- allPaired$FluxCO2*1000/allPaired$leafArea
allPaired$E_area <- allPaired$FluxH2O/allPaired$leafArea
allPaired$gsc_area <- allPaired$FluxH2O/(1.6 * allPaired$VPDmol * allPaired$leafArea)
# calculate gms
allPaired$Ci <- getCifromE(E=allPaired$E_area, VPD=allPaired$VPDmol,
                           ChamberCO2=allPaired$CO2sampleWTC, Photo=allPaired$A_area)
allPaired$DELTAi <- calcDELTAi(a=a, b=b, Ci=allPaired$Ci, Ca=allPaired$CO2sampleWTC)
allPaired$xi <- getXi(chamberCO2=allPaired$CO2sampleWTC, refCO2=allPaired$Cin)
allPaired$DELTAobs <- calcDELTAobs(allPaired$xi, deltaSample=allPaired$Corrdel13C_Avg,
                                   deltaRef=allPaired$del13C_theor_ref)
allPaired$gmes_area <- gmesW(Photo = allPaired$A_area, b, ai, allPaired$DELTAi,
                        allPaired$DELTAobs, refCO2 = deltaPaired$CO2sampleWTC)
allPaired[which(allPaired$condAlert=='yes'), c('gmes_area','Ci','gsc_area','E_area','A_area')] <- NA
allPaired[which(allPaired$A_area < 0), c('gmes_area','Ci')] <- NA
allPaired[which(allPaired$E_area < 0), c('gmes_area','Ci','gsc_area','E_area')] <- NA
allPaired[which(allPaired$Ci < 0), c('gmes_area','Ci')] <- NA
allPaired[which(allPaired$gmes < 0), 'gmes_area'] <- NA
