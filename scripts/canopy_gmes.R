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

#Photosynthetic discrimination against 13C excluding boundary layer, internal transfer,
#respiration and photorespirataion (DELTAi)
# a is 13C diffusion fractionation in permil
a <- 4.4
# ai is 13C fractionation during internal (mesophyll) transfer in permil (including tranfer into water)
ai <- 1.8
# b is 13C combined fractionation during carboxylation by Rubisco and PEP-K
b <- 30
calcDELTAi <- function(a, b, Ci, Ca){
  DELTAi <- a+(b-a)*(Ci/Ca)
  return(DELTAi)
}

# Greek leter xi, ratio of the CO2 entering the well mixed leaf cuvette to the CO2 draw down
#in the gas stream by the leaf
# you can also calculate xi from photo and flow (Warren et al. 2003)
getXi <- function(chamberCO2, refCO2){
  xi <- refCO2/(refCO2-chamberCO2)
  return(xi)
}

# Observed photosynthetic discrimination (DELTAobs)
calcDELTAobs <- function(xi, deltaSample, deltaRef){
  DELTAobs <- xi*(deltaSample-deltaRef)/(1+deltaSample-xi*(deltaSample-deltaRef))
  return(DELTAobs)
}

#Use the approach of Warren et al. 2003 PCE: ignore ternary effects, respiration and photorespiration fractionation.
gmesW <- function(Photo, b, ai, DELTAi, DELTAobs, refCO2){
  gmesW <- Photo*(b-ai)/((DELTAi-DELTAobs)*refCO2)
  return(gmesW)
}

# first attempt to calculate gmes
WTCflux <- read.csv('calculated_data/chamflux_fm.csv')
WTCflux$datetimeFM <- ymd_hms(as.character(WTCflux$datetimeFM))
source('scripts/calculateCin.R')
allPaired <- merge(WTCflux, deltaPaired, by=c('datetimeFM','chamber'), all.x=F, all.y=T)
allPaired$VPDmol <- allPaired$VPDair/101.3 #101.3 kPa is the standard atmospheric pressure
# calculate gms
allPaired$Ci <- getCifromE(E=allPaired$FluxH2O, VPD=allPaired$VPDmol,
                           ChamberCO2=allPaired$totalCO2, Photo=allPaired$FluxCO2*1000)
allPaired$DELTAi <- calcDELTAi(a=a, b=b, Ci=allPaired$Ci, Ca=allPaired$Cin)
allPaired$xi <- getXi(chamberCO2 = deltaPaired$totalCO2, refCO2 = deltaPaired$Cin)
allPaired$DELTAobs <- calcDELTAobs(allPaired$xi, deltaSample=allPaired$Corrdel13C_Avg,
                                   deltaRef=allPaired$del13C_theor_ref)
allPaired$gmes <- gmesW(Photo = allPaired$FluxCO2*1000, b, ai, allPaired$DELTAi, allPaired$DELTAobs, refCO2 = deltaPaired$Cin)
allPaired$Date <- as.Date(allPaired$datetimeFM)
allPaired$chamber2 <- as.character(allPaired$chamber)
allPaired$chamber <- ifelse(nchar(allPaired$chamber2)==2, paste0('C',allPaired$chamber2), 'x')
allPaired$chamber <- ifelse(nchar(allPaired$chamber2)==1, paste0('C0',allPaired$chamber2), allPaired$chamber)
keysChamber <- read.csv('data/treatment_key.csv')
allPaired <- merge(allPaired, keysChamber, by=c('chamber','Date'), all.x=T, all.y=F)
