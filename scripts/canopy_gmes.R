#A first attempt to calculate mesophyll conductance (gmes)
#Use the approach of Warren et al. 2003 PCE: ignore ternary effect,s respiration and photorespiration fractionation.
# a is 13C diffusion fractionation in permil
a <- 4.4
# ai is 13C fractionation during internal (mesophyll) transfer in permil (including tranfer into water)
ai <- 1.8
# b is 13C combined fractionation during carboxylation by Rubisco and PEP-K
b <- 30

getCifromE <- function(E, VPD, ChamberCO2, Photo){
  gtw <- E/VPD
  Ci <- ((gtw-E0.5)*ChamberCO2-Photo)/(gtw+E*0.5)
  return(Ci)
}

DELTAi <- function(a, b, Ci, ChamberCO2){
  DELTAi <- a+(b-a)*(Ci/Ca)
  return(DELTAi)
}

getXi <- function(chamberCO2, refCO2){
  xi <- chamberCO2/(refCO2-chamberCO2)
  return(xi)
}

DELTAobs <- function(xi, deltaSample, deltaRef){
  DELTAobs <- xi(deltaSample-deltaRef)/(1+deltaSample-xi(deltaSample-deltaRef))
  retrun(DELTAobs)
}

gmesW <- function(Photo, b, ai, DELTAi, DELTAobs, Ca){
  gmesW <- Photo*(b-ai)/((DELTAi-DELTAobs)*Ca)
  return(gmesW)
}
testcham <- read.csv("gmes_calc/march_ch9_ch12.csv")