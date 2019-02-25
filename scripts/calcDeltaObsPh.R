source('scripts/canopy_gmes.R')
source('master_scripts/phloem_plotting.R')
source('scripts/basicFunTEG.R')
phl <- phloem3
phl$chamber2 <- as.character(phl$chamber)
phl$chamber <- ifelse((nchar(phl$chamber2) == 1), paste0('C0', phl$chamber2), paste0('C', phl$chamber2))
phl$month <- str_sub(phl$month, 1, 3)
names(phl)[2] <- 'd13Cph'
allPaired$month2 <- allPaired$month
allPaired$month <- as.character(allPaired$month)
allPaired <- merge(allPaired, phl[,c('d13Cph','month','chamber')], by=c('month','chamber'), all=T)
calcDELTAobsMat <- function(del13Camb, del13Cmat){
  DELTAobsMat <- (del13Camb - del13Cmat)*1000/(1000+del13Cmat)
  return(DELTAobsMat)
}
allPaired$DELTAobsPhCont <- calcDELTAobsMat(del13Camb=allPaired$Corrdel13C_Avg, del13Cmat=allPaired$d13Cph)
allPaired$DELTAobsPhCont2 <- calcDELTAobsMat(del13Camb=allPaired$del13C_theor_ref, del13Cmat=allPaired$d13Cph)
del13CcampAvg <- doBy::summaryBy(Cin + totalCO2 + del13C_theor_ref + Corrdel13C_Avg ~ month + chamber,
                                  FUN=c(mean.na, max.na, min.na), data=subset(allPaired, PAR > 1))
phl <- merge(phl, del13CcampAvg, by=c('month','chamber'), all.x=T, all.y=F)
phl$DELTAobsAvg <- calcDELTAobsMat(del13Camb = phl$Corrdel13C_Avg.mean.na, del13Cmat = phl$d13Cph)
phl$DELTAobsAvg2 <- calcDELTAobsMat(del13Camb = phl$del13C_theor_ref.mean.na, del13Cmat = phl$d13Cph)
phl$DELTAobsMin <- calcDELTAobsMat(del13Camb = phl$Corrdel13C_Avg.min.na, del13Cmat = phl$d13Cph)
phl$DELTAobsMin2 <- calcDELTAobsMat(del13Camb = phl$del13C_theor_ref.min.na, del13Cmat = phl$d13Cph)
