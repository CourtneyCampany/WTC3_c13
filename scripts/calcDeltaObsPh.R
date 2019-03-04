source('scripts/canopy_gmes.R')
source('master_scripts/phloem_plotting.R')
source('scripts/basicFunTEG.R')
phl <- phloem3
phl$chamber2 <- as.character(phl$chamber)
phl$chamber <- ifelse((nchar(phl$chamber2) == 1), paste0('C0', phl$chamber2), paste0('C', phl$chamber2))
phl$month <- str_sub(phl$month, 1, 3)
names(phl)[2] <- 'd13Cph'

calcDELTAobsMat <- function(del13Camb, del13Cmat){
  DELTAobsMat <- (del13Camb - del13Cmat)*1000/(1000+del13Cmat)
  return(DELTAobsMat)
}

del13CcampAvg <- doBy::summaryBy(del13C_theor_ref + Corrdel13C_Avg + CO2sampleWTC ~ month + chamber,
                                  FUN=c(mean.na, min.na), data=subset(allPaired, PAR > 3 & condAlert=='no'
                                                                      & A_area > 0 & E_area > 0))
phl <- merge(phl, del13CcampAvg, by=c('month','chamber'), all.x=T, all.y=F)
phl$DELTAobsAvg <- calcDELTAobsMat(del13Camb = phl$Corrdel13C_Avg.mean.na, del13Cmat = phl$d13Cph)
phl$DELTAobsAvg2 <- calcDELTAobsMat(del13Camb = phl$del13C_theor_ref.mean.na, del13Cmat = phl$d13Cph)
phl$DELTAobsMin <- calcDELTAobsMat(del13Camb = phl$Corrdel13C_Avg.min.na, del13Cmat = phl$d13Cph)
phl$DELTAobsMin2 <- calcDELTAobsMat(del13Camb = phl$del13C_theor_ref.min.na, del13Cmat = phl$d13Cph)
phl$iWUEph <- phl$CO2sampleWTC.mean.na*(1-((phl$Corrdel13C_Avg.mean.na-phl$d13Cph-a)/(b-a)))

allPaired <- merge(allPaired, phl[,c('d13Cph','month','chamber','DELTAobsAvg','DELTAobsAvg2','iWUEph',
                                     'DELTAobsMin','DELTAobsMin2')], by=c('month','chamber'), all=T)
allPaired$DELTAobsPhCont <- calcDELTAobsMat(del13Camb=allPaired$Corrdel13C_Avg, del13Cmat=allPaired$d13Cph)
allPaired$DELTAobsPhCont2 <- calcDELTAobsMat(del13Camb=allPaired$del13C_theor_ref, del13Cmat=allPaired$d13Cph)
allPaired$DELTAobsPhCont <- ifelse(allPaired$condAlert=='yes' | allPaired$A_area <= 0 | allPaired$PAR < 3|
                                     allPaired$E_area <= 0, NA, allPaired$DELTAobsPhCont)
allPaired$DELTAobsPhCont2 <- ifelse(allPaired$condAlert=='yes' | allPaired$A_area <= 0| allPaired$E_area <= 0
                                    | allPaired$PAR < 3, NA, allPaired$DELTAobsPhCont2)
