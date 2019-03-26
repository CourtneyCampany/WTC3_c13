source('scripts/canopy_gmes.R')
source('master_scripts/phloem_plotting.R')
source('scripts/basicFunTEG.R')
source('scripts/leafChem.R')
phl <- phloem3
phl$chamber2 <- as.character(phl$chamber)
phl$chamber <- ifelse((nchar(phl$chamber2) == 1), paste0('C0', phl$chamber2), paste0('C', phl$chamber2))
phl$month <- str_sub(phl$month, 1, 3)
names(phl)[2] <- 'd13Cph'
summary(aov(d13Cph~temp*month, data=phl))
summary(aov(d13Cleaf~temp*month*leaf, data=leafChem2))


calcDELTAobsMat <- function(del13Camb, del13Cmat){
  DELTAobsMat <- (del13Camb - del13Cmat)*1000/(1000+del13Cmat)
  return(DELTAobsMat)
}

del13CcampAvg <- doBy::summaryBy(Corrdel13C_Avg + CO2sampleWTC + Cc + diff_Ci.Cc~ month + chamber,
                                  FUN=mean.na, data=subset(allPaired, PAR > 3 & condAlert=='no'
                                                                      & A_area > 0 & E_area > 0))
phl <- merge(phl[,c('chamber','month','d13Cph','temp')], del13CcampAvg, by=c('month','chamber'), all=T)
phl2 <- merge(phl, leafChem2, by=c('month','chamber'), all=T)
phl <- merge(phl, leafChem, by=c('month','chamber'), all=T)

# there are significant differences in d13Cph, d13Cleaf (sun and shade) among campaigns
# no signfiicant differences in d13Cph or d13Cleaf between temperature treatments
modelAll <- lm(d13Cleaf~d13Cph*leaf, data=phl2)
# d13Cph and d13Cleaf are correlated and the interaction with leaf type is not significant
modelSun <- lm(d13CsunLeaf~d13Cph, data=phl)
modelShade <- lm(d13CshLeaf~d13Cph, data=phl)
modelAvg <- lm(d13CleafAvg~d13Cph, data=phl)
# the correlation is best for d13Cph and d13CleafAvg, but barely different from that of sun leaves

windows(16,8)
par(mfrow=c(1,2), mar=c(4,6,1,1))
plot(phl$d13CsunLeaf~phl$d13Cph, pch=19, col='orange', ylim=c(-34,-26), xlim=c(-34,-26), cex.lab=1.5,
     ylab=expression(delta^13*C[leaf]~('\211')), xlab=expression(delta^13*C[ph]~('\211')))
points(phl$d13CshLeaf~phl$d13Cph, pch=19, col='cornflowerblue')
abline(lm(d13Cleaf~d13Cph, data=phl2))
legend('topleft', pch=c(NA, NA, 19, 19), col=c(NA, NA, 'orange','cornflowerblue'),
       legend=c(paste0('P = ', round(summary(modelAll)$coefficients[14], 3)),
                paste0('R2 = ', round(summary(modelAll)$r.squared, 2)),'Sun','Shade'), bty='n')
par(mar=c(4,2,1,1))
plot(phl$d13CleafAvg~phl$d13Cph, pch=19, col='darkolivegreen', ylim=c(-34,-26), xlim=c(-34,-26), cex.lab=1.5,
     ylab=expression(delta^13*C[leaf]~('\211')), xlab=expression(delta^13*C[ph]~('\211')))
abline(lm(d13CleafAvg~d13Cph, data=phl))
legend('topleft', pch=c(NA, NA, 19, 19), col=c(NA, NA, 'darkolivegreen'),
       legend=c(paste0('P = ', round(summary(modelAvg)$coefficients[8], 4)),
                paste0('R2 = ', round(summary(modelAvg)$r.squared, 2)),'Avg'), bty='n')

phl$DELTAobsPhAvg <- calcDELTAobsMat(del13Camb = phl$Corrdel13C_Avg.mean.na, del13Cmat = phl$d13Cph)
phl$DELTAobsSunLeaf <- calcDELTAobsMat(del13Camb = phl$Corrdel13C_Avg.mean.na, del13Cmat = phl$d13CsunLeaf)
phl$DELTAobsShLeaf <- calcDELTAobsMat(del13Camb = phl$Corrdel13C_Avg.mean.na, del13Cmat = phl$d13CshLeaf)
phl$DELTAobsAvgLeaf <- calcDELTAobsMat(del13Camb = phl$Corrdel13C_Avg.mean.na, del13Cmat = phl$d13CleafAvg)
#phl$DELTAobsAvg2 <- calcDELTAobsMat(del13Camb = phl$del13C_theor_ref.mean.na, del13Cmat = phl$d13Cph)
#phl$DELTAobsMin <- calcDELTAobsMat(del13Camb = phl$Corrdel13C_Avg.min.na, del13Cmat = phl$d13Cph)
#phl$DELTAobsMin2 <- calcDELTAobsMat(del13Camb = phl$del13C_theor_ref.min.na, del13Cmat = phl$d13Cph)
phl$iWUEph_uncorr <- phl$CO2sampleWTC.mean.na*(1-((phl$Corrdel13C_Avg.mean.na-phl$d13Cph-a)/(b-a)))
phl$iWUEph_corr <- phl$iWUEph_uncorr - phl$diff_Ci.Cc.mean.na

allPaired <- merge(allPaired, phl, by=c('month','chamber'), all=T)
allPaired$DELTAobsPhCont <- calcDELTAobsMat(del13Camb=allPaired$Corrdel13C_Avg, del13Cmat=allPaired$d13Cph)
allPaired$DELTAobsSunLeafCont <- calcDELTAobsMat(del13Camb=allPaired$Corrdel13C_Avg, del13Cmat=allPaired$d13CsunLeaf)
allPaired$DELTAobsShLeafCont <- calcDELTAobsMat(del13Camb=allPaired$Corrdel13C_Avg, del13Cmat=allPaired$d13CshLeaf)
allPaired$DELTAobsAvgLeafCont <- calcDELTAobsMat(del13Camb=allPaired$Corrdel13C_Avg, del13Cmat=allPaired$d13CleafAvg)
#allPaired$DELTAobsPhCont2 <- calcDELTAobsMat(del13Camb=allPaired$del13C_theor_ref, del13Cmat=allPaired$d13Cph)
allPaired[which(allPaired$condAlert=='yes' | allPaired$A_area <= 0 | allPaired$PAR =< 3 | allPaired$E_area <= 0),
          c('DELTAobsPhCont','DELTAobsSunLeafCont','DELTAobsShLeafCont','DELTAobsAvgLeafCont')] <- NA
allPaired$iWUEph_corr2 <- allPaired$iWUEph_uncorr - allPaired$diff_Ci.Cc
allPaired$iWUEph_corr2 <- ifelse(allPaired$condAlert=='yes' | allPaired$A_area <= 0 | allPaired$PAR < 3 |
                                     allPaired$E_area <= 0, NA, allPaired$iWUEph_corr2)
#allPaired$DELTAobsPhCont2 <- ifelse(allPaired$condAlert=='yes' | allPaired$A_area <= 0| allPaired$E_area <= 0
 #                                   | allPaired$PAR < 3, NA, allPaired$DELTAobsPhCont2)
trtkey$chamber <- c(paste0('C0', 1:9), paste0('C', 10:12))
names(trtkey)[2] <- 'T_treatment'

iWUEsumm <- doBy::summaryBy(iWUE ~ month + chamber, FUN=mean, data=allPaired[which(!is.na(allPaired$gmes_area)),])
iWUEsumm <- merge(iWUEsumm, phl, by=c('month', 'chamber'), all=T)
iWUEsumm <- merge(iWUEsumm, trtkey, by='chamber', all=T)

