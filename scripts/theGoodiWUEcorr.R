source('scripts/canopy_gmes_more.R')
source('master_scripts/phloem_plotting.R')
source('scripts/basicFunTEG.R')
source('scripts/leafChem.R')
phl <- phloem3
phl$chamber2 <- as.character(phl$chamber)
phl$chamber <- ifelse((nchar(phl$chamber2) == 1), paste0('C0', phl$chamber2), paste0('C', phl$chamber2))
phl$month <- str_sub(phl$month, 1, 3)
names(phl)[2] <- 'd13Cph'

del13CcampAvg <- doBy::summaryBy(iWUEge_corr + iWUE + Corrdel13C_Avg + CO2sampleWTC + diff_Ci.Cc +
                                 diff_Ca.Cc + Cc + Ci ~ month + chamber, FUN=mean.na,
                                 data=subset(allPaired, A_area > 0 & E_area > 0 &  PAR > 0 & iWUE < 500))
del13CcampAvgMD <- doBy::summaryBy(iWUEge_corr + iWUE + Corrdel13C_Avg + CO2sampleWTC + diff_Ci.Cc +
                                 diff_Ca.Cc + Cc + Ci ~ month + chamber, FUN=mean.na,
                                 data=subset(allPaired, A_area > 0 & E_area > 0 &  PAR > 0 & iWUE < 500 & midday=='yes'))
names(del13CcampAvgMD)[3:ncol(del13CcampAvgMD)] <- paste0(names(del13CcampAvgMD)[3:ncol(del13CcampAvgMD)], 'MD')
phl <- merge(merge(del13CcampAvgMD, del13CcampAvg, by=c('month','chamber'), all=T),
                   phl, by=c('month','chamber'), all=T)
phl <- merge(phl, leafChem, by=c('month','chamber'), all=T)

chambs <- data.frame(row.names=1:12)
chambs$chamber <- c(paste0('C0', 1:9), paste0('C', 10:12))
chambs$T_treatment <- rep(c('ambient','warmed'), 6)

phl <- merge(phl, chambs, by='chamber', all=T)

phl$meanDiff_Ca.Ci <- phl$CO2sampleWTC.mean.na - phl$Ci.mean.na
phl$meanDiff_Ca.CiMD <- phl$CO2sampleWTC.mean.naMD - phl$Ci.mean.naMD
phl$meanDiff_Ca.Cc <- phl$CO2sampleWTC.mean.na - phl$Cc.mean.na
phl$meanDiff_Ca.CcMD <- phl$CO2sampleWTC.mean.naMD - phl$Cc.mean.naMD
phl$meanDiff_Ci.Cc <- phl$Ci.mean.na - phl$Cc.mean.na
phl$meanDiff_Ci.CcMD <- phl$Ci.mean.naMD - phl$Cc.mean.naMD
phl$iWUEph_uncorr <- phl$CO2sampleWTC.mean.na*(1-((phl$Corrdel13C_Avg.mean.na-phl$d13Cph-a)/(b-a)))
phl$iWUEph_corr <- phl$iWUEph_uncorr - phl$diff_Ci.Cc.mean.na
phl$iWUEph_uncorrMD <- phl$CO2sampleWTC.mean.naMD*(1-((phl$Corrdel13C_Avg.mean.naMD-phl$d13Cph-a)/(b-a)))
phl$iWUEph_corrMD <- phl$iWUEph_uncorrMD - phl$diff_Ci.Cc.mean.naMD
phl$iWUEsunLeaf_uncorrMD <- phl$CO2sampleWTC.mean.naMD*(1-((phl$Corrdel13C_Avg.mean.naMD-phl$d13CsunLeaf-a)/(b-a)))
phl$iWUEsunLeaf_corrMD <- phl$iWUEsunLeaf_uncorrMD - phl$diff_Ci.Cc.mean.naMD
phl$iWUEshLeaf_uncorrMD <- phl$CO2sampleWTC.mean.naMD*(1-((phl$Corrdel13C_Avg.mean.naMD-phl$d13CshLeaf-a)/(b-a)))
phl$iWUEshLeaf_corrMD <- phl$iWUEshLeaf_uncorrMD - phl$diff_Ci.Cc.mean.naMD
phl$iWUEleafAvg_uncorrMD <- phl$CO2sampleWTC.mean.naMD*(1-((phl$Corrdel13C_Avg.mean.naMD-phl$d13CleafAvg-a)/(b-a)))
phl$iWUEleafAvg_corrMD <- phl$iWUEleafAvg_uncorrMD - phl$diff_Ci.Cc.mean.naMD

summary(lm(iWUE.mean.naMD ~ iWUEph_uncorrMD, data=phl))
summary(lm(iWUE.mean.naMD ~ iWUEph_corrMD, data=phl))
summary(lm(iWUE.mean.naMD ~ iWUEsunLeaf_corrMD, data=phl))
summary(lm(iWUE.mean.naMD ~ iWUEshLeaf_corrMD, data=phl))
summary(lm(iWUE.mean.naMD ~ iWUEleafAvg_corrMD, data=phl))
summary(lm(iWUEge_corr.mean.naMD ~ iWUEph_uncorrMD, data=phl))
summary(lm(iWUEge_corr.mean.naMD ~ iWUEleafAvg_uncorrMD, data=phl))

iWUEsumm <- doBy::summaryBy(iWUE.mean.naMD + iWUE.mean.na + iWUEge_corr.mean.naMD + iWUEph_corrMD +
                              iWUEph_uncorrMD + iWUEleafAvg_corrMD ~ month + T_treatment, data=phl,
                            FUN=c(mean.na, s.err.na))
summary(lm(iWUE.mean.naMD.mean.na ~ iWUEph_corrMD.mean.na, data=iWUEsumm))
summary(lm(iWUE.mean.naMD.mean.na ~ iWUEph_uncorrMD.mean.na, data=iWUEsumm))
summary(lm(iWUE.mean.naMD.mean.na ~ iWUEleafAvg_corrMD.mean.na, data=iWUEsumm))
summary(lm(iWUEge_corr.mean.naMD.mean.na ~ iWUEph_uncorrMD.mean.na, data=iWUEsumm))
write.csv(iWUEsumm, file='output/iWUEsumm.csv', row.names = F)
