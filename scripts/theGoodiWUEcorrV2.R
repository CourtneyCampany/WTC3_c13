source('scripts/canopy_gmes_more2.R')
source('master_scripts/phloem_plotting.R')
source('scripts/basicFunTEG.R')
source('scripts/leafChem.R')
source('scripts/leafGmes.R')
leafChem <- merge(leafChem, leafGmes, by=c('month', 'chamber'), all = T)
phl$fchamber <- as.factor(phl$chamber)
phl$month <- factor(phl$month, levels=c('Oct','Dec','Jan','Feb','Mar','Apr'))
# assuming a post-photosynthetic fractionation processes of Eucs render 2.5 permil
# more enriched over night (Gessler et al. 2007 Funct. Plant Biol)
# phl$d13Cph_corrGess <- phl$d13Cph + 2.5
# same thing but calculate my own post-photosynthetic fractionation
photoSumm <- dplyr::summarise(dplyr::group_by(setDT(subset(allPaired, midday=='yes' & A_area > 0 & PAR >= 800
                                                           & deltaSubstrate >= -60 & deltaSubstrate <= 0)),
                                              chamber, month), d13CAnet=mean(deltaSubstrate, na.rm=T))
phl <- dplyr::left_join(phl, photoSumm, by=c('chamber','month'))
# in my case d13Cphloem is more DEPLETED than d13CAnet
phl$phlOffset <- phl$d13CAnet - phl$d13Cph
# there are differences in d13Cph and d13CAnet among campaings, but not between temperature treatments
model <- nlme::lme(phlOffset ~ month + temp, random = ~1 | fchamber,
                   data = phl, na.action = na.omit)
anova(model)
# differences in d13Cph and d13CAnet are not coordinated so differences in phl offset do not make sense
# use the mean of all values
mean(phl$phlOffset, na.rm=T)
s.err.na(phl$phlOffset)
# windows(16,8)
# par(mfrow=c(1,3), mar=c(3,6,1,0.5))
# boxplot(phl$d13CAnet~phl$month, ylab=expression(delta^13*C[Anet]~('\211')), xlab=' ', cex.lab=1.6, ylim=c(-33,-24))
# boxplot(phl$d13Cph~phl$month, ylab=expression(delta^13*C[phloem]~('\211')), xlab=' ', cex.lab=1.6, ylim=c(-33,-24))
# boxplot(phl$phlOffset~phl$month, ylab=expression(Phloem~Offset~('\211')), xlab=' ', cex.lab=1.6)
# abline(mean(phl$phlOffset, na.rm=T), 0, col='red', lwd=2)
# use the mean of all values
phl$d13Cph_corr <- phl$d13Cph + mean.na(phl$phlOffset)

del13CcampAvgMD <- dplyr::summarise(dplyr::group_by(setDT(subset(allPaired, A_area > 0 & E_area > 0 &
                                                                   PAR >= 800 & iWUE < 500 & midday=='yes')),
                                                    month, chamber), iWUEgeCorr=mean.na(iWUEge_corr),
                                    iWUEge=mean.na(iWUE), d13ch=mean.na(Corrdel13C_Avg),
                                    CO2ch=mean.na(CO2sampleWTC), Ci.Cc=mean.na(diff_Ci.Cc))

# del13CcampAvgMDSAFE <- doBy::summaryBy(iWUEge_corr + iWUE + Corrdel13C_Avg + CO2sampleWTC +diff_Ci.Cc ~ month + chamber,
#                                    FUN=mean.na, data=subset(allPaired, A_area > 0 & E_area > 0 &  PAR > 0 & iWUE < 500
#                                                             & midday=='yes'))
names(del13CcampAvgMD)[3:ncol(del13CcampAvgMD)] <- paste0(names(del13CcampAvgMD)[3:ncol(del13CcampAvgMD)], 'MD')
phl <- merge(merge(del13CcampAvgMD, phl, by=c('month','chamber'), all=T),
             leafChem, by=c('month','chamber'), all=T)

chambs <- data.frame(row.names=1:12)
chambs$chamber <- c(paste0('C0', 1:9), paste0('C', 10:12))
chambs$T_treatment <- rep(c('ambient','warmed'), 6)

phl <- merge(phl, chambs, by='chamber', all=T)

phl$iWUEph_uncorrMD <- phl$CO2chMD*
  (1-((1/(b-a))*(-a + (phl$d13chMD-phl$d13Cph)*1000/(1000+phl$d13Cph))))
phl$iWUEph_corrMD <- phl$iWUEph_uncorrMD - phl$Ci.CcMD
phl$iWUEph_uncorrMDmyAv <- phl$CO2chMD*
  (1-((1/(b-a))*(-a + (phl$d13chMD-phl$d13Cph_corr)*1000/(1000+phl$d13Cph_corr))))
phl$iWUEph_corrMDmyAv <- phl$iWUEph_uncorrMDmyAv - phl$Ci.CcMD

phl$iWUEsunLeaf_uncorrMD <- phl$CO2chMD*
  (1-((1/(b-a))*(-a + (phl$d13chMD-phl$d13CsunLeaf)*1000/(1000+phl$d13CsunLeaf))))
phl$iWUEsunLeaf_corrMD <- phl$iWUEsunLeaf_uncorrMD - phl$Ci.CcMD
phl$iWUEsunLeaf_corrSunGm <- phl$iWUEsunLeaf_uncorrMD - phl$Ci.Cc_sun
phl$iWUEsunLeaf_corrAvg1 <- phl$iWUEsunLeaf_uncorrMD - phl$Ci.Cc_Avg1
phl$iWUEsunLeaf_corrAvg2 <- phl$iWUEsunLeaf_uncorrMD - phl$Ci.Cc_Avg2

phl$iWUEshLeaf_uncorrMD <- phl$CO2chMD*
  (1-((1/(b-a))*(-a + (phl$d13chMD-phl$d13CshLeaf)*1000/(1000+phl$d13CshLeaf))))
phl$iWUEshLeaf_corrMD <- phl$iWUEshLeaf_uncorrMD - phl$Ci.CcMD
phl$iWUEshLeaf_corrShGmH <- phl$iWUEshLeaf_uncorrMD - phl$Ci.Cc_shH
phl$iWUEshLeaf_corrShGmL <- phl$iWUEshLeaf_uncorrMD - phl$Ci.Cc_shL
phl$iWUEshLeaf_corrAvg1 <- phl$iWUEshLeaf_uncorrMD - phl$Ci.Cc_Avg1
phl$iWUEshLeaf_corrAvg2 <- phl$iWUEshLeaf_uncorrMD - phl$Ci.Cc_Avg2

phl$iWUEleafAvg_uncorrMD <- phl$CO2chMD*
  (1-((1/(b-a))*(-a + (phl$d13chMD-phl$d13CleafAvg)*1000/(1000+phl$d13CleafAvg))))
phl$iWUEleafAvg_corrMD <- phl$iWUEleafAvg_uncorrMD - phl$Ci.CcMD
phl$iWUEleafAvg_corrSunGm <- phl$iWUEshLeaf_uncorrMD - phl$Ci.Cc_sun
phl$iWUEleafAvg_corrAvg1 <- phl$iWUEshLeaf_uncorrMD - phl$Ci.Cc_Avg1
phl$iWUEleafAvg_corrAvg2 <- phl$iWUEshLeaf_uncorrMD - phl$Ci.Cc_Avg2

summary(lm(iWUEgeMD ~ iWUEph_uncorrMD, data=phl))
summary(nlme::lme(iWUEgeMD ~ iWUEph_uncorrMD, random = ~1 | fchamber,
                   data = phl, na.action = na.omit))
summary(lm(iWUEgeMD ~ iWUEph_corrMD, data=phl))
summary(nlme::lme(iWUEgeMD ~ iWUEph_corrMD, random = ~1 | fchamber,
                   data = phl, na.action = na.omit))
summary(lm(iWUEgeMD ~ iWUEph_corrMDmyAv, data=phl))
summary(nlme::lme(iWUEgeMD ~ iWUEph_corrMDmyAv, random = ~1 | fchamber,
                  data = phl, na.action = na.omit))
summary(lm(iWUEgeMD ~ iWUEsunLeaf_corrMD, data=phl))
summary(lm(iWUEgeMD ~ iWUEsunLeaf_corrSunGm, data=phl))
summary(lm(iWUEgeMD ~ iWUEsunLeaf_corrAvg1, data=phl))
summary(lm(iWUEgeMD ~ iWUEsunLeaf_corrAvg2, data=phl))
summary(lm(iWUEgeMD ~ iWUEshLeaf_corrMD, data=phl))
summary(lm(iWUEgeMD ~ iWUEshLeaf_corrShGmH, data=phl))
summary(lm(iWUEgeMD ~ iWUEshLeaf_corrShGmL, data=phl))
summary(lm(iWUEgeMD ~ iWUEshLeaf_corrAvg1, data=phl))
summary(lm(iWUEgeMD ~ iWUEshLeaf_corrAvg2, data=phl))
summary(lm(iWUEgeMD ~ iWUEleafAvg_uncorrMD, data=phl))
summary(lm(iWUEgeMD ~ iWUEleafAvg_corrMD, data=phl))
summary(lm(iWUEgeMD ~ iWUEleafAvg_corrSunGm, data=phl))
summary(lm(iWUEgeMD ~ iWUEleafAvg_corrAvg1, data=phl))
summary(lm(iWUEgeMD ~ iWUEleafAvg_corrAvg2, data=phl))
summary(lm(iWUEgeCorrMD ~ iWUEph_uncorrMD, data=phl))
summary(lm(iWUEgeCorrMD ~ iWUEph_uncorrMDmyAv, data=phl))
summary(lm(iWUEgeCorrMD ~ iWUEleafAvg_uncorrMD, data=phl))
summary(lm(d13Cph ~ d13CAnet, data=phl))
summary(lm(d13Cph ~ d13CsunLeaf, data=phl))
summary(lm(d13Cph ~ d13CshLeaf, data=phl))
summary(lm(d13Cph ~ d13CleafAvg, data=phl))
summary(lm(iWUEph_uncorrMD ~ iWUEleafAvg_uncorrMD, data=phl))


# iWUEsummSAFE <- doBy::summaryBy(iWUE.mean.naMD + iWUEge_corr.mean.naMD + iWUEph_uncorrMD2 + iWUEph_corrMD2
#                             + iWUEph_corrMDmyAv + iWUEph_uncorrMDmyAv
#                             + iWUEleafAvg_corrMD + iWUEleafAvg_uncorrMD + iWUEsunLeaf_corrMD 
#                             + iWUEsunLeaf_uncorrMD ~ month + T_treatment, data=phl,
#                             FUN=c(mean.na, s.err.na))

iWUEsumm <- dplyr::summarise(dplyr::group_by(phl, T_treatment, month),
                             iWUEgeMean=mean.na(iWUEgeMD), iWUEgeSE=s.err.na(iWUEgeMD),
                             iWUEgeCorrMean=mean.na(iWUEgeCorrMD), iWUEgeCorrSE=s.err.na(iWUEgeCorrMD),
                             iWUEphUncorrMean=mean.na(iWUEph_uncorrMD), iWUEphUncorrSE=s.err.na(iWUEph_uncorrMD),
                             iWUEphCorrMean=mean.na(iWUEph_corrMD), iWUEphCorrSE=s.err.na(iWUEph_corrMD),
                             iWUEphUncorr2Mean=mean.na(iWUEph_uncorrMDmyAv),
                             iWUEphUncorr2SE=s.err.na(iWUEph_uncorrMDmyAv),
                             iWUEphCorr2Mean=mean.na(iWUEph_corrMDmyAv), iWUEphCorr2SE=s.err.na(iWUEph_corrMDmyAv),
                             iWUEsunLeafCorrMean=mean.na(iWUEsunLeaf_corrMD),
                             iWUEsunLeafCorrSE=s.err.na(iWUEsunLeaf_corrMD),
                             iWUEshLeafCorrMean=mean.na(iWUEshLeaf_corrMD),
                             iWUEshLeafCorrSE=s.err.na(iWUEshLeaf_corrMD),
                             iWUEleafAvgCorrMean=mean.na(iWUEleafAvg_corrMD),
                             iWUEleafAvgCorrSE=s.err.na(iWUEleafAvg_corrMD),
                             d13CphMean=mean.na(d13Cph), d13CphSE=s.err.na(d13Cph),
                             d13CAnetMean=mean.na(d13CAnet), d13CAnetSE=s.err.na(d13CAnet),
                             d13CleafAvgMean=mean.na(d13CleafAvg), d13CleafAvgSE=s.err.na(d13CleafAvg),
                             d13CsunLeafMean=mean.na(d13CsunLeaf), d13CsunLeafAvgSE=s.err.na(d13CsunLeaf),
                             d13CsunLeafMean=mean.na(d13CshLeaf), d13CsunLeafAvgSE=s.err.na(d13CshLeaf),
                             iWUEleafSunAvgCorrSunGmMean=mean.na(iWUEsunLeaf_corrSunGm),
                             iWUEleafSunAvgCorrAvg1Mean=mean.na(iWUEsunLeaf_corrAvg1),
                             iWUEleafSunAvgCorrAvg2Mean=mean.na(iWUEsunLeaf_corrAvg2),
                             iWUEleafShAvgCorrShGmHMean=mean.na(iWUEshLeaf_corrShGmH),
                             iWUEleafShAvgCorrShGmLMean=mean.na(iWUEshLeaf_corrShGmL),
                             iWUEleafShAvgCorrAvg1Mean=mean.na(iWUEshLeaf_corrAvg1),
                             iWUEleafShAvgCorrAvg2Mean=mean.na(iWUEshLeaf_corrAvg2),
                             iWUEleafAvgCorrSunGmMean=mean.na(iWUEleafAvg_corrSunGm),
                             iWUEleafAvgCorrAvg1Mean=mean.na(iWUEleafAvg_corrAvg1),
                             iWUEleafAvgCorrAvg2Mean=mean.na(iWUEleafAvg_corrAvg2))

summary(lm(iWUEgeMean ~ iWUEphUncorrMean, data=iWUEsumm)) #bad
summary(lm(iWUEgeMean ~ iWUEphCorrMean, data=iWUEsumm)) # good
summary(lm(iWUEgeMean ~ iWUEphCorr2Mean, data=iWUEsumm)) # better
summary(lm(iWUEgeMean ~ iWUEsunLeafCorrMean, data=iWUEsumm)) # quite good
summary(lm(iWUEgeMean ~ iWUEleafSunAvgCorrSunGmMean, data=iWUEsumm)) #bad
summary(lm(iWUEgeMean ~ iWUEleafSunAvgCorrAvg1Mean, data=iWUEsumm)) #bad
summary(lm(iWUEgeMean ~ iWUEleafSunAvgCorrAvg2Mean, data=iWUEsumm)) #bad
summary(lm(iWUEgeMean ~ iWUEshLeafCorrMean, data=iWUEsumm)) # not the worst
summary(lm(iWUEgeMean ~ iWUEleafShAvgCorrShGmHMean, data=iWUEsumm)) #bad
summary(lm(iWUEgeMean ~ iWUEleafShAvgCorrShGmLMean, data=iWUEsumm)) #bad
summary(lm(iWUEgeMean ~ iWUEleafShAvgCorrAvg1Mean, data=iWUEsumm)) #bad
summary(lm(iWUEgeMean ~ iWUEleafShAvgCorrAvg2Mean, data=iWUEsumm)) #bad
summary(lm(iWUEgeMean ~ iWUEleafAvgCorrMean, data=iWUEsumm)) # good
summary(lm(iWUEgeMean ~ iWUEleafAvgCorrSunGmMean, data=iWUEsumm)) #bad
summary(lm(iWUEgeMean ~ iWUEleafAvgCorrAvg1Mean, data=iWUEsumm)) #bad
summary(lm(iWUEgeMean ~ iWUEleafAvgCorrAvg2Mean, data=iWUEsumm)) #bad
summary(lm(iWUEgeCorrMean ~ iWUEphUncorrMean, data=iWUEsumm)) # good
summary(lm(iWUEgeCorrMean ~ iWUEphUncorr2Mean, data=iWUEsumm)) # good too
summary(lm(iWUEphCorrMean ~ iWUEsunLeafCorrMean, data=iWUEsumm)) # good
summary(lm(iWUEphCorrMean ~ iWUEshLeafCorrMean, data=iWUEsumm)) # quite good
summary(lm(iWUEphCorrMean ~ iWUEleafAvgCorrMean, data=iWUEsumm)) #better
summary(lm(iWUEphCorrMean ~ iWUEleafAvgCorrSunGmMean, data=iWUEsumm)) # bad
summary(lm(d13CleafAvgMean ~ d13CphMean, data=iWUEsumm))
summary(lm(d13CsunLeafMean ~ d13CphMean, data=iWUEsumm))


write.csv(iWUEsumm, file='output/iWUEsumm.csv', row.names = F)

# Correlations that go into table 1

corrResults <- list()
corrResults[[1]] <- summary(nlme::lme(iWUEgeMD ~ iWUEph_uncorrMD, random = ~1 | fchamber,
                  data = phl, na.action = na.omit))
table1 <- data.frame(row.names = 1:length(corrResults))
for (i in 1:length(corrResults)){
  table1$one <- stringi::stri_split_fixed(as.character(corrResults[[i]]$call[2]), "~")[[1]][1]
  table1$two <- stringi::stri_split_fixed(as.character(corrResults[[i]]$call[2]), "~")[[1]][2]
  table1$intercept <- corrResults[[i]]$tTable[1]
  table1$interceptSE <- corrResults[[i]]$tTable[3]
  table1$interceptP <- corrResults[[i]]$tTable[9]
  table1$slope <- corrResults[[i]]$tTable[2]
  table1$slopeSE <- corrResults[[i]]$tTable[4]
  table1$slopeP <- corrResults[[i]]$tTable[10]
}
