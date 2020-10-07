source('scripts/canopy_gmes_more3.R')
source('master_scripts/phloem_plotting.R')
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
                                                           & deltaSubstrate >= -60 & deltaSubstrate <= 0
                                                           & condAlert == 'no')),
                                              chamber, month), d13CAnet=mean(deltaSubstrate, na.rm=T))
phl <- merge(phl, photoSumm, by=c('chamber','month'), all=T)
# in my case d13Cphloem is more DEPLETED than d13CAnet
phl$phlOffset <- phl$d13CAnet - phl$d13Cph
# calculate offset between phloem d13C and d13C of photosynthesis
round(mean(doBy::summaryBy(phlOffset ~ month, data=phl, FUN=mean.na)$phlOffset.mean.na, na.rm=T), 1)
round(s.err.na(doBy::summaryBy(phlOffset ~ month, data=phl, FUN=mean.na)$phlOffset.mean.na), 1)
# calculate a "corrected" d13Cph value
phl$d13Cph_corr <- phl$d13Cph +
  mean(doBy::summaryBy(phlOffset ~ month, data=phl, FUN=mean.na)$phlOffset.mean.na, na.rm=T)
# calculate mean mid-day values of d13Camb and Camb
del13CcampAvgMD <- dplyr::summarise(dplyr::group_by(setDT(subset(allPaired, A_area > 0 & E_area > 0 &
                                                                   PAR >= 800 & iWUE < 410 & midday=='yes')),
                                                    month, chamber), 
                                    iWUEge=mean.na(iWUE), d13ch=mean.na(del13Cch),
                                    CO2ch=mean.na(CO2sampleWTC), A=mean.na(A_area), gm=mean.na(gmes_area))
names(del13CcampAvgMD)[3:ncol(del13CcampAvgMD)] <- paste0(names(del13CcampAvgMD)[3:ncol(del13CcampAvgMD)], 'MD')
phl <- merge(merge(del13CcampAvgMD, phl, by=c('month','chamber'), all=T),
             leafChem, by=c('month','chamber'), all=T)

chambs <- read.csv('data/trtkey2.csv')

phl <- merge(phl, chambs, by=c('chamber', 'month'), all=T)
# calculate DELTAobs with d13Cph
phl$DELTAph <- (phl$d13chMD - phl$d13Cph) * 1000/(1000 + phl$d13Cph)
# incorporate the presumed effect of post-photosynthetic fractionation
phl$DELTAph_corr <- (phl$d13chMD - phl$d13Cph_corr) * 1000/(1000 + phl$d13Cph_corr)

# calculate photosynthetic discrimination from d13Cph
phl$iWUEph_corrMD <- (phl$CO2chMD*(b-phl$DELTAph)-(b-ai)*(phl$AMD/phl$gmMD))/(1.6*(b-a))
phl$iWUEph_uncorrMD <- phl$CO2chMD*(1-((phl$DELTAph-a)/(b-a)))/1.6

summary(lm(iWUEgeMD ~ iWUEph_uncorrMD, data=phl))
summary(nlme::lme(iWUEgeMD ~ iWUEph_uncorrMD, random = ~1 | fchamber,
                  data = phl, na.action = na.omit))
# no significant correlation between iWUE estimates when gm limitation is not incorporated
summary(lm(iWUEgeMD ~ iWUEph_corrMD, data=phl))
summary(nlme::lme(iWUEgeMD ~ iWUEph_corrMD, random = ~1 | fchamber,
                  data = phl, na.action = na.omit))
# significant correlation between iWUE estimates when incorporating gm limitation
summary(nlme::lme(iWUEgeMD ~ iWUEph_corrMD, random = ~1 | fchamber,
                  data = subset(phl, W_treatment == 'control'), na.action = na.omit))
summary(nlme::lme(iWUEgeMD ~ iWUEph_corrMD*T_treatment, random = ~1 | fchamber,
                  data = subset(phl, month!='Feb' & month!='Mar'), na.action = na.omit))
# no effect of the temperature treatment on this correlation
summary(nlme::lme(iWUEgeMD ~ iWUEph_corrMD*T_treatment*W_treatment, random = ~1 | fchamber,
                  data = subset(phl, month=='Feb' | month=='Mar'), na.action = na.omit))
summary(nlme::lme(iWUEgeMD ~ iWUEph_corrMD+T_treatment:iWUEph_corrMD+W_treatment:iWUEph_corrMD, random = ~1 | fchamber,
                  data = subset(phl, month=='Feb' | month=='Mar'), na.action = na.omit))
# no effect of the water treatment, but correlation is ns!
# when removing from the analyses the temp and warming on the intercept is marginally significant

# calculate iWUE using d13Cph corrected for presumed post-photosynthetic fractionation
phl$iWUEph_corrMDmyAv <- (phl$CO2chMD*(b-phl$DELTAph_corr)-(b-ai)*(phl$AMD/phl$gmMD))/(1.6*(b-a))
phl$iWUEph_uncorrMD <- phl$CO2chMD*(1-((phl$DELTAph_corr-a)/(b-a)))/1.6

summary(lm(iWUEgeMD ~ iWUEph_corrMDmyAv, data=phl))
summary(nlme::lme(iWUEgeMD ~ iWUEph_corrMDmyAv, random = ~1 | fchamber,
                  data = phl, na.action = na.omit))
# the intercept becomes non-signficantly different from zero :-)

# calculate DELTAobs from d13Cleaf different combinations
phl$DELTAsunLeaf <- (phl$d13chMD - phl$d13CsunLeaf) * 1000/(1000 + phl$d13CsunLeaf)
phl$DELTAshLeaf <- (phl$d13chMD - phl$d13CshLeaf) * 1000/(1000 + phl$d13CshLeaf)
phl$DELTAleafAvg <- (phl$d13chMD - phl$d13CleafAvg) * 1000/(1000 + phl$d13CleafAvg)

# calculate iWUE from d13Cleaf differnt combinations
phl$iWUEsunLeaf_corrMD <- (phl$CO2chMD*(b-phl$DELTAsunLeaf)-(b-ai)*(phl$AMD/phl$gmMD))/(1.6*(b-a))
phl$iWUEsunLeaf_uncorrMD <- phl$CO2chMD*(1-((phl$DELTAsunLeaf-a)/(b-a)))/1.6
phl$iWUEshLeaf_corrMD <- (phl$CO2chMD*(b-phl$DELTAshLeaf)-(b-ai)*(phl$AMD/phl$gmMD))/(1.6*(b-a))
phl$iWUEshLeaf_uncorrMD <- phl$CO2chMD*(1-((phl$DELTAshLeaf-a)/(b-a)))/1.6
phl$iWUEleafAvg_corrMD <- (phl$CO2chMD*(b-phl$DELTAleafAvg)-(b-ai)*(phl$AMD/phl$gmMD))/(1.6*(b-a))
phl$iWUEleafAvg_uncorrMD <- phl$CO2chMD*(1-((phl$DELTAleafAvg-a)/(b-a)))/1.6


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

# correlations between d13Cph and d13CAnet
summary(lm(d13Cph ~ d13CAnet, data=phl))
summary(lm(d13Cph ~ d13CAnet, data=subset(phl, month != 'Jan' & month != 'Dec')))
summary(nlme::lme(d13CAnet ~ d13Cph, random = ~1 | fchamber,
                  data = phl, na.action = na.omit))
# significant (but poor) correlation beteeen d13Cph and d13CAnet
summary(nlme::lme(d13CAnet ~ d13Cph, random = ~1 | fchamber,
                  data = subset(phl, month!='Jan' & month!='Dec'), na.action = na.omit))
# excluding Jan and Dec data renders much better correlation

# correlation between d13Cph and d13Cleaf
summary(lm(d13Cph ~ d13CsunLeaf, data=phl))
summary(lm(d13Cph ~ d13CshLeaf, data=phl))
summary(lm(d13Cph ~ d13CleafAvg, data=phl))
summary(nlme::lme(d13Cph ~ d13CleafAvg, random = ~1 | fchamber,
                  data = phl, na.action = na.omit))
# significant correlation between d13Cph and d13Cleaf

iWUEsumm <- dplyr::summarise(dplyr::group_by(phl, T_treatment, month),
                             iWUEgeMean=mean.na(iWUEgeMD), iWUEgeSE=s.err.na(iWUEgeMD),
                             iWUEphUncorrMean=mean.na(iWUEph_uncorrMD), iWUEphUncorrSE=s.err.na(iWUEph_uncorrMD),
                             iWUEphCorrMean=mean.na(iWUEph_corrMD), iWUEphCorrSE=s.err.na(iWUEph_corrMD),
                             iWUEphCorr2Mean=mean.na(iWUEph_corrMDmyAv), iWUEphCorr2SE=s.err.na(iWUEph_corrMDmyAv),
                             iWUEsunLeafCorrMean=mean.na(iWUEsunLeaf_corrMD),
                             iWUEsunLeafCorrSE=s.err.na(iWUEsunLeaf_corrMD),
                             iWUEshLeafCorrMean=mean.na(iWUEshLeaf_corrMD),
                             iWUEshLeafCorrSE=s.err.na(iWUEshLeaf_corrMD),
                             iWUEleafAvgUncorrMean=mean.na(iWUEleafAvg_uncorrMD),
                             iWUEleafAvgUncorrSE=s.err.na(iWUEleafAvg_uncorrMD),
                             iWUEleafAvgCorrMean=mean.na(iWUEleafAvg_corrMD),
                             iWUEleafAvgCorrSE=s.err.na(iWUEleafAvg_corrMD),
                             d13CphMean=mean.na(d13Cph), d13CphSE=s.err.na(d13Cph),
                             # d13CAnetMean=mean.na(d13CAnet), d13CAnetSE=s.err.na(d13CAnet)#,
                             # d13CleafAvgMean=mean.na(d13CleafAvg), d13CleafAvgSE=s.err.na(d13CleafAvg),
                             # d13CsunLeafMean=mean.na(d13CsunLeaf), d13CsunLeafAvgSE=s.err.na(d13CsunLeaf),
                             # d13CsunLeafMean=mean.na(d13CshLeaf), d13CsunLeafAvgSE=s.err.na(d13CshLeaf),
                             # iWUEleafSunAvgCorrSunGmMean=mean.na(iWUEsunLeaf_corrSunGm),
                             # iWUEleafSunAvgCorrAvg1Mean=mean.na(iWUEsunLeaf_corrAvg1),
                             # iWUEleafSunAvgCorrAvg2Mean=mean.na(iWUEsunLeaf_corrAvg2),
                             # iWUEleafShAvgCorrShGmHMean=mean.na(iWUEshLeaf_corrShGmH),
                             # iWUEleafShAvgCorrShGmLMean=mean.na(iWUEshLeaf_corrShGmL),
                             # iWUEleafShAvgCorrAvg1Mean=mean.na(iWUEshLeaf_corrAvg1),
                             # iWUEleafShAvgCorrAvg2Mean=mean.na(iWUEshLeaf_corrAvg2),
                             # iWUEleafAvgCorrSunGmMean=mean.na(iWUEleafAvg_corrSunGm),
                             # iWUEleafAvgCorrAvg1Mean=mean.na(iWUEleafAvg_corrAvg1),
                             # iWUEleafAvgCorrAvg1SE=s.err.na(iWUEleafAvg_corrAvg1),
                             # iWUEleafAvgCorrAvg2Mean=mean.na(iWUEleafAvg_corrAvg2)
                             )

# THIS IS THE ONE FOR FIGURE 3A
summary(lm(iWUEgeMean ~ iWUEphUncorrMean, data=iWUEsumm)) #bad
# THIS IS THE ONE FOR FIGURE 3B
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
# THIS IS FOR FIGURE S4A
summary(lm(iWUEgeMean ~ iWUEleafAvgUncorrMean, data=iWUEsumm)) # very bad
# THIS IS FOR FIGURE S4B
summary(lm(iWUEgeMean ~ iWUEleafAvgCorrMean, data=iWUEsumm)) # good
summary(lm(iWUEgeMean ~ iWUEleafAvgCorrSunGmMean, data=iWUEsumm)) #bad
# THIS IS FOR FIGURE S4C
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