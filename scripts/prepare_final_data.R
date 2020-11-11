source('scripts/canopy_gmes_more3.R')
chambs <- read.csv('data/trtkey2.csv')
camps <- read.csv('data/camp_names.csv')
allPaired <- merge(allPaired, chambs[, c('month', 'chamber', 'W_treatment')], by = c('month', 'chamber'), all.x = T, all.y = F)
allPaired <- merge(allPaired, camps, by = 'month', all = T)
gmesExport <- subset(allPaired, PAR >= 800 & midday == 'yes' & gmes_area > 0 & W_treatment == 'control')
gmesExport <- gmesExport[, c('campaign', 'chamber', 'T_treatment', 'W_treatment', 'datetimeFM', 'CO2sampleWTC',
                            'Tair_al', 'VPDmol', 'PAR', 'A_area', 'E_area', 'Ci', 'gmes_area')]
names(gmesExport)[c(5:7)] <- c('DateTime', 'Ca', 'Tair')
gmesExport <- doBy::orderBy(~ chamber + DateTime, data = gmesExport)
write.csv(gmesExport, 'output/WTC_TEMP_gmes_subdaily_20131018-20140420_L2.csv', row.names = F)

source('master_scripts/phloem_plotting.R')
phl$fchamber <- as.factor(phl$chamber)
phl$month <- factor(phl$month, levels=c('Oct','Dec','Jan','Feb','Mar','Apr'))
phl <- merge(phl, chambs, by=c('chamber', 'month'), all=T)
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
                                                                   PAR >= 800 & iWUE < 400 & midday=='yes')),
                                                    month, chamber), 
                                    iWUEge=mean.na(iWUE), d13ch=mean.na(del13Cch), gs = mean.na(gs_area),
                                    CO2ch=mean.na(CO2sampleWTC), A=mean.na(A_area), gm=mean.na(gmes_area),
                                    Gstar=mean.na(gammaStar), Rday=mean.na(Rd_corrT))
names(del13CcampAvgMD)[3:ncol(del13CcampAvgMD)] <- paste0(names(del13CcampAvgMD)[3:ncol(del13CcampAvgMD)], 'MD')
phl <- merge(del13CcampAvgMD, phl, by=c('month','chamber'), all=T)

# calculate DELTAobs with d13Cph
phl$DELTAph <- (phl$d13chMD - phl$d13Cph) * 1000/(1000 + phl$d13Cph)
# incorporate the presumed effect of post-photosynthetic fractionation
phl$DELTAph_corr <- (phl$d13chMD - phl$d13Cph_corr) * 1000/(1000 + phl$d13Cph_corr)

# calcule iWUE from Delta with b = 27, Eq. 4 in the main text
phl$iWUEph_uncorrMD <- (phl$CO2chMD/1.6)*((27-phl$DELTAph)/(27-a))
# calculate iWUE from Delta incorporating gm and b = 29. Eq. 9 in the main text
phl$iWUEph_corrMD <- (phl$CO2chMD*(b-phl$DELTAph)+(ai-b)*(phl$AMD/phl$gmMD))/(1.6*(b-a))

# calculate iWUE using d13Cph corrected for presumed post-photosynthetic fractionation
phl$iWUEph_corrMDmyAv <- (phl$CO2chMD*(b-phl$DELTAph_corr)+(ai-b)*(phl$AMD/phl$gmMD))/(1.6*(b-a))
phlExport <- merge(phl, camps, by = 'month', all.x = T, all.y = F)
phlExport <- phlExport[, c('campaign.y', 'chamber', 'T_treatment', 'W_treatment', 'AMD', 'iWUEgeMD',
                           'gmMD', 'CO2chMD', 'd13chMD', 'd13Cph', 'd13CAnet', 'GstarMD', 'RdayMD')]
names(phlExport) <- c('campaign', 'chamber', 'T_treatment', 'W_treatment', 'Anet_Avg','iWUEge_Avg',
                      'gmes_Avg', 'Ca_Avg', 'd13Cch_Avg', 'd13Cph', 'd13Anet', 'Gstar_Avg', 'Rday_Avg')
write.csv(phlExport, 'output/WTC_TEMP_gmes_20131018-20140420_L2.csv', row.names = F)

