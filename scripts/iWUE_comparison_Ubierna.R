source('scripts/canopy_gmes_more3.R')
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
                                                           & deltaSubstrate >= -60 & deltaSubstrate <= 0
                                                           & condAlert == 'no')),
                                              chamber, month), d13CAnet=mean(deltaSubstrate, na.rm=T))
gmesSumm <- dplyr::summarise(dplyr::group_by(setDT(subset(allPaired, midday == 'yes' & A_area > 0
                                                          & PAR >= 800 & condAlert == 'no')),
                                             chamber, month), gm = mean(gmes_area, na.rm = T),
                             DEL = mean(DELTA, na.rm = T), n=lengthWithoutNA(gmes_area))
phl <- merge(phl, photoSumm, by=c('chamber','month'), all=T)
phl <- merge(phl, gmesSumm, by=c('chamber', 'month'), all = T)
# in my case d13Cphloem is more DEPLETED than d13CAnet
phl$phlOffset <- phl$d13CAnet - phl$d13Cph
# there are differences in d13Cph and d13CAnet among campaings, but not between temperature treatments
# model <- nlme::lme(phlOffset ~ month + temp, random = ~1 | fchamber,
#                    data = phl, na.action = na.omit)
# anova(model)
# model <- nlme::lme(d13Cph ~ month + temp, random = ~1 | fchamber,
#                    data = phl, na.action = na.omit)
# anova(model)
# model <- nlme::lme(d13CAnet ~ month + temp, random = ~1 | fchamber,
#                    data = phl, na.action = na.omit)
# anova(model)
# differences in d13Cph and d13CAnet are not coordinated so differences in phl offset do not make sense
# use the mean of all values
round(mean(doBy::summaryBy(phlOffset ~ month, data=phl, FUN=mean.na)$phlOffset.mean.na, na.rm=T), 1)
round(s.err.na(doBy::summaryBy(phlOffset ~ month, data=phl, FUN=mean.na)$phlOffset.mean.na), 1)

# calculate photosynthetic discrimination from d13Cph