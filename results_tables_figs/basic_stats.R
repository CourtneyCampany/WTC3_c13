source('scripts/canopy_gmes_more3.R')
source('master_scripts/phloem_plotting.R')
photoSumm <- dplyr::summarise(dplyr::group_by(setDT(subset(allPaired, midday=='yes' & A_area > 0 & E_area > 0 & PAR >= 800
                                                           & deltaSubstrate >= -60 & deltaSubstrate <= 0)),
                                              chamber, month), d13CAnet=mean(deltaSubstrate, na.rm=T))
phl <- merge(phl, photoSumm, by=c('month', 'chamber'), all = T)
chambs <- read.csv('data/trtkey2.csv')
phl <- as.data.frame(dplyr::left_join(phl, chambs, by=c('chamber', 'month')))


allPaired$iWUE <- ifelse(allPaired$iWUE > 400, NA, allPaired$iWUE)
gmesMDsumm1 <- dplyr::summarise(dplyr::group_by(setDT(subset(allPaired, midday=='yes' & PAR >= 800 & A_area > 0 & E_area > 0)),
                                                month, chamber), gmesCh = mean.na(gmes_area),
                                gmesChSE=s.err.na(gmes_area), gmesChN=lengthWithoutNA(gmes_area),
                                gsCh = mean.na(gs_area), gsChSE=s.err.na(gs_area),
                                ACh = mean.na(A_area), AChSE=s.err.na(A_area),
                                iWUEch = mean.na(iWUE), iWUEse=s.err.na(iWUE))
gmesMDsumm1 <- dplyr::left_join(gmesMDsumm1, chambs, by=c('chamber','month'))
gmesMDsumm2 <- dplyr::summarise(dplyr::group_by(subset(gmesMDsumm1, gmesChN >= 3),
                                                month, T_treatment, W_treatment),
                                gmesT=mean.na(gmesCh), gmesTse=s.err.na(gmesCh),
                                gsT=mean.na(gsCh), gsTse=s.err.na(gsCh),
                                AT=mean.na(ACh), ATse=s.err.na(ACh),
                                iWUEt=mean.na(iWUEch), iWUEtSE=s.err.na(iWUEch))
gmesMDsumm3 <- dplyr::summarise(dplyr::group_by(gmesMDsumm2, T_treatment, W_treatment),
                                gmesTem=mean.na(gmesT), gmesTemSe=s.err.na(gmesT),
                                gsTem=mean.na(gsT), gsTemSE=s.err.na(gsT),
                                Atem=mean.na(AT), AtemSE=s.err.na(AT),
                                iWUEtem=mean.na(iWUEt), iWUEtemSE=s.err.na(iWUEt))
phlSumm1 <- dplyr::summarise(dplyr::group_by(setDT(phl), month, T_treatment, W_treatment),
                              d13CphMean = mean.na(d13Cph), d13CphSE = s.err.na(d13Cph))
write.csv(gmesMDsumm2, file='output/dataFigure1a.csv', row.names = F)
write.csv(gmesMDsumm3, file='output/dataFigure1b.csv', row.names = F)
write.csv(phlSumm1, file='output/dataFigureS2d.csv', row.names = F)

gmesL <- list()
chambs <- c(paste0('C0', 1:9), paste0('C', 10:12))
for (i in 1:length(levels(as.factor(allPaired$chamber)))){
  gmesL[[i]] <- subset(allPaired, chamber==chambs[i] & gmes_area < 0.3)
}
lapply(gmesL, function(x) write.csv(x, file=paste0('calculated_data/indv_chambs/', x[1,'chamber'], '.csv'),
                                    row.names = F))
basicStats1 <- list()

basicStats1[[1]] <- nlme::lme(log(gmes_area*1000) ~ month + T_treatment + month:T_treatment, random = ~1 | fchamber,
                   data = subset(allPaired, midday == 'yes' & PAR >= 800 & A_area > 0 &
                                   datetimeFM <= as.Date('2014-02-01')), na.action = na.omit)
basicStats1[[2]] <- nlme::lme(gsc_area ~ month + T_treatment + month:T_treatment, random = ~1 | fchamber,
                              data = subset(allPaired, midday == 'yes' & PAR >= 800 & A_area > 0 &
                                              datetimeFM <= as.Date('2014-02-01')), na.action = na.omit)
basicStats1[[3]] <- nlme::lme(A_area ~month + T_treatment + month:T_treatment, random = ~1 | fchamber,
                              data = subset(allPaired, midday == 'yes' & PAR >= 800 & A_area > 0 &
                                              datetimeFM <= as.Date('2014-02-01')), na.action = na.omit)
basicStats1[[4]] <- nlme::lme(iWUE ~ month + T_treatment + month:T_treatment, random = ~1 | fchamber,
                              data = subset(allPaired, midday == 'yes' & PAR >= 800 & A_area > 0 &
                                              datetimeFM <= as.Date('2014-02-01')), na.action = na.omit)
basicStats1[[5]] <- nlme::lme(d13Cph ~ month + T_treatment + month:T_treatment, random = ~1 | as.factor(chamber),
                              data = subset(phl,  month != 'Feb' & month != 'Mar' & month != 'Apr'), na.action = na.omit)
basicStats1[[6]] <- nlme::lme(d13CAnet ~ month + T_treatment + month:T_treatment, random = ~1 | as.factor(chamber),
                              data = subset(phl,  month != 'Feb' & month != 'Mar' & month != 'Apr'), na.action = na.omit)
tableBasicStats1 <- data.frame(row.names = 1:(length(basicStats1)*3))
for (i in 1:length(basicStats1)){
  tableBasicStats1$variable[i*3-2] <- stringi::stri_split_fixed(as.character(summary(basicStats1[[i]])
                                                                        $call[2]), "~")[[1]][1]
  tableBasicStats1$factor[i*3-2] <- stringi::stri_split_fixed(stringi::stri_split_fixed(as.character
                                                                                     (summary(basicStats1[[i]])
                                                                        $call[2]), "~")[[1]][2], '+')[[1]][1]
  tableBasicStats1$Fvalue[i*3-2] <- round(anova(basicStats1[[i]])[2, 3])
  tableBasicStats1$Pvalue[i*3-2] <- round(anova(basicStats1[[i]])[2, 4], 3)
  tableBasicStats1$R2m[i*3-2] <- round(MuMIn::r.squaredGLMM(basicStats1[[i]])[1], 2)
  tableBasicStats1$R2c[i*3-2] <- round(MuMIn::r.squaredGLMM(basicStats1[[i]])[2], 2)
  tableBasicStats1$variable[i*3-1] <- stringi::stri_split_fixed(as.character(summary(basicStats1[[i]])
                                                                             $call[2]), "~")[[1]][1]
  tableBasicStats1$factor[i*3-1] <- stringi::stri_split_fixed(stringi::stri_split_fixed(as.character
                                                                                        (summary(basicStats1[[i]])
                                                                            $call[2]), "~")[[1]][2], '+')[[1]][2]
  tableBasicStats1$Fvalue[i*3-1] <- round(anova(basicStats1[[i]])[3, 3])
  tableBasicStats1$Pvalue[i*3-1] <- round(anova(basicStats1[[i]])[3, 4], 3)
  tableBasicStats1$R2m[i*3-1] <- round(MuMIn::r.squaredGLMM(basicStats1[[i]])[1], 2)
  tableBasicStats1$R2c[i*3-1] <- round(MuMIn::r.squaredGLMM(basicStats1[[i]])[2], 2)
  tableBasicStats1$variable[i*3] <- stringi::stri_split_fixed(as.character(summary(basicStats1[[i]])
                                                                             $call[2]), "~")[[1]][1]
  tableBasicStats1$factor[i*3] <- stringi::stri_split_fixed(stringi::stri_split_fixed(as.character
                                                                                        (summary(basicStats1[[i]])
                                                                            $call[2]), "~")[[1]][2], '+')[[1]][3]
  tableBasicStats1$Fvalue[i*3] <- round(anova(basicStats1[[i]])[4, 3])
  tableBasicStats1$Pvalue[i*3] <- round(anova(basicStats1[[i]])[4, 4], 3)
  tableBasicStats1$R2m[i*3] <- round(MuMIn::r.squaredGLMM(basicStats1[[i]])[1], 2)
  tableBasicStats1$R2c[i*3] <- round(MuMIn::r.squaredGLMM(basicStats1[[i]])[2], 2)
}

basicStats1 <- list()

basicStats1[[1]] <- nlme::lme(log(gmes_area*1000) ~ month + T_treatment + Water_treatment
                              + month:T_treatment + month:Water_treatment + T_treatment:Water_treatment
                              + month:T_treatment:Water_treatment, random = ~1 | fchamber,
                              data = subset(allPaired, midday == 'yes' & PAR >= 800 & A_area > 0 &
                                              datetimeFM >= as.Date('2014-02-01')), na.action = na.omit)
basicStats1[[2]] <- nlme::lme(gsc_area ~ month + T_treatment + Water_treatment
                              + month:T_treatment + month:Water_treatment + T_treatment:Water_treatment
                              + month:T_treatment:Water_treatment, random = ~1 | fchamber,
                              data = subset(allPaired, midday == 'yes' & PAR >= 800 & A_area > 0 &
                                              datetimeFM >= as.Date('2014-02-01')), na.action = na.omit)
basicStats1[[3]] <- nlme::lme(A_area ~ month + T_treatment + Water_treatment
                              + month:T_treatment + month:Water_treatment + T_treatment:Water_treatment
                              + month:T_treatment:Water_treatment, random = ~1 | fchamber,
                              data = subset(allPaired, midday == 'yes' & PAR >= 800 & A_area > 0 &
                                              datetimeFM >= as.Date('2014-02-01')), na.action = na.omit)
basicStats1[[4]] <- nlme::lme(iWUE ~ month + T_treatment + Water_treatment
                              + month:T_treatment + month:Water_treatment + T_treatment:Water_treatment
                              + month:T_treatment:Water_treatment, random = ~1 | fchamber,
                              data = subset(allPaired, midday == 'yes' & PAR >= 800 & A_area > 0 &
                                              datetimeFM >= as.Date('2014-02-01')), na.action = na.omit)
basicStats1[[5]] <- nlme::lme(d13Cph ~ month + T_treatment + W_treatment
                              + month:T_treatment + month:W_treatment + T_treatment:W_treatment
                              + month:T_treatment:W_treatment , random = ~1 | as.factor(chamber),
                              data = subset(phl,  month == 'Feb' | month == 'Mar'), na.action = na.omit)
basicStats1[[6]] <- nlme::lme(d13CAnet ~ month + T_treatment + W_treatment
                              + month:T_treatment + month:W_treatment + T_treatment:W_treatment
                              + month:T_treatment:W_treatment , random = ~1 | as.factor(chamber),
                              data = subset(phl,  month == 'Feb' | month == 'Mar' | month == 'Apr'), na.action = na.omit)
tableBasicStats2 <- data.frame(row.names = 1:(length(basicStats1)*7))
for (i in 1:length(basicStats1)){
  tableBasicStats2$variable[i*7-6] <- stringi::stri_split_fixed(as.character(summary(basicStats1[[i]])
                                                                             $call[2]), "~")[[1]][1]
  tableBasicStats2$factor[i*7-6] <- stringi::stri_split_fixed(stringi::stri_split_fixed(as.character
                                                                          (summary(basicStats1[[i]])
                                                                          $call[2]), "~")[[1]][2], '+')[[1]][1]
  tableBasicStats2$Fvalue[i*7-6] <- round(anova(basicStats1[[i]])[2, 3])
  tableBasicStats2$Pvalue[i*7-6] <- round(anova(basicStats1[[i]])[2, 4], 3)
  tableBasicStats2$R2m[i*7-6] <- round(MuMIn::r.squaredGLMM(basicStats1[[i]])[1], 2)
  tableBasicStats2$R2c[i*7-6] <- round(MuMIn::r.squaredGLMM(basicStats1[[i]])[2], 2)
  tableBasicStats2$variable[i*7-5] <- stringi::stri_split_fixed(as.character(summary(basicStats1[[i]])
                                                                             $call[2]), "~")[[1]][1]
  tableBasicStats2$factor[i*7-5] <- stringi::stri_split_fixed(stringi::stri_split_fixed(as.character
                                                                          (summary(basicStats1[[i]])
                                                                          $call[2]), "~")[[1]][2], '+')[[1]][2]
  tableBasicStats2$Fvalue[i*7-5] <- round(anova(basicStats1[[i]])[3, 3])
  tableBasicStats2$Pvalue[i*7-5] <- round(anova(basicStats1[[i]])[3, 4], 3)
  tableBasicStats2$R2m[i*7-5] <- round(MuMIn::r.squaredGLMM(basicStats1[[i]])[1], 2)
  tableBasicStats2$R2c[i*7-5] <- round(MuMIn::r.squaredGLMM(basicStats1[[i]])[2], 2)
  tableBasicStats2$variable[i*7-4] <- stringi::stri_split_fixed(as.character(summary(basicStats1[[i]])
                                                                           $call[2]), "~")[[1]][1]
  tableBasicStats2$factor[i*7-4] <- stringi::stri_split_fixed(stringi::stri_split_fixed(as.character
                                                                            (summary(basicStats1[[i]])
                                                                            $call[2]), "~")[[1]][2], '+')[[1]][3]
  tableBasicStats2$Fvalue[i*7-4] <- round(anova(basicStats1[[i]])[4, 3])
  tableBasicStats2$Pvalue[i*7-4] <- round(anova(basicStats1[[i]])[4, 4], 3)
  tableBasicStats2$R2m[i*7-4] <- round(MuMIn::r.squaredGLMM(basicStats1[[i]])[1], 2)
  tableBasicStats2$R2c[i*7-4] <- round(MuMIn::r.squaredGLMM(basicStats1[[i]])[2], 2)
  tableBasicStats2$variable[i*7-3] <- stringi::stri_split_fixed(as.character(summary(basicStats1[[i]])
                                                                             $call[2]), "~")[[1]][1]
  tableBasicStats2$factor[i*7-3] <- stringi::stri_split_fixed(stringi::stri_split_fixed(as.character
                                                                            (summary(basicStats1[[i]])
                                                                            $call[2]), "~")[[1]][2], '+')[[1]][4]
  tableBasicStats2$Fvalue[i*7-3] <- round(anova(basicStats1[[i]])[5, 3])
  tableBasicStats2$Pvalue[i*7-3] <- round(anova(basicStats1[[i]])[5, 4], 3)
  tableBasicStats2$R2m[i*7-3] <- round(MuMIn::r.squaredGLMM(basicStats1[[i]])[1], 2)
  tableBasicStats2$R2c[i*7-3] <- round(MuMIn::r.squaredGLMM(basicStats1[[i]])[2], 2)
  tableBasicStats2$variable[i*7-2] <- stringi::stri_split_fixed(as.character(summary(basicStats1[[i]])
                                                                             $call[2]), "~")[[1]][1]
  tableBasicStats2$factor[i*7-2] <- stringi::stri_split_fixed(stringi::stri_split_fixed(as.character
                                                                          (summary(basicStats1[[i]])
                                                                          $call[2]), "~")[[1]][2], '+')[[1]][5]
  tableBasicStats2$Fvalue[i*7-2] <- round(anova(basicStats1[[i]])[6, 3])
  tableBasicStats2$Pvalue[i*7-2] <- round(anova(basicStats1[[i]])[6, 4], 3)
  tableBasicStats2$R2m[i*7-2] <- round(MuMIn::r.squaredGLMM(basicStats1[[i]])[1], 2)
  tableBasicStats2$R2c[i*7-2] <- round(MuMIn::r.squaredGLMM(basicStats1[[i]])[2], 2)
  tableBasicStats2$variable[i*7-1] <- stringi::stri_split_fixed(as.character(summary(basicStats1[[i]])
                                                                             $call[2]), "~")[[1]][1]
  tableBasicStats2$factor[i*7-1] <- stringi::stri_split_fixed(stringi::stri_split_fixed(as.character
                                                                            (summary(basicStats1[[i]])
                                                                            $call[2]), "~")[[1]][2], '+')[[1]][6]
  tableBasicStats2$Fvalue[i*7-1] <- round(anova(basicStats1[[i]])[7, 3])
  tableBasicStats2$Pvalue[i*7-1] <- round(anova(basicStats1[[i]])[7, 4], 3)
  tableBasicStats2$R2m[i*7-1] <- round(MuMIn::r.squaredGLMM(basicStats1[[i]])[1], 2)
  tableBasicStats2$R2c[i*7-1] <- round(MuMIn::r.squaredGLMM(basicStats1[[i]])[2], 2)
  tableBasicStats2$variable[i*7] <- stringi::stri_split_fixed(as.character(summary(basicStats1[[i]])
                                                                             $call[2]), "~")[[1]][1]
  tableBasicStats2$factor[i*7] <- stringi::stri_split_fixed(stringi::stri_split_fixed(as.character
                                                                            (summary(basicStats1[[i]])
                                                                            $call[2]), "~")[[1]][2], '+')[[1]][7]
  tableBasicStats2$Fvalue[i*7] <- round(anova(basicStats1[[i]])[8, 3])
  tableBasicStats2$Pvalue[i*7] <- round(anova(basicStats1[[i]])[8, 4], 3)
  tableBasicStats2$R2m[i*7] <- round(MuMIn::r.squaredGLMM(basicStats1[[i]])[1], 2)
  tableBasicStats2$R2c[i*7] <- round(MuMIn::r.squaredGLMM(basicStats1[[i]])[2], 2)
}

write.csv(rbind(tableBasicStats1, tableBasicStats2), file='results_tables_figs/basicStats.csv', row.names = F)

# post-hoc tests
model <- lme4::lmer(log(gmes_area*1000) ~ month * T_treatment + (1|fchamber),
                    data = subset(allPaired, midday == 'yes' & PAR >= 800  & A_area > 0 & datetimeFM <= as.Date('2014-02-01')))
emmeans::emmeans(model, pairwise ~ T_treatment | month)

model <- lme4::lmer(gsc_area ~ month * T_treatment + (1|fchamber),
                    data = subset(allPaired, midday == 'yes' & PAR >= 800  & A_area > 0
                                  & datetimeFM <= as.Date('2014-02-01')))
emmeans::emmeans(model, pairwise ~ T_treatment | month)

model <- lme4::lmer(A_area ~ month * T_treatment + (1|fchamber),
                    data = subset(allPaired, midday == 'yes' & PAR >= 800  & A_area > 0
                                  & datetimeFM <= as.Date('2014-02-01')))
emmeans::emmeans(model, pairwise ~ T_treatment | month)

model <- lme4::lmer(iWUE ~ month + (1|fchamber),
                    data = subset(allPaired, midday == 'yes' & PAR >= 800  & A_area > 0
                                  & datetimeFM <= as.Date('2014-02-01')))
emmeans::emmeans(model, pairwise ~ month)

model <- lme4::lmer(d13Cph ~ month + (1|chamber),
                    data = subset(phl, month != 'Feb' & month != 'Mar'))
emmeans::emmeans(model, pairwise ~ month)

model <- lme4::lmer(log(gmes_area*1000) ~ month * T_treatment * Water_treatment + (1|fchamber),
                    data = subset(allPaired, midday == 'yes' & PAR >= 800  & A_area > 0 & Date >= as.Date('2014-02-01')))
emmeans::emmeans(model, pairwise ~ Water_treatment | T_treatment | month)

model <- lme4::lmer(gsc_area ~ month * T_treatment * Water_treatment + (1|fchamber),
                    data = subset(allPaired, midday == 'yes' & PAR >= 800 & Date >= as.Date('2014-02-01')))
emmeans::emmeans(model, pairwise ~ Water_treatment | T_treatment | month)

model <- lme4::lmer(A_area ~ month * T_treatment * Water_treatment + (1|fchamber),
                    data = subset(allPaired, midday == 'yes' & PAR >= 800 & A_area > 0 & Date >= as.Date('2014-02-01')))
emmeans::emmeans(model, pairwise ~ Water_treatment | T_treatment | month)

model <- lme4::lmer(iWUE ~ month * T_treatment * Water_treatment + (1|fchamber),
                    data = subset(allPaired, midday == 'yes' & PAR >= 800 & A_area > 0 & Date >= as.Date('2014-02-01')))
emmeans::emmeans(model, pairwise ~ Water_treatment | T_treatment | month)

anova(nlme::lme(d13Cph ~ month * T_treatment * W_treatment, random = ~1 | as.factor(chamber),
          data = subset(phl,  month == 'Feb' | month == 'Mar'), na.action = na.omit))
anova(nlme::lme(iWUEph_uncorrMD ~ month * T_treatment * W_treatment, random = ~1 | as.factor(chamber),
                data = subset(phl,  month == 'Feb' | month == 'Mar'), na.action = na.omit))
anova(nlme::lme(iWUEph_corrMD ~ month * T_treatment * W_treatment, random = ~1 | as.factor(chamber),
                data = subset(phl,  month == 'Feb' | month == 'Mar'), na.action = na.omit))
