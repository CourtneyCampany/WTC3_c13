source('scripts/canopy_gmes_more2.R')
allPaired$iWUE <- ifelse(allPaired$iWUE > 500, NA, allPaired$iWUE)
gmesMDsumm1 <- dplyr::summarise(dplyr::group_by(setDT(subset(allPaired, midday=='yes' & PAR >= 800 & A_area > 0)),
                                                month, chamber), gmesCh = mean.na(gmes_area),
                                gmesChSE=s.err.na(gmes_area), gmesChN=lengthWithoutNA(gmes_area),
                                gscCh = mean.na(gsc_area), gscChSE=s.err.na(gsc_area),
                                ACh = mean.na(A_area), AChSE=s.err.na(A_area),
                                iWUEch = mean.na(iWUE), iWUEse=s.err.na(iWUE))
chambs <- read.csv('data/trtkey2.csv')
gmesMDsumm1 <- dplyr::left_join(gmesMDsumm1, chambs, by=c('chamber','month'))
gmesMDsumm2 <- dplyr::summarise(dplyr::group_by(subset(gmesMDsumm1, gmesChN >= 3), month, T_treatment, W_treatment),
                                gmesT=mean.na(gmesCh), gmesTse=s.err.na(gmesCh),
                                gscT=mean.na(gscCh), gscTse=s.err.na(gscCh),
                                AT=mean.na(ACh), ATse=s.err.na(ACh),
                                iWUEt=mean.na(iWUEch), iWUEtSE=s.err.na(iWUEch))
gmesMDsumm3 <- dplyr::summarise(dplyr::group_by(gmesMDsumm2, T_treatment, W_treatment),
                                gmesTem=mean.na(gmesT), gmesTemSe=s.err.na(gmesT),
                                gscTem=mean.na(gscT), gscTemSE=s.err.na(gscT),
                                Atem=mean.na(AT), AtemSE=s.err.na(AT),
                                iWUEtem=mean.na(iWUEt), iWUEtemSE=s.err.na(iWUEt))
write.csv(gmesMDsumm2, file='output/dataFigure1a.csv', row.names = F)
write.csv(gmesMDsumm3, file='output/dataFigure1b.csv', row.names = F)

gmesL <- list()
chambs <- c(paste0('C0', 1:9), paste0('C', 10:12))
for (i in 1:length(levels(as.factor(allPaired$chamber)))){
  gmesL[[i]] <- subset(allPaired, chamber==chambs[i] & gmes_area < 0.3)
}
lapply(gmesL, function(x) write.csv(x, file=paste0('calculated_data/indv_chambs/', x[1,'chamber'], '.csv'),
                                    row.names = F))
model <- nlme::lme(log(gmes_area*1000) ~ month + T_treatment + month:T_treatment, random = ~1 | fchamber,
                   data = subset(allPaired, midday == 'yes' & PAR >= 800 & A_area > 0 &
                                   Water_treatment == 'control'), na.action = na.omit)
anova(model)

model <- lme4::lmer(log(gmes_area*1000) ~ month * T_treatment + (1|fchamber),
                    data = subset(allPaired, midday == 'yes' & PAR >= 800  & A_area > 0 & Water_treatment == 'control'))
emmeans::emmeans(model, pairwise ~ T_treatment | month)

model <- nlme::lme(gsc_area ~ month + T_treatment + month:T_treatment, random = ~1 | fchamber,
                   data = subset(allPaired, midday == 'yes' & PAR >= 800 & A_area > 0 &
                                   Water_treatment == 'control'), na.action = na.omit)
anova(model)

model <- lme4::lmer(gsc_area ~ month * T_treatment + (1|fchamber),
                    data = subset(allPaired, midday == 'yes' & PAR >= 800  & A_area > 0 & Water_treatment == 'control'))
emmeans::emmeans(model, pairwise ~ T_treatment | month)

model <- nlme::lme(A_area ~ month + T_treatment + month:T_treatment, random = ~1 | fchamber,
                   data = subset(allPaired, midday == 'yes' & PAR >= 800 & A_area > 0 &
                                   Water_treatment == 'control'), na.action = na.omit)
anova(model)

model <- lme4::lmer(A_area ~ month * T_treatment + (1|fchamber),
                    data = subset(allPaired, midday == 'yes' & PAR >= 800  & A_area > 0 & Water_treatment == 'control'))
emmeans::emmeans(model, pairwise ~ T_treatment | month)

model <- nlme::lme(iWUE ~ month + T_treatment + month:T_treatment, random = ~1 | fchamber,
                   data = subset(allPaired, midday == 'yes' & PAR >= 800 & A_area > 0 &
                                   Water_treatment == 'control'), na.action = na.omit)
anova(model)

model <- lme4::lmer(iWUE ~ month * T_treatment + (1|fchamber),
                    data = subset(allPaired, midday == 'yes' & PAR >= 800  & A_area > 0 & Water_treatment == 'control'))
emmeans::emmeans(model, pairwise ~ T_treatment | month)

model <- nlme::lme(log(gmes_area*1000) ~ month * T_treatment * Water_treatment, random = ~1 | fchamber,
                   data = subset(allPaired, midday == 'yes' & PAR >= 800 & A_area > 0 &
                                   Date >= as.Date('2014-02-01')), na.action = na.omit)
anova(model)

model <- lme4::lmer(log(gmes_area*1000) ~ month * T_treatment * Water_treatment + (1|fchamber),
                    data = subset(allPaired, midday == 'yes' & PAR >= 800  & A_area > 0 & Date >= as.Date('2014-02-01')))
emmeans::emmeans(model, pairwise ~ Water_treatment | T_treatment | month)

model <- nlme::lme(gsc_area ~ month * T_treatment * Water_treatment, random = ~1 | fchamber,
                   data = subset(allPaired, midday == 'yes' & PAR >= 800 & Date >= as.Date('2014-02-01')), na.action = na.omit)
anova(model)

model <- lme4::lmer(gsc_area ~ month * T_treatment * Water_treatment + (1|fchamber),
                    data = subset(allPaired, midday == 'yes' & PAR >= 800 & Date >= as.Date('2014-02-01')))
emmeans::emmeans(model, pairwise ~ Water_treatment | T_treatment | month)

model <- nlme::lme(A_area ~ month * T_treatment * Water_treatment, random = ~1 | fchamber,
                   data = subset(allPaired, midday == 'yes' & PAR >= 800 & Date >= as.Date('2014-02-01')), na.action = na.omit)
anova(model)

model <- lme4::lmer(A_area ~ month * T_treatment * Water_treatment + (1|fchamber),
                    data = subset(allPaired, midday == 'yes' & PAR >= 800 & A_area > 0 & Date >= as.Date('2014-02-01')))
emmeans::emmeans(model, pairwise ~ Water_treatment | T_treatment | month)

model <- nlme::lme(iWUE ~ month * T_treatment * Water_treatment, random = ~1 | fchamber,
                   data = subset(allPaired, midday == 'yes' & PAR >= 800 & Date >= as.Date('2014-02-01')), na.action = na.omit)
anova(model)

model <- lme4::lmer(iWUE ~ month * T_treatment * Water_treatment + (1|fchamber),
                    data = subset(allPaired, midday == 'yes' & PAR >= 800 & A_area > 0 & Date >= as.Date('2014-02-01')))
emmeans::emmeans(model, pairwise ~ Water_treatment | T_treatment | month)


