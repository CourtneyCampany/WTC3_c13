source('scripts/basicFunTEG.R')
leafGX <- read.csv('data/WTC_TEMP_CM_TDLSUNSHADE-GX_20131026-20140421.csv')
leafGX$Cc <- leafGX$Ci - leafGX$Photo/leafGX$gm_bar
leafGX[which(leafGX$Cc < 0), 'Cc'] <- NA
leafGX$Ci.Cc <- leafGX$Ci - leafGX$Cc
leafGX[which(leafGX$Ci.Cc < 0), 'Ci.Cc'] <- NA
leafGX$iWUE <- leafGX$Photo/leafGX$Cond
leafGmes <- doBy::summaryBy(Photo + Cond + iWUE + Ci.Cc + gm_bar~id, data=leafGX, FUN=mean.na)
names(leafGmes)[2:6] <- c('Photo', 'Cond', 'iWUE', 'Ci.Cc', 'gm')
leafGmes <- merge(leafGmes, rmDup(leafGX[,c('chamber','Month','leaf','light','leaflight','id')], 'id'), by='id')
names(leafGmes)[8] <- 'month'
leafGmes$chamber <- paste0('C', substr(as.character(leafGmes$chamber), 3, 4))
leafGmesAvg1 <- dplyr::summarise(dplyr::group_by(subset(leafGmes, leaflight!='shade-high'),
                                                 chamber, month),
                                 Photo_Avg1=mean(Photo), Cond_Avg1=mean(Cond),
                                 iWUE_Avg1=mean(iWUE), Ci.Cc_Avg1=mean(Ci.Cc),
                                 gm_Avg1=mean(gm, na.rm = T))
leafGmesAvg2 <- dplyr::summarise(dplyr::group_by(leafGmes, chamber, month),
                                 Photo_Avg2=mean(Photo), Cond_Avg2=mean(Cond),
                                 iWUE_Avg2=mean(iWUE), Ci.Cc_Avg2=mean(Ci.Cc),
                                 gm_Avg2=mean(gm))
sun <- subset(leafGmes, leaflight=='sun-high')[,c('Photo', 'Cond', 'iWUE', 'Ci.Cc', 'gm','chamber','month')]
names(sun)[1:5] <- paste0(names(sun[1:5]), '_sun')
shadeL <- subset(leafGmes, leaflight=='shade-low')[,c('Photo', 'Cond', 'iWUE', 'Ci.Cc','gm', 'chamber','month')]
names(shadeL)[1:5] <- paste0(names(shadeL[1:5]), '_shL')
shadeH <- subset(leafGmes, leaflight=='shade-high')[,c('Photo', 'Cond', 'iWUE', 'Ci.Cc', 'gm','chamber','month')]
names(shadeH)[1:5] <- paste0(names(shadeH[1:5]), '_shH')
leafGmes <- merge(merge(merge(merge(leafGmesAvg1, leafGmesAvg2, by=c('chamber','month'), all=T),
                  sun, by=c('chamber','month'), all=T), shadeH, by=c('chamber','month'), all=T), 
                  shadeL, by=c('chamber','month'), all=T)
# source('scripts/canopy_gmes_more2.R')
# canopyGmes <- doBy::summaryBy(gmes_area ~ month + chamber, FUN=mean,
#                               data=subset(allPaired[which(!is.na(allPaired$gmes_area)),], midday=='yes'
#                                           & PAR >= 800 & A_area > 0 & E_area > 0))
# names(canopyGmes)[3] <- c('canopy_gmes')
# gmesAll <- merge(canopyGmes, leafGmes, by=c('month','chamber'), all=T)
# chambs <- data.frame(row.names=1:12)
# chambs$chamber <- c(paste0('C0', 1:9), paste0('C', 10:12))
# chambs$T_treatment <- rep(c('ambient', 'warmed'), 6)
# gmesAll <- merge(gmesAll, chambs, by='chamber', all=T)
# summary(lm(log(canopy_gmes*1000) ~ log(leaf_gmes_sun*1000), data = gmesAll))
# summary(lm(log(canopy_gmes*1000) ~ log(leaf_gmes_shL*1000), data = gmesAll))
# summary(lm(log(canopy_gmes*1000) ~ log(leaf_gmes_shH*1000), data = gmesAll))
# summary(lm(log(canopy_gmes*1000) ~ log(leaf_gmesAvg1*1000), data = gmesAll))
# summary(lm(log(canopy_gmes*1000) ~ log(leaf_gmesAvg2*1000), data = gmesAll))
# gmesAll$lgCanopy <- log(gmesAll$canopy_gmes*1000)
# gmesAll$lgSun <- log(gmesAll$leaf_gmes_sun*1000)
# gmesAll$lgShH <- log(gmesAll$leaf_gmes_shH*1000)
# gmesAll$lgShL <- log(gmesAll$leaf_gmes_shL*1000)
# gmesAll$lgAvg1 <- log(gmesAll$leaf_gmesAvg1*1000)
# gmesAll$lgAvg2 <- log(gmesAll$leaf_gmesAvg2*1000)
# write.csv(gmesAll, file='output/gmesAll.csv', row.names=F)
# gmesSumm <- doBy::summaryBy(canopy_gmes + leaf_gmes_sun + leaf_gmes_shH + leaf_gmes_shL +
#                               leaf_gmesAvg1 + leaf_gmesAvg2 ~ month + T_treatment, data=gmesAll, FUN=c(mean.na, s.err.na))
# gmesSumm2 <- doBy::summaryBy(canopy_gmes.mean.na + leaf_gmes_sun.mean.na + leaf_gmes_shH.mean.na +
#                                leaf_gmes_shL.mean.na + leaf_gmesAvg1.mean.na + leaf_gmesAvg2.mean.na
#                              ~ T_treatment, data=gmesSumm, FUN=c(mean.na, s.err.na))
# summary(lm(canopy_gmes.mean.na ~ leaf_gmesAvg2.mean.na, data=gmesSumm))
rm(sun, shadeH, shadeL, leafGmesAvg1, leafGmesAvg2, leafGX)
