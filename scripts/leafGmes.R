source('scripts/basicFunTEG.R')
leafGX <- read.csv('data/WTC_TEMP_CM_TDLSUNSHADE-GX_20131026-20140421.csv')
leafGmes <- doBy::summaryBy(gm_bar~id, data=leafGX, FUN=c(mean.na, s.err.na))
names(leafGmes)[2:3] <- c('leaf_gmes','leaf_gmesSE')
leafGmes <- merge(leafGmes, rmDup(leafGX[,c('chamber','Month','leaf','light','leaflight','id')], 'id'), by='id')
names(leafGmes)[5] <- 'month'
leafGmes$chamber <- paste0('C', substr(as.character(leafGmes$chamber), 3, 4))
leafGmesAvg1 <- doBy::summaryBy(leaf_gmes ~ chamber + month, FUN=mean, data=subset(leafGmes, leaflight!='shade-high'))
names(leafGmesAvg1)[3] <- 'leaf_gmesAvg1'
leafGmesAvg2 <- doBy::summaryBy(leaf_gmes ~ chamber + month, FUN=mean, data=leafGmes)
names(leafGmesAvg2)[3] <- 'leaf_gmesAvg2'
sun <- subset(leafGmes, leaflight=='sun-high')[,c('leaf_gmes','leaf_gmesSE','chamber','month')]
names(sun)[1:2] <- paste0(names(sun[1:2]), '_sun')
shadeL <- subset(leafGmes, leaflight=='shade-low')[,c('leaf_gmes','leaf_gmesSE','chamber','month')]
names(shadeL)[1:2] <- paste0(names(shadeL[1:2]), '_shL')
shadeH <- subset(leafGmes, leaflight=='shade-high')[,c('leaf_gmes','leaf_gmesSE','chamber','month')]
names(shadeH)[1:2] <- paste0(names(shadeH[1:2]), '_shH')
leafGmes <- merge(merge(merge(merge(leafGmesAvg1, leafGmesAvg2, by=c('chamber','month'), all=T),
                  sun, by=c('chamber','month'), all=T), shadeH, by=c('chamber','month'), all=T), 
                  shadeL, by=c('chamber','month'), all=T)
source('scripts/canopy_gmes_more.R')
canopyGmes <- doBy::summaryBy(gmes_area ~ month + chamber, FUN=c(mean, s.err),
                              data=subset(allPaired[which(!is.na(allPaired$gmes_area)),], midday=='yes'))
names(canopyGmes)[3:4] <- c('canopy_gmes','canopy_gmesSE')
gmesAll <- merge(canopyGmes, leafGmes, by=c('month','chamber'), all=T)
chambs <- data.frame(row.names=1:12)
chambs$chamber <- c(paste0('C0', 1:9), paste0('C', 10:12))
chambs$T_treatment <- rep(c('ambient', 'warmed'), 6)
gmesAll <- merge(gmesAll, chambs, by='chamber', all=T)
summary(lm(log(canopy_gmes*1000) ~ log(leaf_gmes_sun*1000), data = gmesAll))
summary(lm(log(canopy_gmes*1000) ~ log(leaf_gmes_shL*1000), data = gmesAll))
summary(lm(log(canopy_gmes*1000) ~ log(leaf_gmes_shH*1000), data = gmesAll))
summary(lm(log(canopy_gmes*1000) ~ log(leaf_gmesAvg1*1000), data = gmesAll))
summary(lm(log(canopy_gmes*1000) ~ log(leaf_gmesAvg2*1000), data = gmesAll))
gmesAll$lgCanopy <- log(gmesAll$canopy_gmes*1000)
gmesAll$lgSun <- log(gmesAll$leaf_gmes_sun*1000)
gmesAll$lgShH <- log(gmesAll$leaf_gmes_shH*1000)
gmesAll$lgShL <- log(gmesAll$leaf_gmes_shL*1000)
gmesAll$lgAvg1 <- log(gmesAll$leaf_gmesAvg1*1000)
gmesAll$lgAvg2 <- log(gmesAll$leaf_gmesAvg2*1000)
write.csv(gmesAll, file='output/gmesAll.csv', row.names=F)
gmesSumm <- doBy::summaryBy(canopy_gmes + leaf_gmes_sun + leaf_gmes_shH + leaf_gmes_shL +
                              leaf_gmesAvg1 + leaf_gmesAvg2 ~ month + T_treatment, data=gmesAll, FUN=c(mean.na, s.err.na))
gmesSumm2 <- doBy::summaryBy(canopy_gmes.mean.na + leaf_gmes_sun.mean.na + leaf_gmes_shH.mean.na +
                               leaf_gmes_shL.mean.na + leaf_gmesAvg1.mean.na + leaf_gmesAvg2.mean.na
                             ~ T_treatment, data=gmesSumm, FUN=c(mean.na, s.err.na))
summary(lm(canopy_gmes.mean.na ~ leaf_gmesAvg2.mean.na, data=gmesSumm))

