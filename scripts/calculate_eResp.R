source('scripts/calculateCin.R')
allPaired <- deltaPaired
allPaired$month <- as.factor(lubridate::month(allPaired$datetimeFM, label=T))
allPaired$month <- factor(allPaired$month, levels=c('Oct','Dec','Jan','Feb','Mar','Apr'))
allPaired$midday <- ifelse(allPaired$Time >= 10.30 & allPaired$Time <= 13.30, 'yes', 'no')
allPaired$Date <- as.Date(allPaired$datetimeFM)
# get leaf area for each chamber and date
source('scripts/leafArea.R')
allPaired <- merge(allPaired, treeLeaf, by=c('chamber','Date'), all.x=T, all.y=F)
allPaired$A_area <- allPaired$FluxCO2*1000/allPaired$leafArea
allPaired[which(allPaired$condAlert=='yes'), c('A_area')] <- NA
# calculate d13CAnet
photoSumm <- dplyr::summarise(dplyr::group_by(setDT(subset(allPaired, midday=='yes' & A_area > 0
                                                           & deltaSubstrate >= -60 & deltaSubstrate <= -10)),
                                                            chamber, Date), d13CAnet=mean.na(deltaSubstrate),
                                            d13CAnetSE=s.err.na(deltaSubstrate))
photoSumm[which(photoSumm$d13CAnetSE >= 0.7), 'd13CAnet'] <- NA
respSumm <- dplyr::summarise(dplyr::group_by(setDT(subset(allPaired, nightID >= 1 & timeSinceSunset >= 4 &
                                                                        timeSinceSunset <= 8 & deltaSubstrate >= -60
                                                                        & deltaSubstrate <= -10)),
                                             chamber, DateSunset), d13CRdark=mean(deltaSubstrate, na.rm=T))
names(respSumm)[2] <- 'Date'
eResp <- dplyr::left_join(photoSumm, respSumm, by=c('chamber','Date'))
eResp$month <- as.factor(lubridate::month(eResp$Date, label=T))
source('master_scripts/phloem_plotting.R')
eResp <- dplyr::left_join(eResp, phl, by=c('chamber', 'month'), all=T)
eResp$month <- factor(eResp$month, levels=c('Oct','Dec','Jan','Feb','Mar','Apr'))
# calcuate eresp according to Ghashghaie & Tcherkek 2013 Chapter 8 p. 393
# edark = d13Csubsrate - d13Crespired
# assume that d13Csubstrate is d13Cphloem
# alternatively d13Csubstrate is d13CAnet
eResp$eResp <- eResp$d13CRdark - eResp$d13CAnet
eResp$eRespPh <- eResp$d13CRdark -eResp$d13Cph
rm(photoSumm, respSumm)
# eResp$fchamber <- as.factor(eResp$chamber)
#  model <- nlme::lme(eResp ~ month + temp, random = ~1 | fchamber,
#                     data = eResp, na.action = na.omit)
#  anova(model)
#  windows(12,8)
# 
 # windows(16,8)
 # par(mfrow=c(2,3), mar=c(3,5,1,0.5))
 # boxplot(eResp$d13Cph~eResp$month, ylab=expression(delta^13*C[phloem]~('\211')), xlab=' ', cex.lab=1.5, ylim=c(-35, -18))
 # boxplot(eResp$d13CAnet~eResp$month, ylab=expression(delta^13*C[Anet]~('\211')), xlab=' ', cex.lab=1.5, ylim=c(-35, -18))
 # boxplot(eResp$d13CRdark~eResp$month, ylab=expression(delta^13*C[Rdark]~('\211')), xlab=' ', cex.lab=1.5, ylim=c(-35, -18))
 # boxplot(eResp$eRespPh~eResp$month, ylab=expression(italic(e)[Rdark-ph]~('\211')), xlab=' ', cex.lab=1.5)
 # abline(mean(eResp$eRespPh, na.rm=T), 0, col='red')
 # boxplot(eResp$eResp~eResp$month, ylab=expression(italic(e)[Rdark]~('\211')), xlab=' ', cex.lab=1.5)
 # abline(mean(eResp$eResp, na.rm=T), 0, col='red')
# 
# # optional
#  source('scripts/graphing_d13Csubstrate.R')
 