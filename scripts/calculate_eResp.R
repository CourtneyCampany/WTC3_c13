# this script does not work on its own, only within "canopy_gmes_more2.R"

# calculate d13CRdarl
respSumm <- dplyr::summarise(dplyr::group_by(setDT(subset(allPaired, nightID >= 1 & timeSinceSunset >= 4 &
                                                                        timeSinceSunset <= 8 & deltaSubstrate >= -60
                                                                        & deltaSubstrate <= -10)),
                                            chamber, DateSunset), d13CRdark=mean(deltaSubstrate, na.rm=T))
names(respSumm)[2] <- 'Date'
respSumm$month <- as.factor(lubridate::month(respSumm$Date, label = T))
source('master_scripts/phloem_plotting.R')
eResp <- dplyr::left_join(respSumm, phl, by=c('chamber', 'month'), all=T)
eResp$month <- factor(eResp$month, levels=c('Oct','Dec','Jan','Feb','Mar','Apr'))
# calcuate eresp according to Ghashghaie & Tcherkek 2013 Chapter 8 p. 393
# edark = d13Csubsrate - d13Crespired
# assume that d13Csubstrate is d13Cphloem
eResp$eRespPh <- eResp$d13CRdark -eResp$d13Cph
rm(respSumm)
# eResp$fchamber <- as.factor(eResp$chamber)
 # model <- nlme::lme(eResp ~ month + temp, random = ~1 | fchamber,
 #                    data = eResp, na.action = na.omit)
 # anova(model)
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

# # optional
#  source('scripts/graphing_d13Csubstrate.R')
 