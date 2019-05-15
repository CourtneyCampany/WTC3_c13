source('scripts/canopy_gmes_more2.R')
# calculate d13CAnet
photoSumm <- dplyr::summarise(dplyr::group_by(setDT(subset(allPaired, midday=='yes' & A_area > 0
                                                           & deltaSubstrate >= -60 & deltaSubstrate <= -10)),
                                                            chamber, Date), d13CAnet=mean(deltaSubstrate, na.rm=T),
                                            d13CAnetSE=s.err(deltaSubstrate))
respSumm <- dplyr::summarise(dplyr::group_by(setDT(subset(allPaired, nightID >= 1 & timeSinceSunset >= 4 &
                                                                        timeSinceSunset <= 8 & deltaSubstrate >= -60
                                                                        & deltaSubstrate <= -10)), chamber, DateSunset),
                             d13CRdark=mean(deltaSubstrate, na.rm=T), d13CRdarkSE=s.err(deltaSubstrate),
                             DELTARdark=mean(DELTA, na.rm=T), d13Cchamb=mean(Corrdel13C_Avg, na.rm=T),
                             d13Camb=mean(Corrdel13C_Avg_ref, na.rm=T))
names(respSumm)[2] <- 'Date'
respSummEarly <- dplyr::summarise(dplyr::group_by(setDT(subset(allPaired, nightID >= 1 & timeSinceSunset <= 2)),
                                                  chamber, Date),
                                                d13CRdarkEarly=mean(deltaSubstrate, na.rm=T),
                                                d13CRdarkEarlySE=s.err(deltaSubstrate),
                                  DELTARdarkEarly=mean(DELTA, na.rm=T), d13CchambEarly=mean(Corrdel13C_Avg, na.rm=T),
                                  d13CambEarly=mean(Corrdel13C_Avg_ref, na.rm=T))

eResp <- dplyr::left_join(photoSumm, respSumm, by=c('chamber','Date'))
eResp <- as.data.frame(dplyr::left_join(eResp, respSummEarly, by=c('chamber','Date')))
eResp$month <- as.factor(lubridate::month(eResp$Date, label=T))
eResp$month <- factor(eResp$month, levels=c('Oct','Dec','Jan','Feb','Mar','Apr'))
source('master_scripts/phloem_plotting.R')
eResp <- merge(eResp, phl, by=c('chamber', 'month'), all=T)
eResp$eResp <- (eResp$d13CAnet - eResp$d13CRdark)/(1+eResp$d13CRdark)
eResp$eRespEarly <- (eResp$d13CAnet - eResp$d13CRdarkEarly)/(1+eResp$d13CRdarkEarly)
eResp$eRespPh <- (eResp$d13Cph - eResp$d13CRdark)/(1+eResp$d13CRdark)
eResp$eRespPhEarly <- (eResp$d13Cph - eResp$d13CRdarkEarly)/(1+eResp$d13CRdarkEarly)
eResp$eRespWingate <- eResp$DELTARdark + eResp$d13CAnet - eResp$d13Camb
eResp$eRespWingatePh <- eResp$DELTARdark + eResp$d13Cph - eResp$d13Camb
eResp$eRespWingateEarly <- eResp$DELTARdarkEarly + eResp$d13CAnet - eResp$d13CambEarly
eResp$eRespWingateEarlyPh <- eResp$DELTARdarkEarly + eResp$d13Cph - eResp$d13CambEarly
eResp$fchamber <- as.factor(eResp$chamber)



model <- nlme::lme(eResp ~ month + T_treatment, random = ~1 | fchamber,
                   data = eResp, na.action = na.omit)
anova(model)
windows(12,8)


boxplot(eResp$eResp~eResp$month, ylab=expression(italic(e)[Rdark]~('\211')), xlab=' ', cex.lab=1.4)

model <- nlme::lme(eRespEarly ~ month + T_treatment, random = ~1 | fchamber,
                   data = eResp, na.action = na.omit)
anova(model)
boxplot(eResp$eRespEarly~eResp$month, ylab=expression(Early~italic(e)[Rdark]~('\211')), xlab=' ', cex.lab=1.4)

windows(16,10)
par(mfrow=c(1,3))
par(mar=c(2,5,1,1))
plot(photoSumm$d13CAnet~photoSumm$Date, pch=19, col=as.factor(photoSumm$chamber), ylab=expression(delta^13*C[Anet]),
     xlab='', cex.lab=1.4, cex=1.2, ylim=c(-60, -15))
plot(respSumm$d13CRdark~respSumm$Date, pch=19, col=as.factor(respSumm$chamber), ylab=expression(delta^13*C[Rdark]),
     xlab='', cex.lab=1.4, cex=1.2, ylim=c(-60, -15))
plot(respSumm2$d13CRdark~respSumm2$Date, pch=19, col=as.factor(respSumm2$chamber), ylab=expression(Early~delta^13*C[Rdark]),
     xlab='', cex.lab=1.4, cex=1.2, ylim=c(-60, -15))
# optional
# source('scripts/graphing_d13Csubstrate.R')