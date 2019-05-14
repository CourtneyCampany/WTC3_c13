source('scripts/canopy_gmes_more2.R')
# calculate d13CAnet
photoSumm <- as.data.frame(dplyr::summarise(dplyr::group_by(setDT(subset(allPaired, midday=='yes' & A_area > 0
                                                                         & deltaSubstrate >= -60& deltaSubstrate <= -10)),
                                                            chamber, Date), d13CAnet=mean(deltaSubstrate, na.rm=T),
                                            d13CAnetSE=s.err(deltaSubstrate), d13CAnetN=lengthWithoutNA(deltaSubstrate)))
respSumm <- as.data.frame(dplyr::summarise(dplyr::group_by(setDT(subset(allPaired, nightID >= 1 & timeSinceSunset >= 4
                                                                        & timeSinceSunset <= 8 & deltaSubstrate >= -60
                                                                        & deltaSubstrate <= -10)),
                                                           chamber, DateSunset), d13CRdark=mean(deltaSubstrate, na.rm=T),
                                           d13CRdarkSE=s.err(deltaSubstrate), d13CRdarkN=lengthWithoutNA(deltaSubstrate)))
names(respSumm)[2] <- 'Date'
eResp <- as.data.frame(dplyr::left_join(photoSumm, respSumm, by=c('chamber','Date')))
eResp$eResp <- (eResp$d13CAnet - eResp$d13CRdark)/(1+eResp$d13CRdark)
respSumm2 <- as.data.frame(dplyr::summarise(dplyr::group_by(setDT(subset(allPaired, nightID >= 1 & timeSinceSunset <= 2)),
                                                           chamber, Date), d13CRdark=mean(deltaSubstrate, na.rm=T),
                                           d13CRdarkSE=s.err(deltaSubstrate), d13CRdarkN=lengthWithoutNA(deltaSubstrate)))

windows(11,14)
par(mfrow=c(3,1))
par(mar=c(2,5,1,1))
plot(photoSumm$d13CAnet~photoSumm$Date, pch=19, col=as.factor(photoSumm$chamber), ylab=expression(delta^13*C[Anet]),
     xlab='', cex.lab=1.4, cex=1.2)
plot(respSumm$d13CRdark~respSumm$Date, pch=19, col=as.factor(respSumm$chamber), ylab=expression(delta^13*C[Rdark]),
     xlab='')
plot(respSumm2$d13CRdark~respSumm2$Date, pch=19, col=as.factor(respSumm2$chamber), ylab=expression(Early~delta^13*C[Rdark]),
     xlab='')
# optional
# source('scripts/graphing_d13Csubstrate.R')