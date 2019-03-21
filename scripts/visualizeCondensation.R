source('scripts/calculateCin.R')
WTCrawShort$waterP_kPa_in <- calcWaterP(RH=WTCrawShort$RHref_al, temp=WTCrawShort$Taref_al)
WTCrawShort$condAlert1 <- ifelse(WTCrawShort$RH_al < WTCrawShort$RHref_al, 'yes', 'no')
WTCrawShort$condAlert2 <- ifelse(WTCrawShort$H2OmyFlux <= 0, 'yes', 'no')
WTCrawShort$condAlert3 <- ifelse(WTCrawShort$waterP_kPa_out <= WTCrawShort$waterP_kPa_in, 'yes', 'no')
# condAlert2 and condAlert3 are reduntant
WTCrawShort$condAlert4 <- ifelse(WTCrawShort$dewPointInsideChamb >= WTCrawShort$Taref_al, 'yes', 'no')
WTCrawShort$condAlert5 <- ifelse(WTCrawShort$dewPointInsideChamb >= (WTCrawShort$Taref_al-1.5) &
                                   WTCrawShort$dewPointInsideChamb < WTCrawShort$Taref_al, 'xes', WTCrawShort$condAlert4)

windows(16,8)
par(mfrow=c(2,3))
plot(WTCrawShort$H2Oin*1000*22.4/WTCrawShort$Air_in, pch=19,
     cex=0.8, ylim=c(-5, 31), ylab='H2O conc (mmol/mol)', xlab='', col='magenta')
points(WTCrawShort$H2Oout*1000*22.4/WTCrawShort$Air_out, pch=19, cex=0.8, col='blue')
points(WTCrawShort$H2OmyFlux*1000, pch=19, cex=0.8, col=as.factor(WTCrawShort$condAlert))
abline(0,0)
legend('topleft', legend=c('in','out','good','bad'), pch=19, col=c('magenta','blue','black','red'), bty='n', cex=1.1)
legend('bottomleft', legend='24% out', bty='n')

plot(WTCrawShort$RH_al, pch=19, cex=0.8, col='blue', ylim=c(-15,100),
     ylab='RH (%)', xlab='')
points(WTCrawShort$RHref_al, pch=19, col='magenta')
points((WTCrawShort$RH_al-WTCrawShort$RHref_al), pch=19, col=as.factor(WTCrawShort$condAlert))
abline(0,0)
legend('bottomleft', legend='53% out', bty='n')

plot(WTCrawShort$dewPointInsideChamb, pch=19, col='grey',
     ylab='Temp (c)', xlab='', cex=0.8, ylim=c(-22, 49))
points(WTCrawShort$Tair_al, pch=19, cex=0.8, col='magenta')
points(WTCrawShort$Taref_al, pch=19, cex=0.8, col='blue')
points((WTCrawShort$Taref_al-WTCrawShort$dewPointInsideChamb),
       pch=19, cex=0.8, col=as.factor(WTCrawShort$condAlert))
legend('topright', legend=c('good','bad'), pch=19, bty='n',
       col=c('black','red'), cex=1.1)
legend('topleft', legend=c('Dew point','in','out'), pch=19, bty='n',
       col=c('grey','blue','magenta'), cex=1.1)
legend('bottomright', legend=('32% out'), bty = 'n')
abline(0,0)
abline(1.5, 0, lty=2)

k <- subset(WTCrawShort, chamber=='C09' & datetimeFM>=as.Date('2014-03-10') & datetimeFM<=as.Date('2014-03-31'))

plot((k$H2Oin*1000*22.4/k$Air_in)~k$datetimeFM, pch=19,
     cex=0.8, ylim=c(-5, 31), ylab='H2O conc (mmol/mol)', xlab='', col='magenta')
points((k$H2Oout*1000*22.4/k$Air_out)~k$datetimeFM, pch=19, cex=0.8, col='blue')
points((k$H2OmyFlux*1000)~k$datetimeFM, pch=19, cex=0.8, col=as.factor(k$condAlert))
abline(0,0)

plot(k$RH_al~k$datetimeFM, pch=19, cex=0.8, col='blue', ylim=c(-15,100),
     ylab='RH (%)', xlab='', main='Chamber 9 March 2014')
points(k$RHref_al~k$datetimeFM, pch=19, col='magenta')
points((k$RH_al-k$RHref_al)~k$datetimeFM, pch=19, col=as.factor(k$condAlert))
abline(0,0)

plot(k$dewPointInsideChamb~k$datetimeFM, pch=19, col='grey',
     ylab='Temp (c)', xlab='', cex=0.8, ylim=c(-22, 49))
points(k$Tair_al~k$datetimeFM, pch=19, cex=0.8, col='magenta')
points(k$Taref_al~k$datetimeFM, pch=19, cex=0.8, col='blue')
points((k$Taref_al-k$dewPointInsideChamb)~k$datetimeFM,
       pch=19, cex=0.8, col=as.factor(k$condAlert))
abline(0,0)
abline(1.5, 0, lty=2)
