source('scripts/calculateCin.R')

windows(12,8)
par(mfrow=c(2,2))
plot(WTCrawShort$H2Oin*1000*22.4/WTCrawShort$Air_in, pch=19,
     cex=0.8, ylim=c(-5, 31), ylab='H2O conc (mmol/mol)', xlab='', col='magenta')
points(WTCrawShort$H2Oout*1000*22.4/WTCrawShort$Air_out, pch=19, cex=0.8, col='blue')
points(WTCrawShort$H2OmyFlux*1000, pch=19, cex=0.8, col=as.factor(WTCrawShort$condAlert2))
abline(0,0)
legend('topleft', legend=c('in','out'), pch=19, col=c('magenta','blue'), bty='n', cex=1.1)
legend('bottomleft', legend='24% out', bty='n')

plot(WTCrawShort$RH_al, pch=19, cex=0.8, col='blue', ylim=c(-5,100),
     ylab='RH (%)', xlab='')
points(WTCrawShort$RHref_al, pch=10, col='magenta')
points((WTCrawShort$RH_al-WTCrawShort$RHref_al), pch=19, col=as.factor(WTCrawShort$condAlert1))
abline(0,0)

plot(WTCrawShort$dewPointInsideChamb, pch=19, col='grey',
     ylab='Temp (c)', xlab='', cex=0.8, ylim=c(-15, 45))
points(WTCrawShort$Taref_al, pch=19, cex=0.8, col='blue')
points(WTCrawShort$Tair_al, pch=19, cex=0.8, col='magenta')
points((WTCrawShort$dewPointInsideChamb-WTCrawShort$Taref_al+1.5),
       pch=19, cex=0.8, col=as.factor(WTCrawShort$condAlert))
legend('topleft', legend=c('good','bad'), pch=19, col=c(1:2), bty='n', cex=1.1)
abline(0,0)
abline(1, 0, lty=2)
abline(1.5,0, lty=3)