source('scripts/calculateCin.R')

windows(12,8)
par(mfrow=c(2,2))
plot(WTCrawShort$RHref_al, pch=19, cex=0.8, ylim=c(-20,110), ylab='RH (%)', xlab='', col='magenta')
points(WTCrawShort$RH_al, pch=19, cex=0.8, col='blue')
points((WTCrawShort$RH_al-WTCrawShort$RHref_al), pch=19, cex=0.8, col=as.factor(WTCrawShort$condAlert))
abline(0,0)
legend('topleft', legend=c('in','out'), pch=19, col=c('magenta','blue'), bty='n', cex=1.1)

plot(WTCrawShort$H2Oin*1000, pch=19, cex=0.8, ylab='H2O flux (mmol/s)', ylim=c(-1,10), xlab='', col='magenta')
points(WTCrawShort$H2Oout*1000, pch=19, col='blue')
points((WTCrawShort$H2Oout*1000-WTCrawShort$H2Oin*1000), pch=19, col=as.factor(WTCrawShort$condAlert), cex=0.8)
abline(0,0)
legend('topleft', legend=c('in','out'), pch=19, col=c('magenta','blue'), bty='n', cex=1.1)

plot(WTCrawShort$H2Oin_conc*1000, ylim=c(-5,75), ylab='H2O conc (mmol/mol)', xlab='', cex=0.8, col='magenta')
points(WTCrawShort$satVap*1000, pch=19, col='grey', cex=0.8)
points(WTCrawShort$satVap*1000-WTCrawShort$H2Oin_conc*1000, pch=19, col=as.factor(WTCrawShort$condAlert), cex=0.8)
abline(0,0)
legend('topleft', legend=c('H2O sat','H2O conc in'), pch=19, col=c('grey','magenta'), bty='n', cex=1.1)

plot(WTCrawShort$FluxH2O/(WTCrawShort$VPDair/101.3), pch=19, col=as.factor(WTCrawShort$condAlert), cex=0.8,
     ylab='Cond (mmol/s/tree)', xlab='')
legend('topleft', legend=c('good','bad'), pch=19, col=c('black','red'), bty='n')
