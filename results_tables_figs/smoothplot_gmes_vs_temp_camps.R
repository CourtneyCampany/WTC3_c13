source('scripts/canopy_gmes_more2.R')
library(mgcv)
source('scripts/derivSimulCI.R')
source('scripts/plotCIdate.R')
source('scripts/smoothplot.R')

allPairedMD <- as.data.frame(subset(allPaired, midday=='yes' & PAR >= 800))
myMon <- c('Oct','Dec','Jan','Feb','Mar','Apr')

windows(10,7)
par(mfrow=c(2,3), mar=c(0,5.5,5.5,0), las=1, cex=0.999)
smoothplot(Tair_al, log(gmes_area*1000), T_treatment, data=subset(allPairedMD, month==myMon[1]),
           kgam=4, R='chamber', cex.lab=1.25,
           ylab=expression(Ln~(italic(g)[mes]~mmol~m^-2~s^-1)),
           xlab=' ', axes=F,
           ylim=c(1.5, 7.2), xlim=c(18, 40))
axis(2, at=seq(2, 7, 1), labels=seq(2, 7, 1))
axis(1, at=seq(20, 40, 5), labels=NA)
box()
legend('topright', legend=c('Amb','Warm'), col=c('blue','red'), bty='n', pch=19, cex=1.3)
legend('bottomleft', legend=myMon[1], bty='n', text.font = 2, cex=1.4)
par(mar=c(0,2.25,5.5,2.25))
smoothplot(Tair_al, log(gmes_area*1000), T_treatment, data=subset(allPairedMD, month==myMon[2]),
           kgam=4, R='chamber', cex.lab=1.25,
           ylab=' ',
           xlab=' ', axes=F,
           ylim=c(1.5, 7.2), xlim=c(18, 40))
axis(2, at=seq(2, 7, 1), labels=seq(2, 7, 1))
axis(1, at=seq(20, 40, 5), labels=NA)
box()
legend('bottomleft', legend=myMon[2], bty='n', text.font = 2, cex=1.5)
par(mar=c(0,0,5.5,5.5))
smoothplot(Tair_al, log(gmes_area*1000), T_treatment, data=subset(allPairedMD, month==myMon[3]),
           kgam=4, R='chamber', cex.lab=1.25,
           ylab=' ',
           xlab=' ', axes = F,
           ylim=c(1.5, 7.2), xlim=c(18, 40))
axis(2, at=seq(2, 7, 1), labels=seq(2, 7, 1))
axis(1, at=seq(20, 40, 5), labels=NA)
box()
legend('bottomleft', legend=myMon[3], bty='n', text.font = 2, cex=1.5)
par(mar=c(5.5,5.5,0,0))
smoothplot(Tair_al, log(gmes_area*1000), T_treatment, data=subset(allPairedMD, month==myMon[4]),
           kgam=4,  cex.lab=1.25,
           ylab=expression(Ln~(italic(g)[mes]~mmol~m^-2~s^-1)),
           xlab=' ', axes = F,
           ylim=c(1.5, 7.2), xlim=c(18, 40))
axis(2, at=seq(2, 7, 1), labels=seq(2, 7, 1))
axis(1, at=seq(20, 40, 5), labels=seq(20, 40, 5))
box()
legend('bottomleft', legend=myMon[4], bty='n', text.font = 2, cex=1.5)
par(mar=c(5.5,2.25,0,2.25))
smoothplot(Tair_al, log(gmes_area*1000), T_treatment, data=subset(allPairedMD, month==myMon[5] & Water_treatment=='control'),
           kgam=4, R='chamber', cex.lab=1.25,
           ylab=' ',
           xlab=expression(italic(T)[air]~(degree*C)), axes = F,
           ylim=c(1.5, 7.2), xlim=c(18, 40))
axis(2, at=seq(2, 7, 1), labels=seq(2, 7, 1))
axis(1, at=seq(20, 40, 5), labels=seq(20, 40, 5))
box()
legend('bottomleft', legend=myMon[5], bty='n', text.font = 2, cex=1.5)
par(mar=c(5.5,0,0,5.5))
smoothplot(Tair_al, log(gmes_area*1000), T_treatment, data=subset(allPairedMD, month==myMon[6] & Water_treatment=='control'),
           kgam=4, R='chamber', cex.lab=1.25,
           ylab=' ',
           xlab=' ', axes = F,
           ylim=c(1.5, 7.2), xlim=c(18, 40))
axis(2, at=seq(2, 7, 1), labels=seq(2, 7, 1))
axis(1, at=seq(20, 40, 5), labels=seq(20, 40, 5))
box()
legend('bottomleft', legend=myMon[6], bty='n', text.font = 2, cex=1.5)