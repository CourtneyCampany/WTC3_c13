source('scripts/canopy_gmes_more2.R')
library(mgcv)
source('scripts/derivSimulCI.R')
source('scripts/plotCIdate.R')
source('scripts/smoothplot.R')

allPairedMD <- as.data.frame(subset(allPaired, midday=='yes' & PAR >= 800))
myMon <- c('Oct','Dec','Jan','Feb','Mar','Apr')

windows(12,8)
par(mfrow=c(2,3), mar=c(0,5.5,5.5,0), las=1, cex=1.1)
smoothplot(Tair_al, log(gmes_area*1000), T_treatment, data=subset(allPairedMD, month==myMon[1]),
           kgam=4, R='chamber', cex.lab=1.25,
           ylab=expression(Lg~(italic(g)[mes]~mmol~m^-2~s^-1)),
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
           ylab=expression(Lg~(italic(g)[mes]~mmol~m^-2~s^-1)),
           xlab=' ', axes = F,
           ylim=c(1.5, 7.2), xlim=c(18, 40))
axis(2, at=seq(2, 7, 1), labels=seq(2, 7, 1))
axis(1, at=seq(20, 40, 5), labels=seq(20, 40, 5))
box()
legend('bottomleft', legend=myMon[4], bty='n', text.font = 2, cex=1.5)
par(mar=c(5.5,2.25,0,2.25))
smoothplot(Tair_al, log(gmes_area*1000), T_treatment, data=subset(allPairedMD, month==myMon[5]),
           kgam=4, R='chamber', cex.lab=1.25,
           ylab=' ',
           xlab=expression(italic(T)[air]~(degree*C)), axes = F,
           ylim=c(1.5, 7.2), xlim=c(18, 40))
axis(2, at=seq(2, 7, 1), labels=seq(2, 7, 1))
axis(1, at=seq(20, 40, 5), labels=seq(20, 40, 5))
box()
legend('bottomleft', legend=myMon[5], bty='n', text.font = 2, cex=1.5)
par(mar=c(5.5,0,0,5.5))
smoothplot(Tair_al, log(gmes_area*1000), T_treatment, data=subset(allPairedMD, month==myMon[6]),
           kgam=4, R='chamber', cex.lab=1.25,
           ylab=' ',
           xlab=' ', axes = F,
           ylim=c(1.5, 7.2), xlim=c(18, 40))
axis(2, at=seq(2, 7, 1), labels=seq(2, 7, 1))
axis(1, at=seq(20, 40, 5), labels=seq(20, 40, 5))
box()
legend('bottomleft', legend=myMon[6], bty='n', text.font = 2, cex=1.5)

# # same thing but not log transformed
# windows(12,8)
# par(mfrow=c(2,3), mar=c(0,5.5,5.5,0), las=1, cex=1.1)
# smoothplot(Tair_al, gmes_area, T_treatment, data=subset(allPairedMD, month==myMon[1]),
#            kgam=4, R='chamber', cex.lab=1.25,
#            ylab=expression(italic(g)[mes]~(mol~m^-2~s^-1)),
#            xlab=' ', axes=F,
#            ylim=c(0, 1.1), xlim=c(18, 40))
# axis(2, at=seq(0, 1.1, 0.2), labels=seq(0, 1.1, 0.2))
# axis(1, at=seq(20, 40, 5), labels=NA)
# box()
# legend('topright', legend=c('Amb','Warm'), col=c('blue','red'), bty='n', pch=19, cex=1.3)
# legend('topleft', legend=myMon[1], bty='n', text.font = 2, cex=1.4)
# par(mar=c(0,2.25,5.5,2.25))
# smoothplot(Tair_al, gmes_area, T_treatment, data=subset(allPairedMD, month==myMon[2]),
#            kgam=4, R='chamber', cex.lab=1.25,
#            ylab=' ',
#            xlab=' ', axes=F,
#            ylim=c(0, 1.1), xlim=c(18, 40))
# axis(2, at=seq(0, 1.1, 0.2), labels=seq(0, 1.1, 0.2))
# axis(1, at=seq(20, 40, 5), labels=NA)
# box()
# legend('topleft', legend=myMon[2], bty='n', text.font = 2, cex=1.5)
# par(mar=c(0,0,5.5,5.5))
# smoothplot(Tair_al, gmes_area, T_treatment, data=subset(allPairedMD, month==myMon[3]),
#            kgam=4, R='chamber', cex.lab=1.25,
#            ylab=' ',
#            xlab=' ', axes = F,
#            ylim=c(0, 1.1), xlim=c(18, 40))
# axis(2, at=seq(0, 1.1, 0.2), labels=seq(0, 1.1, 0.2))
# axis(1, at=seq(20, 40, 5), labels=NA)
# box()
# legend('topleft', legend=myMon[3], bty='n', text.font = 2, cex=1.5)
# par(mar=c(5.5,5.5,0,0))
# smoothplot(Tair_al, gmes_area, T_treatment, data=subset(allPairedMD, month==myMon[4]),
#            kgam=4, R='chamber', cex.lab=1.25,
#            ylab=expression(italic(g)[mes]~(mol~m^-2~s^-1)),
#            xlab=' ', axes = F,
#            ylim=c(0, 1.1), xlim=c(18, 40))
# axis(2, at=seq(0, 1.1, 0.2), labels=seq(0, 1.1, 0.2))
# axis(1, at=seq(20, 40, 5), labels=seq(20, 40, 5))
# box()
# legend('topleft', legend=myMon[4], bty='n', text.font = 2, cex=1.5)
# par(mar=c(5.5,2.25,0,2.25))
# smoothplot(Tair_al, gmes_area, T_treatment, data=subset(allPairedMD, month==myMon[5]),
#            kgam=4, R='chamber', cex.lab=1.25,
#            ylab=' ',
#            xlab=expression(italic(T)[air]~(degree*C)), axes = F,
#            ylim=c(0, 1.1), xlim=c(18, 40))
# axis(2, at=seq(0, 1.1, 0.2), labels=seq(0, 1.1, 0.2))
# axis(1, at=seq(20, 40, 5), labels=seq(20, 40, 5))
# box()
# legend('topleft', legend=myMon[5], bty='n', text.font = 2, cex=1.5)
# par(mar=c(5.5,0,0,5.5))
# smoothplot(Tair_al, gmes_area, T_treatment, data=subset(allPairedMD, month==myMon[6]),
#            kgam=4, R='chamber', cex.lab=1.25,
#            ylab=' ',
#            xlab=' ', axes = F,
#            ylim=c(0, 1.1), xlim=c(18, 40))
# axis(2, at=seq(0, 1.1, 0.2), labels=seq(0, 1.1, 0.2))
# axis(1, at=seq(20, 40, 5), labels=seq(20, 40, 5))
# box()
# legend('topleft', legend=myMon[6], bty='n', text.font = 2, cex=1.5)
# 
# allPairedMD <- as.data.frame(subset(allPaired, midday=='yes' & PAR >= 800 & Water_treatment == 'control'))

# all in one plot
myMon <- c('Oct','Dec','Jan','Feb','Mar','Apr')
myChar <- c(21:25, 8)
windows(16,8)
par(mfrow=c(1,1), mar=c(6,6,1,1), cex=1.8, las=1)
plot(log(subset(allPairedMD, month==myMon[1] & T_treatment=='ambient')[,'gmes_area']*1000)~
       subset(allPairedMD, month==myMon[1] & T_treatment=='ambient')[,'Tair_al'], pch=myChar[1],
     col='white', bg='white', ylab=expression(Lg~(italic(g)[mes]~mmol~m^-2~s^-1)),
     xlab=expression(italic(T)[air]~(degree*C)),
     ylim=c(1.5,7.2), xlim=c(18, 40))
for (i in 1:length(myMon)){
  points(log(subset(allPairedMD, month==myMon[i] & T_treatment=='ambient')[,'gmes_area']*1000)~
           subset(allPairedMD, month==myMon[i] & T_treatment=='ambient')[,'Tair_al'],
         pch=myChar[i], col=alpha('blue',0.25), bg=alpha('blue', 0.5))
  points(log(subset(allPairedMD, month==myMon[i] & T_treatment=='warmed')[,'gmes_area']*1000)~
           subset(allPairedMD, month==myMon[i] & T_treatment=='warmed')[,'Tair_al'],
         pch=myChar[i], col=alpha('red',0.25), bg=alpha('red', 0.5))
}
smoothplot(Tair_al, log(gmes_area*1000), T_treatment, data=allPairedMD,
           pointcols=c(NA, NA),
           linecols=c("blue",'red'),polycolor=c(alpha("blue",0.2),alpha('red',0.2)),
           kgam=8, R='chamber',  add=T)
legend('topright', legend=c('Amb','Warm',myMon), pch=c(19, 19, myChar),
       col=c('blue','red', rep('black', length(myMon))), bty='n', cex=1.1)


# same thing but not log transformed
myMon <- c('Oct','Dec','Jan','Feb','Mar','Apr')
myChar <- c(15, 16, 17, 18, 19, 8)
windows(16,8)
par(mfrow=c(1,1), mar=c(6,6,1,1), cex=1.8, las=1)
plot(subset(allPairedMD, month==myMon[1] & T_treatment=='ambient')[,'gmes_area']~
       subset(allPairedMD, month==myMon[1] & T_treatment=='ambient')[,'Tair_al'],
     pch=myChar[1], col=alpha('blue',0.01), ylab=expression(italic(g)[mes]~(mol~m^-2~s^-1)),
     xlab=expression(italic(T)[air]~(degree*C)),
     ylim=c(0,1.1), xlim=c(18, 40))
for (i in 1:length(myMon)){
  points(subset(allPairedMD, month==myMon[i] & T_treatment=='ambient')[,'gmes_area']~
           subset(allPairedMD, month==myMon[i] & T_treatment=='ambient')[,'Tair_al'],
         pch=myChar[i], col=alpha('blue',0.5))
  points(subset(allPairedMD, month==myMon[i] & T_treatment=='warmed')[,'gmes_area']~
           subset(allPairedMD, month==myMon[i] & T_treatment=='warmed')[,'Tair_al'],
         pch=myChar[i], col=alpha('red',0.5))
}
smoothplot(Tair_al, gmes_area, T_treatment, data=allPairedMD,
           pointcols=c(NA, NA),
           linecols=c("blue",'red'),polycolor=c(alpha("blue",0.2),alpha('red',0.2)),
           kgam=8, R='chamber',  add=T)

legend('topleft', legend=c('Amb','Warm'), col=c('blue','red'), bty='n', pch=19)
legend('topright', legend=myMon, pch=myChar, bty='n')
