library(mgcv)
source('scripts/derivSimulCI.R')
source('scripts/plotCIdate.R')
source('scripts/smoothplot.R')

windows(12,8)
par(mfrow=c(2,3), mar=c(5,5,1,1))
myMon <- c('Oct','Dec','Jan','Feb','Mar','Apr')
for (i in 1:length(myMon)){
  smoothplot(Tair_al, log(gmes_area), T_treatment, data=subset(allPaired, month==myMon[i]),
             kgam=5, R='chamber', main=myMon[i], cex.lab=1.75,
             ylab=expression(log(italic(g)[mes]~mol~m^-2~s^-1)),
             xlab=expression(italic(T)[air]~(degree*C)),
             ylim=c(-10, 0), xlim=c(6, 40))
  legend('topleft', legend=c('Amb','Warm'), col=c('blue','red'), bty='n', pch=19)
}


windows(14,8)
par(mfrow=c(1,1), mar=c(5,6,1,1))
smoothplot(Tair_al, gmes_area, T_treatment, data=subset(allPaired, gmes_area < 0.3),
           pointcols=c('white', 'white'), 
           linecols=c("blue",'red'),polycolor=c(alpha("blue",0.2),alpha('red',0.2)),
           kgam=5, R='chamber', cex.lab=1.75,
           ylab=expression(italic(g)[mes]~(mol~m^-2~s^-1)),
           xlab=expression(italic(T)[air]~(degree*C)),
           ylim=c(0, 0.3), xlim=c(8, 40))
myMon <- c('Oct','Dec','Jan','Feb','Mar','Apr')
myChar <- c(15, 16, 17, 18, 19, 8)
for (i in 1:length(myMon)){
  points(subset(allPaired, month==myMon[i] & T_treatment=='ambient' & gmes_area < 0.3)[,'gmes_area']~
           subset(allPaired, month==myMon[i] & T_treatment=='ambient' & gmes_area < 0.3)[,'Tair_al'],
         pch=myChar[i], col='blue')
  points(subset(allPaired, month==myMon[i] & T_treatment=='warmed' & gmes_area < 0.3)[,'gmes_area']~
           subset(allPaired, month==myMon[i] & T_treatment=='warmed' & gmes_area < 0.3)[,'Tair_al'],
         pch=myChar[i], col='red')
}
legend('topleft', legend=c('Amb','Warm'), col=c('blue','red'), bty='n', pch=19)
legend('topright', legend=myMon, pch=myChar, bty='n')

windows(12,8)
par(mfrow=c(1,1), mar=c(5,5,1,1))
smoothplot(Tair_al, log(gmes_area), T_treatment, data=subset(allPaired),
           kgam=8, R='chamber', 
           ylab=expression(Log(italic(g)[mes]~mol~m^-2~s^-1)),
           xlab=expression(italic(T)[air]~(degree*C)))
legend('bottomright', legend=c('Amb','Warm'), col=c('blue','red'), bty='n', pch=19)