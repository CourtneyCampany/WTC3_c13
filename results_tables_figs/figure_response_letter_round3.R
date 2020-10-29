source('scripts/canopy_gmes_more3.R')
library(mgcv)
source('scripts/derivSimulCI.R')
source('scripts/plotCIdate.R')
source('scripts/smoothplot.R')

allPairedMD <- as.data.frame(subset(allPaired, midday=='yes' & PAR >= 800 & Water_treatment == 'control'))
myMon <- c('Oct','Dec','Jan','Feb','Mar','Apr')

windows(12, 6)
par(mfrow=c(1, 3), mar = c(5, 5, 2, 2), cex = 1.3)
hist(allPairedMD$gmes_area, ylab= 'Frequency', xlab = expression(italic(g)[m]~(mol~m^-2~s^-1)), main = ' ', cex.lab = 1.2)
hist(log(allPairedMD$gmes_area*1000), ylab= ' ', xlab = expression(Log(italic(g)[m]~(mmol~m^-2~s^-1))), main = ' ', cex.lab = 1.2)
hist((1/allPairedMD$gmes_area), ylab= '', xlab = expression((italic(g)[m]~(mol~m^-2~s^-1))^-1), main = ' ', cex.lab = 1.2)

myChar <- c(21:25, 8)
windows(16, 10)
par(mfrow=c(2,1), mar=c(2.5,5,2.5,1), cex=1.2, las=1)
plot(log(subset(allPairedMD, month == myMon[1] & T_treatment == 'ambient')[,'gmes_area']*1000)~
       subset(allPairedMD, month == myMon[1] & T_treatment == 'ambient')[,'Tair_al'], pch = myChar[1],
     col = 'white', bg = 'white', ylab = expression(Ln~(italic(g)[m]~mmol~m^-2~s^-1)),
     xlab = expression(italic(T)[air]~(degree*C)),
     ylim = c(1.5,7.2), xlim = c(18, 40), cex.lab = 1.5)
for (i in 1:length(myMon)){
  points(log(subset(allPairedMD, month == myMon[i] & T_treatment == 'ambient')[,'gmes_area']*1000)~
           subset(allPairedMD, month == myMon[i] & T_treatment == 'ambient')[,'Tair_al'],
         pch = myChar[i], col = scales::alpha('blue',0.5), bg = scales::alpha('blue', 0.15))
  points(log(subset(allPairedMD, month == myMon[i] & T_treatment == 'warmed')[,'gmes_area']*1000)~
           subset(allPairedMD, month == myMon[i] & T_treatment == 'warmed')[,'Tair_al'],
         pch = myChar[i], col = scales::alpha('red',0.5), bg = scales::alpha('red', 0.2))
}
smoothplot(Tair_al, log(gmes_area*1000), T_treatment, data = allPairedMD,
           pointcols = c(NA, NA),
           linecols = c("blue",'red'), polycolor = c(scales::alpha("blue",0.15), scales::alpha('red',0.15)),
           kgam = 6, R = 'chamber',  add = T)
legend('topright', legend = c('Amb','Warm', myMon), pch = c(19, 19, myChar),
       col = c('blue','red', rep('black', length(myMon))), bty = 'n', cex = 1.1)

par(mar=c(5,5,1,1), cex=1.2, las=1)
plot(subset(allPairedMD, month == myMon[1] & T_treatment == 'ambient')[,'gmes_area']~
       subset(allPairedMD, month == myMon[1] & T_treatment == 'ambient')[,'Tair_al'], pch = myChar[1],
     col = 'white', bg = 'white', ylab = expression(italic(g)[m]~mol~m^-2~s^-1),
     xlab = expression(italic(T)[air]~(degree*C)),
     ylim = c(0, 1.1), xlim = c(18, 40), cex.lab = 1.5)
for (i in 1:length(myMon)){
  points(subset(allPairedMD, month == myMon[i] & T_treatment == 'ambient')[,'gmes_area']~
           subset(allPairedMD, month == myMon[i] & T_treatment == 'ambient')[,'Tair_al'],
         pch = myChar[i], col = scales::alpha('blue',0.5), bg = scales::alpha('blue', 0.15))
  points(subset(allPairedMD, month == myMon[i] & T_treatment == 'warmed')[,'gmes_area']~
           subset(allPairedMD, month == myMon[i] & T_treatment == 'warmed')[,'Tair_al'],
         pch = myChar[i], col = scales::alpha('red',0.5), bg = scales::alpha('red', 0.2))
}
smoothplot(Tair_al, gmes_area, T_treatment, data = allPairedMD,
           pointcols = c(NA, NA),
           linecols = c("blue",'red'), polycolor = c(scales::alpha("blue",0.15), scales::alpha('red',0.15)),
           kgam = 6, R = 'chamber',  add = T)
