source('scripts/canopy_gmes.R')
march <- subset(allPaired, datetimeFM>=lubridate::ymd_hm('2014-03-22 06:00') & 
                  datetimeFM<=lubridate::ymd_hm('2014-03-23 22:00'))
march <- doBy::orderBy(~datetimeFM, march)
k <- subset(march, chamber=='C09')
windows(18,8)
par(mfrow=c(1,3))
plot(k$FluxCO2~k$datetimeFM, ylab='CO2 flux (mmol/s)', xlab='', type='l', col= 'red', lwd=2, cex.lab=1.5)
plot(k$FluxH2O~k$datetimeFM, ylab='H2O flux (mol/s)', xlab='', type='l', col='blue', main='Ch 9 22 Mar 14', lwd=2, cex.lab=1.5)
plot(k$gsc_area~k$datetimeFM, ylab='gsc (mol/m2/s)', xlab= '', col='forestgreen', lwd=2, cex.lab=1.5, type='l')

plot(k$CO2sampleWTC~k$datetimeFM, type= 'l', lty=1, ylim=c(370, 1200), ylab='[CO2] ppm', xlab='', lwd=2, main='Ch 9 22 March 2014')
lines(k$Cin~k$datetimeFM, col='green', lwd=2)
lines(k$CO2refWTC~k$datetimeFM, lwd=2, lty=2)
legend('topright', legend=c('CO2 amb', 'CO2 out','CO2 entering'), lty=c(2,1,1), col=c('black', 'black', 'green'), bty='n')

plot(k$Cin~k$datetimeFM, type= 'l', ylim=c(150, 1200), ylab='[CO2] ppm', xlab='', lwd=2, col='green', main='Ch 9 22 March 2014')
lines(k$Ci~k$datetimeFM, col='blue', lwd=2)
lines(k$CO2refWTC~k$datetimeFM, lty=2, lwd=2)
lines(k$CO2sampleWTC~k$datetimeFM, lwd=2)
legend('topright', legend=c('Ce', 'Ci', 'CO2 amb', 'CO2 out'), lty=c(1,1,2,1), col=c('green','blue','black','black'), bty='n')

par(mfrow=c(1,1), mar=c(4,6,4,1))
plot(k$Corrdel13C_Avg~k$datetimeFM, ylim=c(-24, -8), type='l', lwd=2,
     ylab=expression({delta}^13*C~~('\211')), xlab='', cex.lab=1.5, main='Ch 9 22 Mar 14')
lines(k$del13C_theor_ref~k$datetimeFM, col='green', lwd=2)
lines(k$Corrdel13C_Avg_ref~k$datetimeFM, lwd=2, lty=2)
legend('topright', legend=c('amb', 'sample', 'theor in'), lty=c(2,1,1), col=c('black','black','green'), bty='n')

windows(8,12)
par(cex.axis=1, cex.lab=1,las=1,mgp=c(3,1,0),mfrow=c(2,1)) 
par(mar=c(0,6,2,2))
plot(k$gsc~k$datetimeFM, col='blue', pch=19, cex.lab=1.6, axes= F, ylab='')
points(k$gmes~k$datetimeFM, col='forestgreen', pch=19)
axis(2, k$gsc, at=c(1:6), labels=c(1:6))
mtext(expression(italic(g)[c]~mol~tree^-1~s^-1),side=2, line=3, las=0, cex=1.3)
box()
legend('topright', legend=c(expression(italic(g)[s]), expression(italic(g)[mes])), pch=19, col=c('blue','forestgreen'), bty='n')
par(mar=c(3,6,0,2))
plot(k$gsc_area~k$datetimeFM, col='blue', ylab=expression(italic(g)[c]~mol~m^-2~s^-1), xlab='', pch=19, cex.lab=1.3)
points(k$gmes_area~k$datetimeFM, col='forestgreen', pch=19)

chAmb <- c('C01','C09','C03','C05','C11','C07')
chEle <- c('C02','C04','C06','C08','C10','C12')
palAmb <- c('blue','cyan','cornflowerblue','deepskyblue','darkblue','navy')
palEle <- c('red','orange','magenta','darkred','firebrick','darkorange4')
windows(12,8)
par(cex.axis=1, cex.lab=1,las=1,mgp=c(3,1,0),mfrow=c(2,3))
par(mar=c(0,6,3,2))
plot(subset(march, chamber==chAmb[1])[,'A_area']~subset(march, chamber==chAmb[1])[,'datetimeFM'], type='l',
     col=palAmb[1],axes=F, ylab='', ylim=c(-4,12), xlim=ymd_hm(c('2014-03-22 06:00','2014-03-23 22:00')))
axis(2, subset(march, chamber==chAmb[1])[,'A_area'], at=c(-4,0,4,8,12), labels=c(-4,0,4,8,12))
mtext(expression(italic(A)[net]~mu*mol~m^-2~s^-1),side=2, line=3, las=0, cex=1.3)
box()
for(i in 2:length(chAmb)){
  lines(subset(march, chamber==chAmb[i])[,'A_area']~subset(march, chamber==chAmb[i])[,'datetimeFM'], col=palAmb[i])
}
legend('topright', legend=chAmb, lty=1, col=palAmb, bty= 'n')
plot(subset(march, chamber==chAmb[1])[,'gsc_area']~subset(march, chamber==chAmb[1])[,'datetimeFM'], type='l',
     col=palAmb[1],axes=F, ylab='', ylim=c(-0.05,0.7), main='Amb T', xlim=ymd_hm(c('2014-03-22 06:00','2014-03-23 22:00')))
axis(2, subset(march, chamber==chAmb[1])[,'gsc_area'], at=c(0.1,0.2,0.3,0.4,0.5,0.6), labels=c(0.1,0.2,0.3,0.4,0.5,0.6))
mtext(expression(italic(g)[sc]~mol~m^-2~s^-1),side=2, line=3, las=0, cex=1.3)
box()
for(i in 2:length(chAmb)){
  lines(subset(march, chamber==chAmb[i])[,'gsc_area']~subset(march, chamber==chAmb[i])[,'datetimeFM'], col=palAmb[i])
}
plot(subset(march, chamber==chAmb[1])[,'gmes_area']~subset(march, chamber==chAmb[1])[,'datetimeFM'], type='l',
     col=palAmb[1],axes=F, ylab='', ylim=c(-0.05,0.45), xlim=ymd_hm(c('2014-03-22 06:00','2014-03-23 22:00')))
axis(2, subset(march, chamber==chAmb[1])[,'gmes_area'], at=c(0,0.1,0.2,0.3,0.4), labels=c(0,0.1,0.2,0.3,0.4))
mtext(expression(italic(g)[mes]~mol~m^-2~s^-1),side=2, line=3, las=0, cex=1.3)
box()
for(i in 2:length(chAmb)){
  lines(subset(march, chamber==chAmb[i])[,'gsc_area']~subset(march, chamber==chAmb[i])[,'datetimeFM'], col=palAmb[i])
}
par(mar=c(4,6,0,2))
plot(subset(march, chamber==chEle[1])[,'A_area']~subset(march, chamber==chEle[1])[,'datetimeFM'], type='l',
     col=palEle[1],ylab=expression(italic(A)[net]~mu*mol~m^-2~s^-1), ylim=c(-4,12), xlab= '', cex.lab=1.6,
     xlim=ymd_hm(c('2014-03-22 06:00','2014-03-23 22:00')))
for(i in 2:length(chEle)){
  lines(subset(march, chamber==chEle[i])[,'A_area']~subset(march, chamber==chEle[i])[,'datetimeFM'], col=palEle[i])
}
legend('topright', legend=chEle, lty=1, col=palEle, bty= 'n')
plot(subset(march, chamber==chEle[1])[,'gsc_area']~subset(march, chamber==chEle[1])[,'datetimeFM'], type='l',
     col=palEle[1], ylab=expression(italic(g)[sc]~mol~m^-2~s^-1), ylim=c(-0.05,0.7), xlab='Ele T', cex.lab=1.6,
     xlim=ymd_hm(c('2014-03-22 06:00','2014-03-23 22:00')))
for(i in 2:length(chEle)){
  lines(subset(march, chamber==chEle[i])[,'gsc_area']~subset(march, chamber==chEle[i])[,'datetimeFM'], col=palEle[i])
}
plot(subset(march, chamber==chEle[1])[,'gmes_area']~subset(march, chamber==chEle[1])[,'datetimeFM'], type='l',
     col=palEle[1],ylab=expression(italic(g)[mes]~mol~m^-2~s^-1), xlab='', ylim=c(-0.05,0.45), cex.lab=1.6,
     xlim=ymd_hm(c('2014-03-22 06:00','2014-03-23 22:00')))
for(i in 2:length(chEle)){
  lines(subset(march, chamber==chEle[i])[,'gsc_area']~subset(march, chamber==chEle[i])[,'datetimeFM'], col=palEle[i])
}

par(mfrow=c(1,1), mar=c(4,6,2,2))
plot(subset(march, chamber==chAmb[1])[,'gmes_area']~subset(march, chamber==chAmb[1])[,'datetimeFM'], type='l',
     col=palAmb[1],ylab=expression(italic(g)[mes]~mol~m^-2~s^-1), xlab='', ylim=c(-0.06,0.55), cex.lab=1.6,
     xlim=ymd_hm(c('2014-03-22 06:00','2014-03-23 22:00')))
for(i in 2:length(chAmb)){
  lines(subset(march, chamber==chAmb[i])[,'gsc_area']~subset(march, chamber==chAmb[i])[,'datetimeFM'], col=palAmb[i])
}
for(i in 1:length(chEle)){
  lines(subset(march, chamber==chEle[i])[,'gsc_area']~subset(march, chamber==chEle[i])[,'datetimeFM'], col=palEle[i])
}
legend('topright', legend=c(chAmb, chEle), lty=1, col=c(palAmb, palEle), bty='n')

marchDay <- subset(march, PAR>=60)
plot(marchDay$gmes_area~marchDay$gsc_area, pch=19, col=as.factor(marchDay$T_treatment), ylim=c(-0.01, 0.06),
     ylab=expression(italic(g)[mes]~mol~m^-2~s^-1), xlab=expression(italic(g)[sc]~mol~m^-2~s^-1), cex.lab=1.6)
