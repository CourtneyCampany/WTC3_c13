# Teresa's Token: rdL6pPFYS4BW2EpQQoYe 
source('scripts/canopy_gmes.R')
march <- subset(allPaired, datetimeFM>=lubridate::ymd_hm('2014-03-22 06:00') & 
                  datetimeFM<=lubridate::ymd_hm('2014-03-23 20:00'))
march <- doBy::orderBy(~datetimeFM, march)
march <- subset(march, condAlert=='no')
k <- subset(march, chamber=='C09' & condAlert=='no')
windows(10,8)
par(mfrow=c(2,2), mgp=c(3,1,0))
par(mar=c(0,6,2,2))
plot(k$A_area~k$datetimeFM, xlab='', ylab=expression(italic(A)[net]~(mu*mol~m^-2~s^-1)), type='l', col= 'red', lwd=2, cex.lab=1.5, axes=F)
axis(2, k$A_area, at=c(-2,0,2,4,6,8,10), labels=c(-2,0,2,4,6,8,10))
box()
plot(k$E_area*1000~k$datetimeFM,  xlab='', ylab=expression(italic(E)~(mmol~m^-2~s^-1)), type='l', col='blue', lwd=2, cex.lab=1.5, axes=F)
axis(2, k$E_area*1000, at=c(0.5,1,1.5), labels=c(0.5,1,1.5))
box()
par(mar=c(4,6,0,2))
plot(k$VPDair~k$datetimeFM, ylab='VPD (kPa)', xlab='', type='l', lwd=2, cex.lab=1.5, col='darkgrey')
plot(k$gsc_area~k$datetimeFM, ylab=expression(italic(g)[sc]~(mol~m^-2~s^-1)), xlab= '', col='forestgreen', lwd=2, cex.lab=1.5, type='l')


plot(k$CO2sampleWTC~k$datetimeFM, type= 'l', lty=1, ylim=c(370, 1200), ylab='[CO2] ppm', xlab='', lwd=2, main='Ch 9 22 March 2014')
lines(k$Cin~k$datetimeFM, col='green', lwd=2)
lines(k$CO2refWTC~k$datetimeFM, lwd=2, lty=2)
legend('topright', legend=c('CO2 amb', 'CO2 out','CO2 entering'), lty=c(2,1,1), col=c('black', 'black', 'green'), bty='n')

plot(k$Cin~k$datetimeFM, type= 'l', ylim=c(min(k$Ci, na.rm=T), 1200), ylab='[CO2] ppm', xlab='', lwd=2, col='green',
     xlim=c(ymd_hm(as.character(c('2014-03-22 10:55', '2014-03-22 16:30')))), main='Ch 9 22 March 2014')
lines(k$Ci~k$datetimeFM, col='blue', lwd=2)
lines(k$CO2refWTC~k$datetimeFM, lty=2, lwd=2)
lines(k$CO2sampleWTC~k$datetimeFM, lwd=2)
legend('topright', legend=c('Ce', 'Ci', 'CO2 amb', 'CO2 out'), lty=c(1,1,2,1), col=c('green','blue','black','black'), bty='n')

par(mfrow=c(1,1), mar=c(4,6,4,1))
plot(k$Corrdel13C_Avg~k$datetimeFM, ylim=c(-24, -8), type='l', lwd=2, 
     ylab=expression({delta}^13*C~~('\211')), xlab='', cex.lab=1.5, main='Ch 9 22 Mar 14')
lines(k$del13C_theor_ref~k$datetimeFM, col='green', lwd=2)
lines(k$Corrdel13C_Avg_ref~k$datetimeFM, lwd=2, lty=2)
legend('bottomright', legend=c('amb', 'sample', 'theor in'), lty=c(2,1,1), col=c('black','black','green'), bty='n')

windows(8,8)
par(cex.axis=1, cex.lab=1,las=1,mgp=c(3,1,0),mfrow=c(1,1)) 
par(mar=c(3,6,4,2))
plot(k$gsc_area~k$datetimeFM, col='blue', ylab=expression(italic(g)[c]~mol~m^-2~s^-1), xlab='', type='l', cex.lab=1.3,
     xlim=c(ymd_hm(as.character(c('2014-03-22 10:55', '2014-03-22 16:30')))), ylim=c(0,0.14), main='CH 9 22-Mar-2014')
lines(k$gmes_area~k$datetimeFM, col='forestgreen', pch=19)
legend('topright', legend=c(expression(italic(g)[s]), expression(italic(g)[mes])), lty=c(1,1), col=c('blue','forestgreen'), bty='n')

chAmb <- c('C01','C09','C03','C05','C11','C07')
chEle <- c('C02','C04','C06','C08','C10','C12')
palAmb <- c('blue','cyan','cornflowerblue','deepskyblue','darkblue','navy')
palEle <- c('red','orange','magenta','darkred','firebrick','darkorange4')
windows(12,8)
par(cex.axis=1, cex.lab=1,las=1,mgp=c(3,1,0),mfrow=c(2,3))
par(mar=c(0,6,3,2))
plot(subset(march, chamber==chAmb[1])[,'A_area']~subset(march, chamber==chAmb[1])[,'datetimeFM'], pch=19,col=palAmb[1],axes=F, ylab='',
      ylim=c(min(march$A_area, na.rm=T), max(march$A_area, na.rm=T)), xlim=ymd_hm(c('2014-03-22 06:00','2014-03-23 22:00')))
axis(2, subset(march, chamber==chAmb[1])[,'A_area'], at=c(-2,0,2,4,6,8,10,12), labels=c(-2,0,2,4,6,8,10,12))
mtext(expression(italic(A)[net]~(mu*mol~m^-2~s^-1)),side=2, line=3, las=0, cex=1.3)
box()
for(i in 2:length(chAmb)){
  points(subset(march, chamber==chAmb[i])[,'A_area']~subset(march, chamber==chAmb[i])[,'datetimeFM'], col=palAmb[i], pch=19)
}
legend('topright', legend=chAmb, pch=19, col=palAmb, bty= 'n')
plot(subset(march, chamber==chAmb[1])[,'gsc_area']~subset(march, chamber==chAmb[1])[,'datetimeFM'], pch=19,col=palAmb[1],axes=F, ylab='', 
     ylim=c(0, 0.7), main='Amb T', xlim=ymd_hm(c('2014-03-22 06:00','2014-03-23 22:00')))
axis(2, subset(march, chamber==chAmb[1])[,'gsc_area'], at=c(0,0.2,0.4,0.6), labels=c(0,0.2,0.4,0.6))
mtext(expression(italic(g)[sc]~(mol~m^-2~s^-1)),side=2, line=3, las=0, cex=1.3)
box()
for(i in 2:length(chAmb)){
  points(subset(march, chamber==chAmb[i])[,'gsc_area']~subset(march, chamber==chAmb[i])[,'datetimeFM'], col=palAmb[i], pch=19)
}
plot(subset(march, chamber==chAmb[1])[,'gmes_area']~subset(march, chamber==chAmb[1])[,'datetimeFM'], pch=19,col=palAmb[1],axes=F, ylab='',
      ylim=c(0, 0.7), xlim=ymd_hm(c('2014-03-22 06:00','2014-03-23 22:00')), xlab='')
axis(2, subset(march, chamber==chAmb[1])[,'gmes_area'], at=c(0,0.2,0.4,0.6), labels=c(0,0.2,0.4,0.6))
mtext(expression(italic(g)[mes]~(mol~m^-2~s^-1)),side=2, line=3, las=0, cex=1.3)
box()
for(i in 2:length(chAmb)){
  points(subset(march, chamber==chAmb[i])[,'gsc_area']~subset(march, chamber==chAmb[i])[,'datetimeFM'], col=palAmb[i], pch=19)
}
par(mar=c(4,6,0,2))
plot(subset(march, chamber==chEle[1])[,'A_area']~subset(march, chamber==chEle[1])[,'datetimeFM'], pch=19,
     col=palEle[1],ylab=expression(italic(A)[net]~(mu*mol~m^-2~s^-1)), ylim=c(min(march$A_area, na.rm=T), max(march$A_area, na.rm=T)),
     xlab= '', cex.lab=1.6,  xlim=ymd_hm(c('2014-03-22 06:00','2014-03-23 22:00')))
for(i in 2:length(chEle)){
  points(subset(march, chamber==chEle[i])[,'A_area']~subset(march, chamber==chEle[i])[,'datetimeFM'], col=palEle[i], pch=19)
}
legend('topright', legend=chEle, pch=19, col=palEle, bty= 'n')
plot(subset(march, chamber==chEle[1])[,'gsc_area']~subset(march, chamber==chEle[1])[,'datetimeFM'], pch=19,
     col=palEle[1], ylab=expression(italic(g)[sc]~(mol~m^-2~s^-1)), ylim=c(0, 0.7), xlab='Ele T', cex.lab=1.6,
     xlim=ymd_hm(c('2014-03-22 06:00','2014-03-23 22:00')))
for(i in 2:length(chEle)){
  points(subset(march, chamber==chEle[i])[,'gsc_area']~subset(march, chamber==chEle[i])[,'datetimeFM'], col=palEle[i], pch=19)
}
plot(subset(march, chamber==chEle[1])[,'gmes_area']~subset(march, chamber==chEle[1])[,'datetimeFM'], pch=19,
     col=palEle[1],ylab=expression(italic(g)[mes]~(mol~m^-2~s^-1)), xlab='', ylim=c(0, 0.7), cex.lab=1.6,
     xlim=ymd_hm(c('2014-03-22 06:00','2014-03-23 22:00')))
for(i in 2:length(chEle)){
  points(subset(march, chamber==chEle[i])[,'gsc_area']~subset(march, chamber==chEle[i])[,'datetimeFM'], col=palEle[i], pch=19)
}

windows(10,8)
par(cex.axis=1, cex.lab=1,las=1,mgp=c(3,1,0),mfrow=c(2,1))
par(mar=c(0,6,2,2))
plot(subset(march, chamber==chAmb[1])[,'gsc_area']~subset(march, chamber==chAmb[1])[,'datetimeFM'], pch=17, col=palAmb[1],axes=F, ylab='', 
     ylim=c(0, 0.7), main='Amb T', xlim=ymd_hm(c('2014-03-22 06:00','2014-03-23 22:00')))
axis(2, subset(march, chamber==chAmb[1])[,'gsc_area'], at=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7), labels=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7))
mtext(expression(italic(g)[c]~(mol~m^-2~s^-1)),side=2, line=3, las=0, cex=1.3)
box()
for(i in 2:length(chAmb)){
  points(subset(march, chamber==chAmb[i])[,'gsc_area']~subset(march, chamber==chAmb[i])[,'datetimeFM'], col=palAmb[i], pch=17)
}
for(i in 1:length(chAmb)){
  points(subset(march, chamber==chAmb[i])[,'gmes_area']~subset(march, chamber==chAmb[i])[,'datetimeFM'], col=palAmb[i], pch=19)
}
legend('topright', legend=chAmb, pch=15, col=palAmb, bty= 'n')
legend('topleft', legend=c(expression(italic(g)[s]), expression(italic(g)[mes])), pch=c(17,19), bty='n')
par(mar=c(4,6,0,2))
plot(subset(march, chamber==chEle[1])[,'gsc_area']~subset(march, chamber==chEle[1])[,'datetimeFM'], pch=17,
     ylab=expression(italic(g)[c]~(mol~m^-2~s^-1)), col=palEle[1], ylim=c(0, 0.7), xlab='Ele T', cex.lab=1.6,
     xlim=ymd_hm(c('2014-03-22 06:00','2014-03-23 22:00')))
for(i in 2:length(chEle)){
  points(subset(march, chamber==chEle[i])[,'gsc_area']~subset(march, chamber==chEle[i])[,'datetimeFM'], col=palEle[i], pch=17)
}
for(i in 1:length(chEle)){
  points(subset(march, chamber==chEle[i])[,'gmes_area']~subset(march, chamber==chEle[i])[,'datetimeFM'], col=palEle[i], pch=19)
}
legend('topright', legend=chEle, pch=15, col=palEle, bty= 'n')
legend('topleft', legend=c(expression(italic(g)[s]), expression(italic(g)[mes])), pch=c(17,19), bty='n')

windows(10,8)
par(cex.axis=1, cex.lab=1,las=1,mgp=c(3,1,0),mfrow=c(2,1))
par(mar=c(0,6,2,2))
plot(subset(march, chamber==chAmb[1])[,'gsc_area']~subset(march, chamber==chAmb[1])[,'datetimeFM'], pch=17, col=palAmb[1],axes=F, ylab='', 
     ylim=c(0, 0.2), main='Amb T', xlim=ymd_hm(c('2014-03-22 06:00','2014-03-23 22:00')))
axis(2, subset(march, chamber==chAmb[1])[,'gsc_area'], at=c(0,0.05,0.1,0.15,0.2), labels=c(0,0.05,0.1,0.15,0.2))
mtext(expression(italic(g)[c]~(mol~m^-2~s^-1)),side=2, line=3, las=0, cex=1.3)
box()
for(i in 2:length(chAmb)){
  points(subset(march, chamber==chAmb[i])[,'gsc_area']~subset(march, chamber==chAmb[i])[,'datetimeFM'], col=palAmb[i], pch=17)
}
for(i in 1:length(chAmb)){
  points(subset(march, chamber==chAmb[i])[,'gmes_area']~subset(march, chamber==chAmb[i])[,'datetimeFM'], col=palAmb[i], pch=19)
}
legend('topright', legend=chAmb, pch=15, col=palAmb, bty= 'n')
legend('topleft', legend=c(expression(italic(g)[s]), expression(italic(g)[mes])), pch=c(17,19), bty='n')
par(mar=c(4,6,0,2))
plot(subset(march, chamber==chEle[1])[,'gsc_area']~subset(march, chamber==chEle[1])[,'datetimeFM'], pch=17,
     ylab=expression(italic(g)[c]~(mol~m^-2~s^-1)), col=palEle[1], ylim=c(0, 0.2), xlab='Ele T', cex.lab=1.6,
     xlim=ymd_hm(c('2014-03-22 06:00','2014-03-23 22:00')))
for(i in 2:length(chEle)){
  points(subset(march, chamber==chEle[i])[,'gsc_area']~subset(march, chamber==chEle[i])[,'datetimeFM'], col=palEle[i], pch=17)
}
for(i in 1:length(chEle)){
  points(subset(march, chamber==chEle[i])[,'gmes_area']~subset(march, chamber==chEle[i])[,'datetimeFM'], col=palEle[i], pch=19)
}
legend('topright', legend=chEle, pch=15, col=palEle, bty= 'n')
legend('topleft', legend=c(expression(italic(g)[s]), expression(italic(g)[mes])), pch=c(17,19), bty='n')


windows(12,7)
par(mfrow=c(1,1), mar=c(4,6,2,2))
plot(subset(march, chamber==chAmb[1])[,'gmes_area']~subset(march, chamber==chAmb[1])[,'datetimeFM'], pch=19,
     col=palAmb[1],ylab=expression(italic(g)[mes]~mol~m^-2~s^-1), xlab='', ylim=c(0,0.35), cex.lab=1.6,
     xlim=ymd_hm(c('2014-03-22 06:00','2014-03-23 22:00')))
for(i in 2:length(chAmb)){
  points(subset(march, chamber==chAmb[i])[,'gsc_area']~subset(march, chamber==chAmb[i])[,'datetimeFM'], col=palAmb[i], pch=19)
}
for(i in 1:length(chEle)){
  points(subset(march, chamber==chEle[i])[,'gsc_area']~subset(march, chamber==chEle[i])[,'datetimeFM'], col=palEle[i], pch=19)
}
legend('topright', legend=c(chAmb, chEle), pch=19, col=c(palAmb, palEle), bty='n')

marchDay <- subset(march, PAR>=10)
windows(8,8)
par(mar=c(6,6,1,1))
plot(marchDay$gmes_area~marchDay$gsc_area, pch=19, col=as.factor(marchDay$T_treatment), ylim=c(0,0.5), xlim=c(0,0.5),
     ylab=expression(italic(g)[mes]~(mol~m^-2~s^-1)), xlab=expression(italic(g)[sc]~(mol~m^-2~s^-1)), cex.lab=1.6)
legend('topright', pch=19, col=c('red','black'), legend=c(levels(as.factor(marchDay$T_treatment))), bty='n')

corrCond <- lm(gmes_area~gsc_area, data=marchDay)
