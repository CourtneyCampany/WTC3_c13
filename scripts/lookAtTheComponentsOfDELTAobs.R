k <- subset(allPaired, chamber=='C09' & month=='Apr')
k <- subset(k, del13C_theor_ref >= Corrdel13C_Avg)
windows(14,8)
par(mfrow=c(2,3), mar=c(2,6,1,1))
plot(k$Corrdel13C_Avg~k$datetimeFM, ylim=c(-12, -8.5), xlab='', ylab=expression(delta^13*C~('\u2030')), pch=19, col='blue', cex.lab=1.5)
points(k$del13C_theor_ref~k$datetimeFM, pch=19, col='red')
legend('topleft', pch=c(19,19), col=c('blue','red'), legend=c(expression(italic(C)[e]), expression(italic(C)[o])), bty='n', cex=1.5)

plot(x=k$datetimeFM, y=(k$del13C_theor_ref-k$Corrdel13C_Avg), ylab=expression(delta^13*C[o]~'-'~delta^13*C[e]~('\u2030')), xlab='', cex.lab=1.5)

plot(x=k$datetimeFM, y=(k$del13C_theor_ref-k$Corrdel13C_Avg), ylab=expression(delta^13*C[o]~'-'~delta^13*C[e]~('\u2030')), xlab='', cex.lab=1.5)


