windows(12,8)
par(mfrow=c(2,2))
plot(subset(allPaired, A_area > 0)[,'VPDair'], subset(allPaired, A_area > 0)[,'diffConc'],
     ylab='Cent - Cout', xlab='VPD (kPa)')
points(subset(allPaired, A_area > 0 & VPDair < 0.1)[,'VPDair'], subset(allPaired, A_area > 0 & VPDair < 0.1)[,'diffConc'],
       pch=19, col='red')
abline(10,0)
legend('topright', pch=19, col='red', legend='VPD < 0.1 kPa', bty='n')
plot(allPaired$xi~allPaired$VPDair, ylab=expression(xi), xlab='VPD (kPa)')
plot(allPaired$xi~allPaired$CO2in, ylab=expression(xi), xlab='CO2 entering')
plot(allPaired$xi~allPaired$diffConc, ylab=expression(xi), xlab='Cent - Cout', xlim=c(0,900), ylim=c(0,200))
abline(10,0)