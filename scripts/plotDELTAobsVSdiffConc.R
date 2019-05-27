windows(15,8)
par(mfrow=c(1,2), mar=c(4,4,1,1))
plot(allPaired$DELTAobs*1000~allPaired$diffConc, ylab=expression(Delta^13*C[obs]),
     xlab=expression(italic(C)[ent]-italic(C)[out]), xlim=c(0,900), pch=19, cex=0.7)
abline(quantile(allPaired$DELTAobs, prob=0.05, na.rm=T)*1000, 0, lty=2)
abline(quantile(allPaired$DELTAobs, prob=0.95, na.rm=T)*1000, 0, lty=2)
legend('bottomright', lty=2, legend='95% CI', bty='n')


plot(allPaired$DELTAobs*1000~allPaired$diffConc, ylab=expression(Delta^13*C[obs]),
     xlab=expression(italic(C)[ent]-italic(C)[out]), xlim=c(0,100), pch=19, cex=0.7)
abline(quantile(allPaired$DELTAobs, prob=0.05, na.rm=T)*1000, 0, lty=2)
abline(quantile(allPaired$DELTAobs, prob=0.95, na.rm=T)*1000, 0, lty=2)
lines(x=c(10,10,10), y=c(-10000000, 0, 1000000), col='blue')
lines(x=c(25,25,25), y=c(-10000000, 0, 1000000), col='deeppink')
lines(x=c(35,35,35), y=c(-10000000, 0, 1000000), col='darkgrey')
lines(x=c(50,50,50), y=c(-10000000, 0, 1000000), col='darkolivegreen4')
legend('bottomright', lty=c(2,1,1,1,1), col=c('black', 'blue', 'deeppink', 'darkgrey', 'darkolivegreen4'),
       legend=c('95% CI', '10 ppm', '25 ppm', '35 ppm', '50 ppm'), bty='n')
