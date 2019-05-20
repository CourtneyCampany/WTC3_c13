source('scripts/theGoodiWUEcorrV2.R')
windows(10, 10)
par(mfrow=c(1,1), mar=c(6,6,1.5,1.5), las=1, cex=1.2)
myMon <- c('Oct','Dec','Jan','Feb','Mar','Apr')
myPch <- c(15, 1, 2, 16, 17, 19)
plot(subset(phl, month==myMon[1] & T_treatment=='ambient')[,'d13CAnet'] ~
       subset(phl, month==myMon[1] & T_treatment=='ambient')[,'d13Cph'],
     xlim=c(-33, -26.5), ylim=c(-32,-24), pch=myPch[1], col='blue', cex=1.3, cex.lab=1.65,
     ylab=expression(delta^13*C[Anet]~('\211')), xlab=expression(delta^13*C[phl]~('\211')))
points(subset(phl, month==myMon[1] & T_treatment=='warmed')['d13CAnet'] ~
         subset(phl, month==myMon[1] & T_treatment=='warmed')['d13Cph'],
       pch=myPch[i], col='red', cex=1.3)
abline(lm(d13CAnet ~ d13Cph, data=phl), lty=2, lwd=1.5)
abline(lm(d13CAnet ~ d13Cph, data=subset(phl, month!='Dec' & month!='Jan')), lwd=1.5)
legend('topleft', legend=myMon, pch=myPch, bty='n')
legend('topright', legend=c('ambient','warmed'), pch=19, col=c('blue','red'), bty='n')
legend('bottomright', legend=c(expression(italic(P)~'= 0.017'), expression(italic(R)^2~'= 0.1'),
                               expression(italic(P)~'< 0.001'), expression(italic(R)^2~'= 0.7')),
       lty=c(3, NA, 1, NA), bty='n')
for(i in 2:length(myMon)){
  points(subset(phl, month==myMon[i] & T_treatment=='ambient')[,'d13CAnet'] ~
           subset(phl, month==myMon[i] & T_treatment=='ambient')[,'d13Cph'],
         pch=myPch[i], col='blue', cex=1.3)
  points(subset(phl, month==myMon[i] & T_treatment=='warmed')[,'d13CAnet'] ~
           subset(phl, month==myMon[i] & T_treatment=='warmed')[,'d13Cph'],
         pch=myPch[i], col='red', cex=1.3)
}