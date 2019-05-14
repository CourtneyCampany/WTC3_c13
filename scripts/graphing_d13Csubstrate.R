# graphs to compre d13CAnet and d13CRedark (minimal filtering)
photo <- subset(allPaired, A_area > 0 & PAR >= 3)
resp <- subset(allPaired, PAR < 5)
windows(10,8)
par(mfrow=c(4,3))
par(mar=c(2,3.5,0.5,1))
myMon <- c('Oct','Dec','Jan','Feb','Mar','Apr')
pointsPhoto <- c(0,1,2,5,6,3)
pointsResp <- c(15,16,17,18,14,8)
chambs <- c(paste0('C0', 1:9), paste0('C', 10:12))
for (j in 1:length(chambs)){
  plot(subset(photo, chamber==chambs[j] & month==myMon[1])$deltaSubstrate ~
         subset(photo, chamber==chambs[j] & month==myMon[1])$Time,
       ylab=expression(delta^13), xlab=' ', xlim=c(0,24), ylim=c(-50, 0), pch=pointsPhoto[1])
  abline(0,0)
  for(i in 2:length(myMon)){
    points(subset(photo, chamber==chambs[j] & month==myMon[i])$deltaSubstrate ~
             subset(photo, chamber==chambs[j] & month==myMon[i])$Time, pch=pointsPhoto[i])
  }
  for(i in 1:length(myMon)){
    points(subset(resp, chamber==chambs[j] & month==myMon[i])$deltaSubstrate ~
             subset(resp, chamber==chambs[j] & month==myMon[i])$Time, pch=pointsResp[i])
  }
}

# graph d13CAnet
windows(15,8)
par(mfrow=c(4,3))
par(mar=c(1.5,4.5,3,1))
myMon <- c('Oct','Dec','Jan','Feb','Mar','Apr')
myPal <- c('red','blue','green','darkorange','black','grey')
chambs <- c(paste0('C0', 1:9), paste0('C', 10:12))
for (j in 1:length(chambs)){
  plot(subset(photo, chamber==chambs[j] & month==myMon[1])$deltaSubstrate ~
         subset(photo, chamber==chambs[j] & month==myMon[1])$Time, cex=1.3, main=chambs[j], cex.lab=1.3,
       ylab=expression(delta^13*C[Anet]), xlab=' ', xlim=c(6, 18), ylim=c(-35, -15), pch=19, col=myPal[1])
  for(i in 2:length(myMon)){
    points(subset(photo, chamber==chambs[j] & month==myMon[i])$deltaSubstrate ~
             subset(photo, chamber==chambs[j] & month==myMon[i])$Time, cex=1.3, pch=19, col=myPal[i])
  }
}
legend('topleft', legend=myMon, col=myPal, pch=19, bty='n')

# graph d13CRdark
windows(15,8)
par(mfrow=c(3,4))
par(mar=c(2,4.5,2,1))
myPal <- c('red','blue','green','yellow','black','grey')
myMon <- c('Oct','Dec','Jan','Feb','Mar','Apr')
chambs <- c(paste0('C0', 1:9), paste0('C', 10:12))
for (j in 1:length(chambs)){
  plot(subset(resp, chamber==chambs[j] & month==myMon[1])$deltaSubstrate ~
         subset(resp, chamber==chambs[j] & month==myMon[1])$timeSinceSunset, main=chambs[j], cex.lab=1.3,
       ylab=expression(delta^13*C[Rdark]), xlab=' ', xlim=c(0,11), ylim=c(-100, 0), pch=19, col=myPal[1])
  abline(-10, 0)
  abline(-60, 0)
  #abline(subset(photoSummPhl, chamber==chambs[j] & month==myMon[1])$deltaSubstrate.mean.na, 0, col=myPal[1])
  #abline(subset(photoSummPhl, chamber==chambs[j] & month==myMon[1])$d13Cph, 0, col=myPal[1], lty=2, lwd=1.8)
  legend('bottomright', legend=myMon, col=myPal, pch=19, bty='n')
  for(i in 2:length(myMon)){
    points(subset(resp, chamber==chambs[j] & month==myMon[i])$deltaSubstrate ~
             subset(resp, chamber==chambs[j] & month==myMon[i])$timeSinceSunset, pch=19, col=myPal[i])
    #abline(subset(photoSummPhl, chamber==chambs[j] & month==myMon[i])$deltaSubstrate.mean.na, 0, col=myPal[i])
    #abline(subset(photoSummPhl, chamber==chambs[j] & month==myMon[i])$d13Cph, 0, col=myPal[i], lty=2, lwd=1.8)
  }
}