myMon <- c('Oct','Dec','Jan','Feb','Mar','Apr')
windows(12,8)
par(mfrow=c(2,3))
for (i in 1:length(myMon)){
  k <- subset(allPaired, month==myMon[i])
  plot(k$PAR~k$datetimeFM, ylim=c(0,2200), main=k$month[i],  ylab='PAR', xlab=' ', pch=19, col='white')
  abline(1000, 0, lty=2)
  with(subset(k, midday=='yes'), points(PAR~datetimeFM, pch=19, col='red'))
  with(subset(k, midday=='no'), points(PAR~datetimeFM, pch=19, col='grey'))
}

myMon <- c('Oct','Dec','Jan','Feb','Mar','Apr')
windows(12,8)
par(mfrow=c(2,3), mar=c(4.5,5,1.5,0.5))
for (i in 1:length(myMon)){
  k <- subset(allPaired, month==myMon[i])
  plot(k$A_area~k$PAR, xlim=c(0,2200), ylim=c(-5,25), main=k$month[i], pch=19, col='white', cex.lab=1.4,
       ylab=expression(italic(A)[net]~(mu*mol~m^-2~s^-1)), xlab=expression(PPFD~(mu*mol~m^-2~s^-1)), cex=1.3)
  legend('topleft', legend=c('Non-midday','Midday High Light', 'Midday Low Light'),
         pch=19, col=c('grey','red','blue'), bty = 'n')
  legend('topright', legend=c('Amb','Warm'), pch=c(19,17), bty='n')
  with(subset(k, midday=='no' & T_treatment =='ambient'), points(A_area~PAR, pch=19, col='grey'))
  with(subset(k, midday=='no' & T_treatment =='warmed'), points(A_area~PAR, pch=17, col='grey'))
  with(subset(k, midday=='yes' & T_treatment == 'ambient'), points(A_area~PAR, pch=19, col='red'))
  with(subset(k, midday=='yes' & T_treatment == 'warmed'), points(A_area~PAR, pch=17, col='red'))
  with(subset(k, midday=='yes' & PAR < 800), points(A_area~PAR, pch=19, col='blue'))
  with(subset(k, midday=='yes' & PAR < 800 & T_treatment =='ambient'), points(A_area~PAR, pch=19, col='blue'))
  with(subset(k, midday=='yes' & PAR < 800 & T_treatment =='warmed'), points(A_area~PAR, pch=17, col='blue'))
  with(subset(k, midday=='no'), points(A_area~PAR, pch=19, col='grey'))
}