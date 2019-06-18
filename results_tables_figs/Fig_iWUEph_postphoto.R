source('scripts/theGoodiWUEcorrV2.R')

myMon <- c('Oct','Dec','Jan','Feb','Mar')
myChar <- c(21:25)
palette(c('blue', 'red'))

windows(10,10)
par(mfrow=c(1,1), las=1, cex=1.25, mar=c(5, 5, 0.5, 0.5))
iWUEdf <- as.data.frame(iWUEsumm)
plot(x=subset(iWUEdf, month==myMon[1] & T_treatment=='ambient')[,'iWUEphCorrMean'],
     y=subset(iWUEdf, month==myMon[1] & T_treatment=='ambient')[,'iWUEgeMean'],
     pch=myChar[1], col='black', bg=scales::alpha('blue', 0.5), cex=1.1,
     xlab=expression(iWUE[Delta~ph]~'-'~italic(A)/italic(g)[m]~(mu*mol~mol^-1)),
     ylab=expression(iWUE[ge]~(mu*mol~mol^-1)), cex.lab=1.5,
     xlim=c(75, 210), ylim=c(75, 210))
abline(0, 1, lty=2)
plotrix::ablineclip(lm(iWUEgeMean ~ iWUEphCorrMean, data=iWUEdf), x1 = min(iWUEdf$iWUEphCorrMean, na.rm = T),
                    x2 = max(iWUEdf$iWUEphCorrMean, na.rm = T), col='darkgrey', lwd=2)
plotrix::ablineclip(lm(iWUEgeMean ~ iWUEphCorr2Mean, data=iWUEdf), x1 = min(iWUEdf$iWUEphCorr2Mean, na.rm = T),
                    x2 = max(iWUEdf$iWUEphCorr2Mean, na.rm = T), lwd=2)
for(i in 2:length(myMon)){
  points(x=subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'iWUEphCorrMean'],
        y=subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'iWUEgeMean'],
        pch=myChar[i], col='black', bg=scales::alpha('blue', 0.5), cex=1.1)
}
for(i in 1:length(myMon)){
  points(x=subset(iWUEdf, month==myMon[i] & T_treatment=='warmed')[,'iWUEphCorrMean'],
        y=subset(iWUEdf, month==myMon[i] & T_treatment=='warmed')[,'iWUEgeMean'],
        pch=myChar[i], col='black', bg=scales::alpha('red', 0.5), cex=1.1)
}
for(i in 1:length(myMon)){
  points(x=subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'iWUEphCorr2Mean'],
         y=subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'iWUEgeMean'],
         pch=myChar[i], col='black', bg='blue', cex=2)
}
for(i in 1:length(myMon)){
  points(x=subset(iWUEdf, month==myMon[i] & T_treatment=='warmed')[,'iWUEphCorr2Mean'],
         y=subset(iWUEdf, month==myMon[i] & T_treatment=='warmed')[,'iWUEgeMean'],
         pch=myChar[i], col='black', bg='red', cex=2)
}
legend('topleft', legend=c('Amb', 'Warm', myMon), pch=c(19, 19, myChar),
       col=c('blue', 'red', rep('black', length(myMon))), bty='n', cex=1.5)
legend('bottomright', legend=c(expression(iWUE[Delta~ph-corr]), expression(iWUE[Delta~ph])),
       pch=19, lty=1, col=c('black', scales::alpha('darkgrey', 0.7)),
       bty='n', cex = 1.5, lwd=2)
