source('scripts/theGoodiWUEcorrV2.R')

myMon <- c('Oct','Dec','Jan','Feb','Mar')
myChar <- c(21:25)
palette(c('blue', 'red'))

windows(10,10)
par(mfrow=c(1,1), las=1, cex=1.1, mar=c(5, 5, 0.5, 0.5))
iWUEdf <- as.data.frame(iWUEsumm)
plot(x=subset(iWUEdf, month==myMon[1] & T_treatment=='ambient')[,'iWUEphCorrMean'],
     y=subset(iWUEdf, month==myMon[1] & T_treatment=='ambient')[,'iWUEgeMean'],
     pch=NA, col='black', bg=scales::alpha('blue', 0.5), cex=1.1,
     xlab=expression(iWUE[Delta~ph~corr]~'-'~italic(A)/italic(g)[m]~(mu*mol~mol^-1)),
     ylab=expression(iWUE[ge]~(mu*mol~mol^-1)), cex.lab=1.5,
     xlim=c(105, 210), ylim=c(105, 210))
abline(0, 1, lty = 2, lwd = 2)
# plotrix::ablineclip(lm(iWUEgeMean ~ iWUEphCorrMean, data=iWUEdf), x1 = min(iWUEdf$iWUEphCorrMean, na.rm = T),
#                     x2 = max(iWUEdf$iWUEphCorrMean, na.rm = T), col='darkgrey', lwd=2)
plotrix::ablineclip(lm(iWUEgeMean ~ iWUEphCorr2Mean, data=iWUEdf), x1 = min(iWUEdf$iWUEphCorr2Mean, na.rm = T),
                    x2 = max(iWUEdf$iWUEphCorr2Mean, na.rm = T), lwd=2.5)
# for(i in 2:length(myMon)){
#   points(x=subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'iWUEphCorrMean'],
#         y=subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'iWUEgeMean'],
#         pch=myChar[i], col='black', bg=scales::alpha('blue', 0.5), cex=1.1)
# }
# for(i in 1:length(myMon)){
#   points(x=subset(iWUEdf, month==myMon[i] & T_treatment=='warmed')[,'iWUEphCorrMean'],
#         y=subset(iWUEdf, month==myMon[i] & T_treatment=='warmed')[,'iWUEgeMean'],
#         pch=myChar[i], col='black', bg=scales::alpha('red', 0.5), cex=1.1)
# }

iWUEdf <- as.data.frame(iWUEsumm)
for(i in 1:length(myMon)){
  Hmisc::errbar(x=subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'iWUEphCorr2Mean'],
                y=subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'iWUEgeMean'],
                yplus=subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'iWUEgeMean']+
                  subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'iWUEgeSE'],
                yminus=subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'iWUEgeMean']-
                  subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'iWUEgeSE'],
                pch=myChar[i], col='black', bg='blue', errbar.col = 'black', add=T, cex=2)
  arrows(x0 = subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'iWUEphCorr2Mean'],
         y0 = subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'iWUEgeMean'],
         x1 = subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'iWUEphCorr2Mean']+
           subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'iWUEphCorr2SE'],
         length = 0.03, angle = 90, code = 2)
  arrows(x0 = subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'iWUEphCorr2Mean'],
         y0 = subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'iWUEgeMean'],
         x1 = subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'iWUEphCorr2Mean']-
           subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'iWUEphCorr2SE'],
         length = 0.03, angle = 90, code = 2)
}
for(i in 1:length(myMon)){
  Hmisc::errbar(x=subset(iWUEdf, month==myMon[i] & T_treatment=='warmed')[,'iWUEphCorr2Mean'],
                y=subset(iWUEdf, month==myMon[i] & T_treatment=='warmed')[,'iWUEgeMean'],
                yplus=subset(iWUEdf, month==myMon[i] & T_treatment=='warmed')[,'iWUEgeMean']+
                  subset(iWUEdf, month==myMon[i] & T_treatment=='warmed')[,'iWUEgeSE'],
                yminus=subset(iWUEdf, month==myMon[i] & T_treatment=='warmed')[,'iWUEgeMean']-
                  subset(iWUEdf, month==myMon[i] & T_treatment=='warmed')[,'iWUEgeSE'],
                pch=myChar[i], col='black', bg='red', errbar.col = 'black', add=T, cex=2)
  arrows(x0 = subset(iWUEdf, month==myMon[i] & T_treatment=='warmed')[,'iWUEphCorr2Mean'],
         y0 = subset(iWUEdf, month==myMon[i] & T_treatment=='warmed')[,'iWUEgeMean'],
         x1 = subset(iWUEdf, month==myMon[i] & T_treatment=='warmed')[,'iWUEphCorr2Mean']+
           subset(iWUEdf, month==myMon[i] & T_treatment=='warmed')[,'iWUEphCorr2SE'],
         length = 0.03, angle = 90, code = 2)
  arrows(x0 = subset(iWUEdf, month==myMon[i] & T_treatment=='warmed')[,'iWUEphCorr2Mean'],
         y0 = subset(iWUEdf, month==myMon[i] & T_treatment=='warmed')[,'iWUEgeMean'],
         x1 = subset(iWUEdf, month==myMon[i] & T_treatment=='warmed')[,'iWUEphCorr2Mean']-
           subset(iWUEdf, month==myMon[i] & T_treatment=='warmed')[,'iWUEphCorr2SE'],
         length = 0.03, angle = 90, code = 2)
}
legend('topleft', legend=c('Amb', 'Warm', myMon), pch=c(19, 19, myChar),
       col=c('blue', 'red', rep('black', length(myMon))), bty='n', cex=1.3)
# legend('bottomright', legend=c(expression(iWUE[Delta~ph-corr]), expression(iWUE[Delta~ph])),
#        pch=19, lty=1, col=c('black', scales::alpha('darkgrey', 0.7)),
#        bty='n', cex = 1.5, lwd=2)
