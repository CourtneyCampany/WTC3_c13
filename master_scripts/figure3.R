source('scripts/theGoodiWUEcorrV2.R')

myMon <- c('Oct','Dec','Jan','Feb','Mar')
myChar <- c(21:25)
palette(c('blue', 'red'))

windows(12,6)
par(mfrow=c(1,2), las=1, cex=1.25, mar=c(5, 5, 0.5, 0))
plot(subset(phl, month==myMon[1] & T_treatment=='ambient')[,'iWUEgeMD']~
       subset(phl, month==myMon[1] & T_treatment=='ambient')[,'iWUEph_uncorrMD'], pch=myChar[1],
     col=scales::alpha('blue', 0.3), bg=scales::alpha('blue',0.3),  
     ylab=expression(iWUE[ge]~(mu*mol~mol^-1)), xlim=c(45, 210),
     xlab=expression(iWUE[Delta~ph]~(mu*mol~mol^-1)), ylim=c(85, 251), cex.lab=1.3)
for(i in 2:length(myMon)){
  points(subset(phl, month==myMon[i] & T_treatment=='ambient')[,'iWUEgeMD']~
           subset(phl, month==myMon[i] & T_treatment=='ambient')[,'iWUEph_uncorrMD'],
         pch=myChar[i], col=scales::alpha('blue', 0.3), bg=scales::alpha('blue',0.3))
}
for(i in 1:length(myMon)){
  points(subset(phl, month==myMon[i] & T_treatment=='warmed')[,'iWUEgeMD']~
           subset(phl, month==myMon[i] & T_treatment=='warmed')[,'iWUEph_uncorrMD'],
         pch=myChar[i], col=scales::alpha('red', 0.3), bg=scales::alpha('red',0.3))
}
iWUEdf <- as.data.frame(iWUEsumm)
for(i in 1:length(myMon)){
  Hmisc::errbar(x=subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'iWUEphUncorrMean'],
                y=subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'iWUEgeMean'],
                yplus=subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'iWUEgeMean']+
                  subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'iWUEgeSE'],
                yminus=subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'iWUEgeMean']-
                  subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'iWUEgeSE'],
                pch=myChar[i], col='black', bg='blue', errbar.col = 'black', add=T, cex=2)
  arrows(x0 = subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'iWUEphUncorrMean'],
         y0 = subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'iWUEgeMean'],
         x1 = subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'iWUEphUncorrMean']+
           subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'iWUEphUncorrSE'],
         length = 0.03, angle = 90, code = 2)
  arrows(x0 = subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'iWUEphUncorrMean'],
         y0 = subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'iWUEgeMean'],
         x1 = subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'iWUEphUncorrMean']-
           subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'iWUEphUncorrSE'],
         length = 0.03, angle = 90, code = 2)
}
for(i in 1:length(myMon)){
  Hmisc::errbar(x=subset(iWUEdf, month==myMon[i] & T_treatment=='warmed')[,'iWUEphUncorrMean'],
                y=subset(iWUEdf, month==myMon[i] & T_treatment=='warmed')[,'iWUEgeMean'],
                yplus=subset(iWUEdf, month==myMon[i] & T_treatment=='warmed')[,'iWUEgeMean']+
                  subset(iWUEdf, month==myMon[i] & T_treatment=='warmed')[,'iWUEgeSE'],
                yminus=subset(iWUEdf, month==myMon[i] & T_treatment=='warmed')[,'iWUEgeMean']-
                  subset(iWUEdf, month==myMon[i] & T_treatment=='warmed')[,'iWUEgeSE'],
                pch=myChar[i], col='black', bg='red', errbar.col = 'black', add=T, cex=2)
  arrows(x0 = subset(iWUEdf, month==myMon[i] & T_treatment=='warmed')[,'iWUEphUncorrMean'],
         y0 = subset(iWUEdf, month==myMon[i] & T_treatment=='warmed')[,'iWUEgeMean'],
         x1 = subset(iWUEdf, month==myMon[i] & T_treatment=='warmed')[,'iWUEphUncorrMean']+
           subset(iWUEdf, month==myMon[i] & T_treatment=='warmed')[,'iWUEphUncorrSE'],
         length = 0.03, angle = 90, code = 2)
  arrows(x0 = subset(iWUEdf, month==myMon[i] & T_treatment=='warmed')[,'iWUEphUncorrMean'],
         y0 = subset(iWUEdf, month==myMon[i] & T_treatment=='warmed')[,'iWUEgeMean'],
         x1 = subset(iWUEdf, month==myMon[i] & T_treatment=='warmed')[,'iWUEphUncorrMean']-
           subset(iWUEdf, month==myMon[i] & T_treatment=='warmed')[,'iWUEphUncorrSE'],
         length = 0.03, angle = 90, code = 2)
}
legend('bottomleft', legend=c('Amb', 'Warm', myMon), pch=c(19, 19, myChar), cex=1.2,
       col=c('blue', 'red', rep('black', 5)), bg=c('blue', 'red', rep('black', 5)), bty='n')
legend(x=25, y=260, legend=c('(a) Uncorrected'), text.font = 2, cex = 1.3, bty = 'n', pch=NA)

par(las=1, cex=1.25, mar=c(5, 0, 0.5, 5))
plot(subset(phl, month==myMon[1] & T_treatment=='ambient')[,'iWUEgeMD']~
       subset(phl, month==myMon[1] & T_treatment=='ambient')[,'iWUEph_corrMD'], pch=myChar[1],
     col=scales::alpha('blue', 0.3), bg=scales::alpha('blue',0.3), axes = F,
     xlab=expression(iWUE[ge]~'-'~italic(A)/italic(g)[m]~(mu*mol~mol^-1)),
     xlim=c(45, 210), ylim=c(85, 251), cex.lab=1.3)
axis(1, at=seq(50, 200, 50), labels = seq(50, 200, 50), las=1)
axis(4, at=seq(100, 250, 50), labels = seq(100, 250, 50), las=1)
box()
for(i in 2:length(myMon)){
  points(subset(phl, month==myMon[i] & T_treatment=='ambient')[,'iWUEgeMD']~
           subset(phl, month==myMon[i] & T_treatment=='ambient')[,'iWUEph_corrMD'],
         pch=myChar[i], col=scales::alpha('blue', 0.3), bg=scales::alpha('blue',0.3))
}
for(i in 1:length(myMon)){
  points(subset(phl, month==myMon[i] & T_treatment=='warmed')[,'iWUEgeMD']~
           subset(phl, month==myMon[i] & T_treatment=='warmed')[,'iWUEph_corrMD'],
         pch=myChar[i], col=scales::alpha('red', 0.3), bg=scales::alpha('red',0.3))
}
iWUEdf <- as.data.frame(iWUEsumm)
for(i in 1:length(myMon)){
  Hmisc::errbar(x=subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'iWUEphCorrMean'],
                y=subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'iWUEgeMean'],
                yplus=subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'iWUEgeMean']+
                  subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'iWUEgeSE'],
                yminus=subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'iWUEgeMean']-
                  subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'iWUEgeSE'],
                pch=myChar[i], col='black', bg='blue', errbar.col = 'black', add=T, cex=2)
  arrows(x0 = subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'iWUEphCorrMean'],
         y0 = subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'iWUEgeMean'],
         x1 = subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'iWUEphCorrMean']+
           subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'iWUEphCorrSE'],
         length = 0.03, angle = 90, code = 2)
  arrows(x0 = subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'iWUEphCorrMean'],
         y0 = subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'iWUEgeMean'],
         x1 = subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'iWUEphCorrMean']-
           subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'iWUEphCorrSE'],
         length = 0.03, angle = 90, code = 2)
}
for(i in 1:length(myMon)){
  Hmisc::errbar(x=subset(iWUEdf, month==myMon[i] & T_treatment=='warmed')[,'iWUEphCorrMean'],
                y=subset(iWUEdf, month==myMon[i] & T_treatment=='warmed')[,'iWUEgeMean'],
                yplus=subset(iWUEdf, month==myMon[i] & T_treatment=='warmed')[,'iWUEgeMean']+
                  subset(iWUEdf, month==myMon[i] & T_treatment=='warmed')[,'iWUEgeSE'],
                yminus=subset(iWUEdf, month==myMon[i] & T_treatment=='warmed')[,'iWUEgeMean']-
                  subset(iWUEdf, month==myMon[i] & T_treatment=='warmed')[,'iWUEgeSE'],
                pch=myChar[i], col='black', bg='red', errbar.col = 'black', add=T, cex=2)
  arrows(x0 = subset(iWUEdf, month==myMon[i] & T_treatment=='warmed')[,'iWUEphCorrMean'],
         y0 = subset(iWUEdf, month==myMon[i] & T_treatment=='warmed')[,'iWUEgeMean'],
         x1 = subset(iWUEdf, month==myMon[i] & T_treatment=='warmed')[,'iWUEphCorrMean']+
           subset(iWUEdf, month==myMon[i] & T_treatment=='warmed')[,'iWUEphCorrSE'],
         length = 0.03, angle = 90, code = 2)
  arrows(x0 = subset(iWUEdf, month==myMon[i] & T_treatment=='warmed')[,'iWUEphCorrMean'],
         y0 = subset(iWUEdf, month==myMon[i] & T_treatment=='warmed')[,'iWUEgeMean'],
         x1 = subset(iWUEdf, month==myMon[i] & T_treatment=='warmed')[,'iWUEphCorrMean']-
           subset(iWUEdf, month==myMon[i] & T_treatment=='warmed')[,'iWUEphCorrSE'],
         length = 0.03, angle = 90, code = 2)
}
abline(lm(iWUEgeMD ~ iWUEph_corrMD, data=phl))
legend(x=25, y=260, legend=c('(b) Corrected'), text.font = 2, cex = 1.3, bty = 'n')
