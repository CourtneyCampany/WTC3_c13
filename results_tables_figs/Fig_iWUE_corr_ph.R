source('scripts/iWUE_comparison_Ubierna.R')

myMon <- c('Oct','Dec','Jan','Feb','Mar')
myChar <- c(21:25)
palette(c('blue', 'red'))

windows(12,6)
par(mfrow=c(1,2), las=1, cex=1.25, mar=c(5, 5, 0.5, 0))
plot(subset(phl, month==myMon[1] & T_treatment=='ambient')[,'iWUEgeMD']~
       subset(phl, month==myMon[1] & T_treatment=='ambient')[,'iWUEph_uncorrMD'], pch=myChar[1],
     axes = F, col=scales::alpha('blue', 0.3), bg=scales::alpha('blue',0.3),  
     ylab=expression(iWUE[ge]~(mu*mol~mol^-1)), xlim=c(25, 165),
     xlab=expression(iWUE[Delta-ph]~(mu*mol~mol^-1)), ylim=c(25, 165), cex.lab=1.3)
axis(1, at=seq(25, 150, 25), labels = seq(25, 150, 25), las=1)
axis(2, at=seq(25, 150, 25), labels = seq(25, 150, 25), las=1)
box()
for(i in 2:length(myMon)){
  points(subset(phl, month==myMon[i] & T_treatment=='ambient' & W_treatment=='control')[,'iWUEgeMD']~
           subset(phl, month==myMon[i] & T_treatment=='ambient' & W_treatment=='control')[,'iWUEph_uncorrMD'],
         pch=myChar[i], col=scales::alpha('blue', 0.3), bg=scales::alpha('blue',0.3))
}
for(i in 1:length(myMon)){
  points(subset(phl, month==myMon[i] & T_treatment=='warmed' & W_treatment=='control')[,'iWUEgeMD']~
           subset(phl, month==myMon[i] & T_treatment=='warmed' & W_treatment=='control')[,'iWUEph_uncorrMD'],
         pch=myChar[i], col=scales::alpha('red', 0.3), bg=scales::alpha('red',0.3))
}
for(i in 4:length(myMon)){
  points(subset(phl, month==myMon[i] & T_treatment=='ambient' & W_treatment=='drydown')[,'iWUEgeMD']~
           subset(phl, month==myMon[i] & T_treatment=='ambient' & W_treatment=='drydown')[,'iWUEph_uncorrMD'],
         pch=myChar[i], col=scales::alpha('blue', 0.3))
}
for(i in 4:length(myMon)){
  points(subset(phl, month==myMon[i] & T_treatment=='warmed' & W_treatment=='drydown')[,'iWUEgeMD']~
           subset(phl, month==myMon[i] & T_treatment=='warmed' & W_treatment=='drydown')[,'iWUEph_uncorrMD'],
         pch=myChar[i], col=scales::alpha('red', 0.3))
}
iWUEdf <- as.data.frame(iWUEsumm)
for(i in 1:length(myMon)){
  Hmisc::errbar(x=subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'iWUEphUncorrMean'],
                y=subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'iWUEgeMean'],
                yplus=subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'iWUEgeMean']+
                  subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'iWUEgeSE'],
                yminus=subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'iWUEgeMean']-
                  subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'iWUEgeSE'],
                pch=myChar[i], col='black', bg='blue', errbar.col = 'black', add=T, cex=1.6)
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
                pch=myChar[i], col='black', bg='red', errbar.col = 'black', add=T, cex=1.6)
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
legend('bottomright', legend=myMon, pch=myChar, cex=1.3, bty='n')
legend(x = 7, y = 170, legend=c('(a)'), text.font = 2, cex = 1.3, bty = 'n', pch=NA)

par(las=1, cex=1.25, mar=c(5, 0, 0.5, 5))
plot(subset(phl, month==myMon[1] & T_treatment=='ambient')[,'iWUEgeMD']~
       subset(phl, month==myMon[1] & T_treatment=='ambient')[,'iWUEph_corrMD'], pch=myChar[1],
     col=scales::alpha('blue', 0.3), bg=scales::alpha('blue',0.3), axes = F,
     xlab=expression(iWUE[Delta-ph-gm]~(mu*mol~mol^-1)),
     xlim=c(25, 165), ylim=c(25, 165), cex.lab=1.3)
axis(1, at=seq(25, 150, 25), labels = seq(25, 150, 25), las=1)
axis(4, at=seq(25, 150, 25), labels = seq(25, 150, 25), las=1)
box()
for(i in 2:length(myMon)){
  points(subset(phl, month==myMon[i] & T_treatment=='ambient' & W_treatment=='control')[,'iWUEgeMD']~
           subset(phl, month==myMon[i] & T_treatment=='ambient' & W_treatment=='control')[,'iWUEph_corrMD'],
         pch=myChar[i], col=scales::alpha('blue', 0.3), bg=scales::alpha('blue',0.3))
}
for(i in 1:length(myMon)){
  points(subset(phl, month==myMon[i] & T_treatment=='warmed' & W_treatment=='control')[,'iWUEgeMD']~
           subset(phl, month==myMon[i] & T_treatment=='warmed' & W_treatment=='control')[,'iWUEph_corrMD'],
         pch=myChar[i], col=scales::alpha('red', 0.3), bg=scales::alpha('red',0.3))
}
for(i in 4:length(myMon)){
  points(subset(phl, month==myMon[i] & T_treatment=='ambient' & W_treatment=='drydown')[,'iWUEgeMD']~
           subset(phl, month==myMon[i] & T_treatment=='ambient' & W_treatment=='drydown')[,'iWUEph_corrMD'],
         pch=myChar[i], col=scales::alpha('blue', 0.3), bg=scales::alpha('white',0.3))
}
for(i in 4:length(myMon)){
  points(subset(phl, month==myMon[i] & T_treatment=='warmed' & W_treatment=='drydown')[,'iWUEgeMD']~
           subset(phl, month==myMon[i] & T_treatment=='warmed' & W_treatment=='drydown')[,'iWUEph_corrMD'],
         pch=myChar[i], col=scales::alpha('red', 0.3), bg=scales::alpha('white',0.3))
}
iWUEdf <- as.data.frame(iWUEsumm)
for(i in 1:length(myMon)){
  Hmisc::errbar(x=subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'iWUEphCorrMean'],
                y=subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'iWUEgeMean'],
                yplus=subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'iWUEgeMean']+
                  subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'iWUEgeSE'],
                yminus=subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'iWUEgeMean']-
                  subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'iWUEgeSE'],
                pch=myChar[i], col='black', bg='blue', errbar.col = 'black', add=T, cex=1.6)
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
                pch=myChar[i], col='black', bg='red', errbar.col = 'black', add=T, cex=1.6)
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
plotrix::ablineclip(lm(iWUEgeMD ~ iWUEph_corrMD, data=phl),
                    x1=min(phl$iWUEph_corrMD, na.rm=T), x2=max(phl$iWUEph_corrMD, na.rm=T))
legend(x = 7, y = 170, legend='(b)', text.font = 2, cex = 1.3, bty = 'n')
legend('topright', legend = c('Amb', 'Warm'), pch = 19, col = c('blue', 'red'), bty = 'n', cex = 1.3)
legend('bottomright', legend = c('Control', 'Drought'), pch = c(19, 21), col = c('darkgrey', 'black'), bty = 'n', cex = 1.3)
