source('scripts/iWUE_comparison_Ubierna.R')

myMon <- c('Oct','Dec','Jan','Feb','Mar', 'Apr')
myChar <- c(21:25, 11)
palette(c('blue', 'red'))

windows(12,6)
par(mfrow=c(1,2), las=1, cex=1.25, mar=c(5, 5, 0.5, 0))
plot(subset(phl, month==myMon[1] & T_treatment=='ambient')[,'iWUEgeMD']~
       subset(phl, month==myMon[1] & T_treatment=='ambient')[,'iWUEleafAvg_uncorrMD'], pch=myChar[1],
     col=scales::alpha('blue', 0.3), bg=scales::alpha('blue',0.3), axes = F,  
     ylab=expression(iWUE[ge]~(mu*mol~mol^-1)), xlim=c(35, 165),
     xlab=expression(iWUE[Delta-leaf]~(mu*mol~mol^-1)), ylim=c(35, 165), cex.lab=1.3)
axis(1, at=seq(50, 160, 25), labels = seq(50, 160, 25), las=1)
axis(2, at=seq(50, 160, 25), labels = seq(50, 160, 25), las=1)
box()
for(i in 2:(length(myMon)-1)){
  points(subset(phl, month==myMon[i] & T_treatment=='ambient' & W_treatment == 'control')[,'iWUEgeMD']~
           subset(phl, month==myMon[i] & T_treatment=='ambient' & W_treatment == 'control')[,'iWUEleafAvg_uncorrMD'],
         pch=myChar[i], col=scales::alpha('blue', 0.3), bg=scales::alpha('blue',0.3))
}
points(subset(phl, month=='Apr' & T_treatment=='ambient' & W_treatment == 'control')[,'iWUEgeMD']~
         subset(phl, month=='Apr' & T_treatment=='ambient' & W_treatment == 'control')[,'iWUEleafAvg_uncorrMD'],
       pch=24, col=scales::alpha('blue', 0.3), bg=scales::alpha('blue',0.3))
points(subset(phl, month=='Apr' & T_treatment=='ambient' & W_treatment == 'control')[,'iWUEgeMD']~
         subset(phl, month=='Apr' & T_treatment=='ambient' & W_treatment == 'control')[,'iWUEleafAvg_uncorrMD'],
       pch=25, col=scales::alpha('blue', 0.3), bg=scales::alpha('blue',0.3))
for(i in 1:(length(myMon)-1)){
  points(subset(phl, month==myMon[i] & T_treatment=='warmed' & W_treatment == 'control')[,'iWUEgeMD']~
           subset(phl, month==myMon[i] & T_treatment=='warmed' & W_treatment == 'control')[,'iWUEleafAvg_uncorrMD'],
         pch=myChar[i], col=scales::alpha('red', 0.3), bg=scales::alpha('red',0.3))
}
points(subset(phl, month=='Apr' & T_treatment=='warmed' & W_treatment == 'control')[,'iWUEgeMD']~
         subset(phl, month=='Apr' & T_treatment=='warmed' & W_treatment == 'control')[,'iWUEleafAvg_uncorrMD'],
       pch=24, col=scales::alpha('red', 0.3), bg=scales::alpha('red',0.3))
points(subset(phl, month=='Apr' & T_treatment=='warmed' & W_treatment == 'control')[,'iWUEgeMD']~
         subset(phl, month=='Apr' & T_treatment=='warmed' & W_treatment == 'control')[,'iWUEleafAvg_uncorrMD'],
       pch=25, col=scales::alpha('red', 0.3), bg=scales::alpha('red',0.3))
for(i in 4:length(myMon)){
  points(subset(phl, month==myMon[i] & T_treatment=='ambient' & W_treatment == 'drydown')[,'iWUEgeMD']~
           subset(phl, month==myMon[i] & T_treatment=='ambient' & W_treatment == 'drydown')[,'iWUEleafAvg_uncorrMD'],
         pch=myChar[i], col='blue', bg='white')
}
for(i in 4:length(myMon)){
  points(subset(phl, month==myMon[i] & T_treatment=='warmed' & W_treatment == 'drydown')[,'iWUEgeMD']~
           subset(phl, month==myMon[i] & T_treatment=='warmed' & W_treatment == 'drydown')[,'iWUEleafAvg_uncorrMD'],
         pch=myChar[i], col='red', bg='white')
}
iWUEdf <- as.data.frame(iWUEsumm)
points(subset(iWUEdf, month=='Apr' & T_treatment=='ambient')[,'iWUEleafAvgUncorrMean'],
         subset(iWUEdf, month=='Apr' & T_treatment=='ambient')[,'iWUEgeMean'],
       pch=24, col='blue', bg='blue', cex=2)
points(subset(iWUEdf, month=='Apr' & T_treatment=='ambient')[,'iWUEleafAvgUncorrMean'],
         subset(iWUEdf, month=='Apr' & T_treatment=='ambient')[,'iWUEgeMean'],
       pch=25, col='blue', bg='blue', cex=2)
points(subset(iWUEdf, month=='Apr' & T_treatment=='warmed')[,'iWUEleafAvgUncorrMean'],
         subset(iWUEdf, month=='Apr' & T_treatment=='warmed')[,'iWUEgeMean'],
       pch=24, col='red', bg='red', cex=2)
points(subset(iWUEdf, month=='Apr' & T_treatment=='warmed')[,'iWUEleafAvgUncorrMean'],
         subset(iWUEdf, month=='Apr' & T_treatment=='warmed')[,'iWUEgeMean'],
       pch=25, col='red', bg='red', cex=2)
myChar <- c(21:25, 3)
for(i in 1:length(myMon)){
  Hmisc::errbar(x=subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'iWUEleafAvgUncorrMean'],
                y=subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'iWUEgeMean'],
                yplus=subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'iWUEgeMean']+
                  subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'iWUEgeSE'],
                yminus=subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'iWUEgeMean']-
                  subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'iWUEgeSE'],
                pch=myChar[i], col='blue', bg='blue', errbar.col = 'black', add=T, cex=2)
  arrows(x0 = subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'iWUEleafAvgUncorrMean'],
         y0 = subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'iWUEgeMean'],
         x1 = subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'iWUEleafAvgUncorrMean']+
           subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'iWUEleafAvgUncorrSE'],
         length = 0.03, angle = 90, code = 2)
  arrows(x0 = subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'iWUEleafAvgUncorrMean'],
         y0 = subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'iWUEgeMean'],
         x1 = subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'iWUEleafAvgUncorrMean']-
           subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'iWUEleafAvgUncorrSE'],
         length = 0.03, angle = 90, code = 2)
}
for(i in 1:length(myMon)){
  Hmisc::errbar(x=subset(iWUEdf, month==myMon[i] & T_treatment=='warmed')[,'iWUEleafAvgUncorrMean'],
                y=subset(iWUEdf, month==myMon[i] & T_treatment=='warmed')[,'iWUEgeMean'],
                yplus=subset(iWUEdf, month==myMon[i] & T_treatment=='warmed')[,'iWUEgeMean']+
                  subset(iWUEdf, month==myMon[i] & T_treatment=='warmed')[,'iWUEgeSE'],
                yminus=subset(iWUEdf, month==myMon[i] & T_treatment=='warmed')[,'iWUEgeMean']-
                  subset(iWUEdf, month==myMon[i] & T_treatment=='warmed')[,'iWUEgeSE'],
                pch=myChar[i], col='red', bg='red', errbar.col = 'black', add=T, cex=2)
  arrows(x0 = subset(iWUEdf, month==myMon[i] & T_treatment=='warmed')[,'iWUEleafAvgUncorrMean'],
         y0 = subset(iWUEdf, month==myMon[i] & T_treatment=='warmed')[,'iWUEgeMean'],
         x1 = subset(iWUEdf, month==myMon[i] & T_treatment=='warmed')[,'iWUEleafAvgUncorrMean']+
           subset(iWUEdf, month==myMon[i] & T_treatment=='warmed')[,'iWUEleafAvgUncorrSE'],
         length = 0.03, angle = 90, code = 2)
  arrows(x0 = subset(iWUEdf, month==myMon[i] & T_treatment=='warmed')[,'iWUEleafAvgUncorrMean'],
         y0 = subset(iWUEdf, month==myMon[i] & T_treatment=='warmed')[,'iWUEgeMean'],
         x1 = subset(iWUEdf, month==myMon[i] & T_treatment=='warmed')[,'iWUEleafAvgUncorrMean']-
           subset(iWUEdf, month==myMon[i] & T_treatment=='warmed')[,'iWUEleafAvgUncorrSE'],
         length = 0.03, angle = 90, code = 2)
}
myChar <- c(21:25, 11)
legend('bottomleft', legend=c('Amb', 'Warm', myMon), pch=c(19, 19, myChar),
       col=c('blue', 'red', rep('black', length(myMon))), bty='n', cex = 1.2)
legend(x=20, y=170, legend=c('(a)'), text.font = 2, cex = 1.3, bty = 'n', pch=NA)

par(las=1, cex=1.25, mar=c(5, 0, 0.5, 5))
plot(subset(phl, month==myMon[1] & T_treatment=='ambient')[,'iWUEgeMD']~
       subset(phl, month==myMon[1] & T_treatment=='ambient')[,'iWUEleafAvg_corrMD'], pch=myChar[1],
     col=scales::alpha('blue', 0.3), bg=scales::alpha('blue',0.3), axes = F,
     xlab=expression(iWUE[Delta-leaf-gm]~(mu*mol~mol^-1)),
     xlim=c(35, 165), ylim=c(35, 165), cex.lab=1.3)
axis(1, at=seq(50, 160, 25), labels = seq(50, 160, 25), las=1)
axis(4, at=seq(50, 160, 25), labels = seq(50, 160, 20), las=1)
box()
for(i in 2:(length(myMon)-1)){
  points(subset(phl, month==myMon[i] & T_treatment=='ambient' & W_treatment == 'control')[,'iWUEgeMD']~
           subset(phl, month==myMon[i] & T_treatment=='ambient'& W_treatment == 'control')[,'iWUEleafAvg_corrMD'],
         pch=myChar[i], col=scales::alpha('blue', 0.3), bg=scales::alpha('blue',0.3))
}
points(subset(phl, month=='Apr' & T_treatment=='ambient' & W_treatment == 'control')[,'iWUEgeMD']~
         subset(phl, month=='Apr' & T_treatment=='ambient'& W_treatment == 'control')[,'iWUEleafAvg_corrMD'],
       pch=24, col=scales::alpha('blue', 0.3), bg=scales::alpha('blue',0.3))
points(subset(phl, month=='Apr' & T_treatment=='ambient' & W_treatment == 'control')[,'iWUEgeMD']~
         subset(phl, month=='Apr' & T_treatment=='ambient'& W_treatment == 'control')[,'iWUEleafAvg_corrMD'],
       pch=25, col=scales::alpha('blue', 0.3), bg=scales::alpha('blue',0.3))
for(i in 1:(length(myMon)-1)){
  points(subset(phl, month==myMon[i] & T_treatment=='warmed' & W_treatment == 'control')[,'iWUEgeMD']~
           subset(phl, month==myMon[i] & T_treatment=='warmed' & W_treatment == 'control')[,'iWUEleafAvg_corrMD'],
         pch=myChar[i], col=scales::alpha('red', 0.3), bg=scales::alpha('red',0.3))
}
points(subset(phl, month=='Apr' & T_treatment=='warmed' & W_treatment == 'control')[,'iWUEgeMD']~
         subset(phl, month=='Apr' & T_treatment=='warmed'& W_treatment == 'control')[,'iWUEleafAvg_corrMD'],
       pch=24, col=scales::alpha('red', 0.3), bg=scales::alpha('red',0.3))
points(subset(phl, month=='Apr' & T_treatment=='warmed' & W_treatment == 'control')[,'iWUEgeMD']~
         subset(phl, month=='Apr' & T_treatment=='warmed'& W_treatment == 'control')[,'iWUEleafAvg_corrMD'],
       pch=25, col=scales::alpha('red', 0.3), bg=scales::alpha('red',0.3))
for(i in 4:length(myMon)){
  points(subset(phl, month==myMon[i] & T_treatment=='ambient' & W_treatment == 'drydown')[,'iWUEgeMD']~
           subset(phl, month==myMon[i] & T_treatment=='ambient'& W_treatment == 'drydown')[,'iWUEleafAvg_corrMD'],
         pch=myChar[i], col='blue', bg='white')
}
for(i in 4:length(myMon)){
  points(subset(phl, month==myMon[i] & T_treatment=='warmed' & W_treatment == 'drydown')[,'iWUEgeMD']~
           subset(phl, month==myMon[i] & T_treatment=='warmed' & W_treatment == 'drydown')[,'iWUEleafAvg_corrMD'],
         pch=myChar[i], col='red', bg='white')
}
iWUEdf <- as.data.frame(iWUEsumm)
points(subset(iWUEdf, month=='Apr' & T_treatment=='ambient' )[,'iWUEleafAvgCorrMean'],
         subset(iWUEdf, month=='Apr' & T_treatment=='ambient')[,'iWUEgeMean'],
       pch=24, col='blue', bg='blue', cex=2)
points(subset(iWUEdf, month=='Apr' & T_treatment=='ambient' )[,'iWUEleafAvgCorrMean'],
       subset(iWUEdf, month=='Apr' & T_treatment=='ambient')[,'iWUEgeMean'],
       pch=25, col='blue', bg='blue', cex=2)
points(subset(iWUEdf, month=='Apr' & T_treatment=='warmed' )[,'iWUEleafAvgCorrMean'],
       subset(iWUEdf, month=='Apr' & T_treatment=='warmed')[,'iWUEgeMean'],
       pch=24, col='red', bg='red', cex=2)
points(subset(iWUEdf, month=='Apr' & T_treatment=='warmed' )[,'iWUEleafAvgCorrMean'],
       subset(iWUEdf, month=='Apr' & T_treatment=='warmed')[,'iWUEgeMean'],
       pch=25, col='red', bg='red', cex=2)
myChar <- c(21:25, 3)
for(i in 1:length(myMon)){
  Hmisc::errbar(x=subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'iWUEleafAvgCorrMean'],
                y=subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'iWUEgeMean'],
                yplus=subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'iWUEgeMean']+
                  subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'iWUEgeSE'],
                yminus=subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'iWUEgeMean']-
                  subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'iWUEgeSE'],
                pch=myChar[i], col='blue', bg='blue', errbar.col = 'black', add=T, cex=2)
  arrows(x0 = subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'iWUEleafAvgCorrMean'],
         y0 = subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'iWUEgeMean'],
         x1 = subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'iWUEleafAvgCorrMean']+
           subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'iWUEleafAvgCorrSE'],
         length = 0.03, angle = 90, code = 2)
  arrows(x0 = subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'iWUEleafAvgCorrMean'],
         y0 = subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'iWUEgeMean'],
         x1 = subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'iWUEleafAvgCorrMean']-
           subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'iWUEleafAvgCorrSE'],
         length = 0.03, angle = 90, code = 2)
}
for(i in 1:length(myMon)){
  Hmisc::errbar(x=subset(iWUEdf, month==myMon[i] & T_treatment=='warmed')[,'iWUEleafAvgCorrMean'],
                y=subset(iWUEdf, month==myMon[i] & T_treatment=='warmed')[,'iWUEgeMean'],
                yplus=subset(iWUEdf, month==myMon[i] & T_treatment=='warmed')[,'iWUEgeMean']+
                  subset(iWUEdf, month==myMon[i] & T_treatment=='warmed')[,'iWUEgeSE'],
                yminus=subset(iWUEdf, month==myMon[i] & T_treatment=='warmed')[,'iWUEgeMean']-
                  subset(iWUEdf, month==myMon[i] & T_treatment=='warmed')[,'iWUEgeSE'],
                pch=myChar[i], col='red', bg='red', errbar.col = 'black', add=T, cex=2)
  arrows(x0 = subset(iWUEdf, month==myMon[i] & T_treatment=='warmed')[,'iWUEleafAvgCorrMean'],
         y0 = subset(iWUEdf, month==myMon[i] & T_treatment=='warmed')[,'iWUEgeMean'],
         x1 = subset(iWUEdf, month==myMon[i] & T_treatment=='warmed')[,'iWUEleafAvgCorrMean']+
           subset(iWUEdf, month==myMon[i] & T_treatment=='warmed')[,'iWUEleafAvgCorrSE'],
         length = 0.03, angle = 90, code = 2)
  arrows(x0 = subset(iWUEdf, month==myMon[i] & T_treatment=='warmed')[,'iWUEleafAvgCorrMean'],
         y0 = subset(iWUEdf, month==myMon[i] & T_treatment=='warmed')[,'iWUEgeMean'],
         x1 = subset(iWUEdf, month==myMon[i] & T_treatment=='warmed')[,'iWUEleafAvgCorrMean']-
           subset(iWUEdf, month==myMon[i] & T_treatment=='warmed')[,'iWUEleafAvgCorrSE'],
         length = 0.03, angle = 90, code = 2)
}
plotrix::ablineclip(lm(iWUEgeMD ~ iWUEleafAvg_corrMD, data=phl), x1 = min(phl$iWUEleafAvg_corrMD, na.rm = T),
                    x2 = max(phl$iWUEleafAvg_corrMD, na.rm = T))
legend(x=20, y=170, legend=c('(b)'), text.font = 2, cex = 1.3, bty = 'n')
legend('bottomright', legend=c('Control', 'Drought'), pch=c(19, 21), col=c('darkgrey', 'black'), cex = 1.25, bty = 'n')
