source('scripts/theGoodiWUEcorrV2.R')

myMon <- c('Oct','Dec','Jan','Feb','Mar')
myChar <- c(21:25)
palette(c('blue', 'red'))

windows(12,6)
par(mfrow=c(1,2), las=1, cex=1.25, mar=c(5, 5, 0.5, 0))
plot(subset(phl, month==myMon[1] & T_treatment=='ambient')[,'iWUEgeMD']~
       subset(phl, month==myMon[1] & T_treatment=='ambient')[,'iWUEleafAvg_corrMD'], pch=myChar[1],
     col='white', bg='white', axes = F,
     xlab=expression(iWUE[Delta~leaf]~'-'~italic(A)/italic(g)[m]~(mu*mol~mol^-1)),
     ylab=expression(iWUE[ge]~(mu*mol~mol^-1)),
     xlim=c(75, 220), ylim=c(75, 220), cex.lab=1.3)
axis(1, at=seq(50, 200, 50), labels = seq(50, 200, 50), las=1)
axis(2, at=seq(100, 250, 50), labels = seq(100, 250, 50), las=1)
box()
# for(i in 1:length(myMon)){
#   points(subset(phl, month==myMon[i] & T_treatment=='ambient' & W_treatment == 'control')[,'iWUEgeMD']~
#            subset(phl, month==myMon[i] & T_treatment=='ambient'& W_treatment == 'control')[,'iWUEleafAvg_corrMD'],
#          pch=myChar[i], col=scales::alpha('blue', 0.3), bg=scales::alpha('blue',0.3))
# }
# for(i in 1:length(myMon)){
#   points(subset(phl, month==myMon[i] & T_treatment=='warmed' & W_treatment == 'control')[,'iWUEgeMD']~
#            subset(phl, month==myMon[i] & T_treatment=='warmed' & W_treatment == 'control')[,'iWUEleafAvg_corrMD'],
#          pch=myChar[i], col=scales::alpha('red', 0.3), bg=scales::alpha('red',0.3))
# }
# for(i in 4:length(myMon)){
#   points(subset(phl, month==myMon[i] & T_treatment=='ambient' & W_treatment == 'drydown')[,'iWUEgeMD']~
#            subset(phl, month==myMon[i] & T_treatment=='ambient'& W_treatment == 'drydown')[,'iWUEleafAvg_corrMD'],
#          pch=myChar[i], col='blue', bg='white')
# }
# for(i in 4:length(myMon)){
#   points(subset(phl, month==myMon[i] & T_treatment=='warmed' & W_treatment == 'drydown')[,'iWUEgeMD']~
#            subset(phl, month==myMon[i] & T_treatment=='warmed' & W_treatment == 'drydown')[,'iWUEleafAvg_corrMD'],
#          pch=myChar[i], col='red', bg='white')
# }
iWUEdf <- as.data.frame(iWUEsumm)
for(i in 1:length(myMon)){
  Hmisc::errbar(x=subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'iWUEleafAvgCorrMean'],
                y=subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'iWUEgeMean'],
                yplus=subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'iWUEgeMean']+
                  subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'iWUEgeSE'],
                yminus=subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'iWUEgeMean']-
                  subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'iWUEgeSE'],
                pch=myChar[i], col='black', bg='blue', errbar.col = 'black', add=T, cex=1.6)
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
                pch=myChar[i], col='black', bg='red', errbar.col = 'black', add=T, cex=1.6)
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
plotrix::ablineclip(lm(iWUEgeMean ~ iWUEleafAvgCorrMean, data=iWUEdf),
                    x1 = min(iWUEdf$iWUEleafAvgCorrMean, na.rm = T),
                    x2 = max(iWUEdf$iWUEleafAvgCorrMean, na.rm = T), lwd=2)

legend('topleft', legend=myMon, pch=myChar, cex=1.3, bty='n')
legend('topright', legend=c(expression(Leaf~delta^13*C)), text.font = 2, cex = 1.3, bty = 'n', pch=NA)
legend('bottomright', legend=expression(italic(R)^2~'='~0.36), bty = 'n')

par(las=1, cex=1.25, mar=c(5, 0, 0.5, 5))
plot(subset(phl, month==myMon[1] & T_treatment=='ambient')[,'iWUEgeMD']~
       subset(phl, month==myMon[1] & T_treatment=='ambient')[,'iWUEph_corrMD'], pch=myChar[1],
     col='white', bg='white', axes = F,
     xlab=expression(iWUE[Delta~ph]~'-'~italic(A)/italic(g)[m]~(mu*mol~mol^-1)),
     xlim=c(75, 220), ylim=c(75, 220), cex.lab=1.3)
axis(1, at=seq(50, 200, 50), labels = seq(50, 200, 50), las=1)
axis(4, at=seq(100, 250, 50), labels = seq(100, 250, 50), las=1)
box()
# for(i in 1:length(myMon)){
#   points(subset(phl, month==myMon[i] & T_treatment=='ambient' & W_treatment=='control')[,'iWUEgeMD']~
#            subset(phl, month==myMon[i] & T_treatment=='ambient' & W_treatment=='control')[,'iWUEph_corrMD'],
#          pch=myChar[i], col=scales::alpha('blue', 0.3), bg=scales::alpha('blue',0.3))
# }
# for(i in 1:length(myMon)){
#   points(subset(phl, month==myMon[i] & T_treatment=='warmed' & W_treatment=='control')[,'iWUEgeMD']~
#            subset(phl, month==myMon[i] & T_treatment=='warmed' & W_treatment=='control')[,'iWUEph_corrMD'],
#          pch=myChar[i], col=scales::alpha('red', 0.3), bg=scales::alpha('red',0.3))
# }
# for(i in 4:length(myMon)){
#   points(subset(phl, month==myMon[i] & T_treatment=='ambient' & W_treatment=='drydown')[,'iWUEgeMD']~
#            subset(phl, month==myMon[i] & T_treatment=='ambient' & W_treatment=='drydown')[,'iWUEph_corrMD'],
#          pch=myChar[i], col=scales::alpha('blue', 0.3), bg=scales::alpha('white',0.3))
# }
# for(i in 4:length(myMon)){
#   points(subset(phl, month==myMon[i] & T_treatment=='warmed' & W_treatment=='drydown')[,'iWUEgeMD']~
#            subset(phl, month==myMon[i] & T_treatment=='warmed' & W_treatment=='drydown')[,'iWUEph_corrMD'],
#          pch=myChar[i], col=scales::alpha('red', 0.3), bg=scales::alpha('white',0.3))
# }
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
plotrix::ablineclip(lm(iWUEgeMean ~ iWUEphCorrMean, data=iWUEdf),
                    x1=min(iWUEdf$iWUEphCorrMean, na.rm=T), x2=max(iWUEdf$iWUEphCorrMean, na.rm=T), lwd=2)
legend('topright', legend=c(expression(Phloem~delta^13*C)), text.font = 2, cex = 1.3, bty = 'n')
legend('topleft', legend = c('Amb', 'Warm'), pch = c(19, 19),
       col = c('blue', 'red'), bty = 'n', cex = 1.3)
legend('bottomright', legend=expression(italic(R)^2~'='~0.76), bty = 'n')