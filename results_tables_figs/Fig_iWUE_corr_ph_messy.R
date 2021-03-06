source('scripts/theGoodiWUEcorrV2.R')

iWUEsumm <- dplyr::summarise(dplyr::group_by(phl, T_treatment, W_treatment, month),
                             iWUEgeMean=mean.na(iWUEgeMD), iWUEgeSE=s.err.na(iWUEgeMD),
                             iWUEgeCorrMean=mean.na(iWUEgeCorrMD), iWUEgeCorrSE=s.err.na(iWUEgeCorrMD),
                             iWUEphUncorrMean=mean.na(iWUEph_uncorrMD), iWUEphUncorrSE=s.err.na(iWUEph_uncorrMD),
                             iWUEphCorrMean=mean.na(iWUEph_corrMD), iWUEphCorrSE=s.err.na(iWUEph_corrMD),
                             iWUEphUncorr2Mean=mean.na(iWUEph_uncorrMDmyAv),
                             iWUEphUncorr2SE=s.err.na(iWUEph_uncorrMDmyAv),
                             iWUEphCorr2Mean=mean.na(iWUEph_corrMDmyAv), iWUEphCorr2SE=s.err.na(iWUEph_corrMDmyAv),
                             iWUEsunLeafCorrMean=mean.na(iWUEsunLeaf_corrMD),
                             iWUEsunLeafCorrSE=s.err.na(iWUEsunLeaf_corrMD),
                             iWUEshLeafCorrMean=mean.na(iWUEshLeaf_corrMD),
                             iWUEshLeafCorrSE=s.err.na(iWUEshLeaf_corrMD),
                             iWUEleafAvgUncorrMean=mean.na(iWUEleafAvg_uncorrMD),
                             iWUEleafAvgUncorrSE=s.err.na(iWUEleafAvg_uncorrMD),
                             iWUEleafAvgCorrMean=mean.na(iWUEleafAvg_corrMD),
                             iWUEleafAvgCorrSE=s.err.na(iWUEleafAvg_corrMD),
                             d13CphMean=mean.na(d13Cph), d13CphSE=s.err.na(d13Cph),
                             d13CAnetMean=mean.na(d13CAnet), d13CAnetSE=s.err.na(d13CAnet),
                             d13CleafAvgMean=mean.na(d13CleafAvg), d13CleafAvgSE=s.err.na(d13CleafAvg),
                             d13CsunLeafMean=mean.na(d13CsunLeaf), d13CsunLeafAvgSE=s.err.na(d13CsunLeaf),
                             d13CsunLeafMean=mean.na(d13CshLeaf), d13CsunLeafAvgSE=s.err.na(d13CshLeaf),
                             iWUEleafSunAvgCorrSunGmMean=mean.na(iWUEsunLeaf_corrSunGm),
                             iWUEleafSunAvgCorrAvg1Mean=mean.na(iWUEsunLeaf_corrAvg1),
                             iWUEleafSunAvgCorrAvg2Mean=mean.na(iWUEsunLeaf_corrAvg2),
                             iWUEleafShAvgCorrShGmHMean=mean.na(iWUEshLeaf_corrShGmH),
                             iWUEleafShAvgCorrShGmLMean=mean.na(iWUEshLeaf_corrShGmL),
                             iWUEleafShAvgCorrAvg1Mean=mean.na(iWUEshLeaf_corrAvg1),
                             iWUEleafShAvgCorrAvg2Mean=mean.na(iWUEshLeaf_corrAvg2),
                             iWUEleafAvgCorrSunGmMean=mean.na(iWUEleafAvg_corrSunGm),
                             iWUEleafAvgCorrAvg1Mean=mean.na(iWUEleafAvg_corrAvg1),
                             iWUEleafAvgCorrAvg1SE=s.err.na(iWUEleafAvg_corrAvg1),
                             iWUEleafAvgCorrAvg2Mean=mean.na(iWUEleafAvg_corrAvg2))

myMon <- c('Oct','Dec','Jan','Feb','Mar')
myChar <- c(21:25)
palette(c('blue', 'red'))

windows(12,6)
par(mfrow=c(1,2), las=1, cex=1.1, mar=c(5, 5, 0.5, 0))
plot(subset(phl, month==myMon[1] & T_treatment=='ambient')[,'iWUEgeMD']~
       subset(phl, month==myMon[1] & T_treatment=='ambient')[,'iWUEph_uncorrMD'], pch=myChar[1],
     col=scales::alpha('blue', 0.3), bg=scales::alpha('blue',0.3),  
     ylab=expression(iWUE[ge]~(mu*mol~mol^-1)), xlim=c(45, 210),
     xlab=expression(iWUE[Delta~ph]~(mu*mol~mol^-1)), ylim=c(85, 251), cex.lab=1.3)
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
for(i in 4:length(myMon)){
  Hmisc::errbar(x=subset(iWUEdf, month==myMon[i] & T_treatment=='ambient' & W_treatment=='drydown')[,'iWUEphUncorrMean'],
                y=subset(iWUEdf, month==myMon[i] & T_treatment=='ambient' & W_treatment=='drydown')[,'iWUEgeMean'],
                yplus=subset(iWUEdf, month==myMon[i] & T_treatment=='ambient' & W_treatment=='drydown')[,'iWUEgeMean']+
                  subset(iWUEdf, month==myMon[i] & T_treatment=='ambient' & W_treatment=='drydown')[,'iWUEgeSE'],
                yminus=subset(iWUEdf, month==myMon[i] & T_treatment=='ambient' & W_treatment=='drydown')[,'iWUEgeMean']-
                  subset(iWUEdf, month==myMon[i] & T_treatment=='ambient' & W_treatment=='drydown')[,'iWUEgeSE'],
                pch=myChar[i], col='blue', bg='white', errbar.col = 'black', add=T, cex=1.6)
  arrows(x0 = subset(iWUEdf, month==myMon[i] & T_treatment=='ambient' & W_treatment=='drydown')[,'iWUEphUncorrMean'],
         y0 = subset(iWUEdf, month==myMon[i] & T_treatment=='ambient' & W_treatment=='drydown')[,'iWUEgeMean'],
         x1 = subset(iWUEdf, month==myMon[i] & T_treatment=='ambient' & W_treatment=='drydown')[,'iWUEphUncorrMean']+
           subset(iWUEdf, month==myMon[i] & T_treatment=='ambient' & W_treatment=='drydown')[,'iWUEphUncorrSE'],
         length = 0.03, angle = 90, code = 2)
  arrows(x0 = subset(iWUEdf, month==myMon[i] & T_treatment=='ambient' & W_treatment=='drydown')[,'iWUEphUncorrMean'],
         y0 = subset(iWUEdf, month==myMon[i] & T_treatment=='ambient' & W_treatment=='drydown')[,'iWUEgeMean'],
         x1 = subset(iWUEdf, month==myMon[i] & T_treatment=='ambient' & W_treatment=='drydown')[,'iWUEphUncorrMean']-
           subset(iWUEdf, month==myMon[i] & T_treatment=='ambient' & W_treatment=='drydown')[,'iWUEphUncorrSE'],
         length = 0.03, angle = 90, code = 2)
}
for(i in 4:length(myMon)){
  Hmisc::errbar(x=subset(iWUEdf, month==myMon[i] & T_treatment=='warmed' & W_treatment=='drydown')[,'iWUEphUncorrMean'],
                y=subset(iWUEdf, month==myMon[i] & T_treatment=='warmed' & W_treatment=='drydown')[,'iWUEgeMean'],
                yplus=subset(iWUEdf, month==myMon[i] & T_treatment=='warmed' & W_treatment=='drydown')[,'iWUEgeMean']+
                  subset(iWUEdf, month==myMon[i] & T_treatment=='warmed' & W_treatment=='drydown')[,'iWUEgeSE'],
                yminus=subset(iWUEdf, month==myMon[i] & T_treatment=='warmed' & W_treatment=='drydown')[,'iWUEgeMean']-
                  subset(iWUEdf, month==myMon[i] & T_treatment=='warmed' & W_treatment=='drydown')[,'iWUEgeSE'],
                pch=myChar[i], col='red', bg='white', errbar.col = 'black', add=T, cex=1.6)
  arrows(x0 = subset(iWUEdf, month==myMon[i] & T_treatment=='warmed' & W_treatment=='drydown')[,'iWUEphUncorrMean'],
         y0 = subset(iWUEdf, month==myMon[i] & T_treatment=='warmed' & W_treatment=='drydown')[,'iWUEgeMean'],
         x1 = subset(iWUEdf, month==myMon[i] & T_treatment=='warmed' & W_treatment=='drydown')[,'iWUEphUncorrMean']+
           subset(iWUEdf, month==myMon[i] & T_treatment=='warmed' & W_treatment=='drydown')[,'iWUEphUncorrSE'],
         length = 0.03, angle = 90, code = 2)
  arrows(x0 = subset(iWUEdf, month==myMon[i] & T_treatment=='warmed' & W_treatment=='drydown')[,'iWUEphUncorrMean'],
         y0 = subset(iWUEdf, month==myMon[i] & T_treatment=='warmed' & W_treatment=='drydown')[,'iWUEgeMean'],
         x1 = subset(iWUEdf, month==myMon[i] & T_treatment=='warmed' & W_treatment=='drydown')[,'iWUEphUncorrMean']-
           subset(iWUEdf, month==myMon[i] & T_treatment=='warmed' & W_treatment=='drydown')[,'iWUEphUncorrSE'],
         length = 0.03, angle = 90, code = 2)
}
for(i in 1:length(myMon)){
  Hmisc::errbar(x=subset(iWUEdf, month==myMon[i] & T_treatment=='ambient' & W_treatment=='control')[,'iWUEphUncorrMean'],
                y=subset(iWUEdf, month==myMon[i] & T_treatment=='ambient' & W_treatment=='control')[,'iWUEgeMean'],
                yplus=subset(iWUEdf, month==myMon[i] & T_treatment=='ambient' & W_treatment=='control')[,'iWUEgeMean']+
                  subset(iWUEdf, month==myMon[i] & T_treatment=='ambient' & W_treatment=='control')[,'iWUEgeSE'],
                yminus=subset(iWUEdf, month==myMon[i] & T_treatment=='ambient' & W_treatment=='control')[,'iWUEgeMean']-
                  subset(iWUEdf, month==myMon[i] & T_treatment=='ambient' & W_treatment=='control')[,'iWUEgeSE'],
                pch=myChar[i], col='black', bg='blue', errbar.col = 'black', add=T, cex=1.6)
  arrows(x0 = subset(iWUEdf, month==myMon[i] & T_treatment=='ambient' & W_treatment=='control')[,'iWUEphUncorrMean'],
         y0 = subset(iWUEdf, month==myMon[i] & T_treatment=='ambient' & W_treatment=='control')[,'iWUEgeMean'],
         x1 = subset(iWUEdf, month==myMon[i] & T_treatment=='ambient' & W_treatment=='control')[,'iWUEphUncorrMean']+
           subset(iWUEdf, month==myMon[i] & T_treatment=='ambient' & W_treatment=='control')[,'iWUEphUncorrSE'],
         length = 0.03, angle = 90, code = 2)
  arrows(x0 = subset(iWUEdf, month==myMon[i] & T_treatment=='ambient' & W_treatment=='control')[,'iWUEphUncorrMean'],
         y0 = subset(iWUEdf, month==myMon[i] & T_treatment=='ambient' & W_treatment=='control')[,'iWUEgeMean'],
         x1 = subset(iWUEdf, month==myMon[i] & T_treatment=='ambient' & W_treatment=='control')[,'iWUEphUncorrMean']-
           subset(iWUEdf, month==myMon[i] & T_treatment=='ambient' & W_treatment=='control')[,'iWUEphUncorrSE'],
         length = 0.03, angle = 90, code = 2)
}
for(i in 1:length(myMon)){
  Hmisc::errbar(x=subset(iWUEdf, month==myMon[i] & T_treatment=='warmed' & W_treatment=='control')[,'iWUEphUncorrMean'],
                y=subset(iWUEdf, month==myMon[i] & T_treatment=='warmed' & W_treatment=='control')[,'iWUEgeMean'],
                yplus=subset(iWUEdf, month==myMon[i] & T_treatment=='warmed' & W_treatment=='control')[,'iWUEgeMean']+
                  subset(iWUEdf, month==myMon[i] & T_treatment=='warmed' & W_treatment=='control')[,'iWUEgeSE'],
                yminus=subset(iWUEdf, month==myMon[i] & T_treatment=='warmed' & W_treatment=='control')[,'iWUEgeMean']-
                  subset(iWUEdf, month==myMon[i] & T_treatment=='warmed' & W_treatment=='control')[,'iWUEgeSE'],
                pch=myChar[i], col='black', bg='red', errbar.col = 'black', add=T, cex=1.6)
  arrows(x0 = subset(iWUEdf, month==myMon[i] & T_treatment=='warmed' & W_treatment=='control')[,'iWUEphUncorrMean'],
         y0 = subset(iWUEdf, month==myMon[i] & T_treatment=='warmed' & W_treatment=='control')[,'iWUEgeMean'],
         x1 = subset(iWUEdf, month==myMon[i] & T_treatment=='warmed' & W_treatment=='control')[,'iWUEphUncorrMean']+
           subset(iWUEdf, month==myMon[i] & T_treatment=='warmed' & W_treatment=='control')[,'iWUEphUncorrSE'],
         length = 0.03, angle = 90, code = 2)
  arrows(x0 = subset(iWUEdf, month==myMon[i] & T_treatment=='warmed' & W_treatment=='control')[,'iWUEphUncorrMean'],
         y0 = subset(iWUEdf, month==myMon[i] & T_treatment=='warmed' & W_treatment=='control')[,'iWUEgeMean'],
         x1 = subset(iWUEdf, month==myMon[i] & T_treatment=='warmed' & W_treatment=='control')[,'iWUEphUncorrMean']-
           subset(iWUEdf, month==myMon[i] & T_treatment=='warmed' & W_treatment=='control')[,'iWUEphUncorrSE'],
         length = 0.03, angle = 90, code = 2)
}
legend('bottomleft', legend=c('Control', 'Drought', 'Ambient', 'Warm', myMon), pch=c(19, 21, 19, 19, myChar), cex=1.1,
       col=c('darkgrey', 'black', 'blue', 'red', rep('black', 5)), bty='n')
legend(x=25, y=260, legend=c('(a) Uncorrected'), text.font = 2, cex = 1.3, bty = 'n', pch=NA)

par(las=1, cex=1.1, mar=c(5, 0, 0.5, 5))
plot(subset(phl, month==myMon[1] & T_treatment=='ambient')[,'iWUEgeMD']~
       subset(phl, month==myMon[1] & T_treatment=='ambient')[,'iWUEph_corrMD'], pch=myChar[1],
     col=scales::alpha('blue', 0.3), bg=scales::alpha('blue',0.3), axes = F,
     xlab=expression(iWUE[Delta~ph]~'-'~italic(A)/italic(g)[m]~(mu*mol~mol^-1)),
     xlim=c(45, 210), ylim=c(85, 251), cex.lab=1.3)
axis(1, at=seq(50, 200, 50), labels = seq(50, 200, 50), las=1)
axis(4, at=seq(100, 250, 50), labels = seq(100, 250, 50), las=1)
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
         pch=myChar[i], col=scales::alpha('blue', 0.3), bg=scales::alpha('blue',0.3))
}
for(i in 4:length(myMon)){
  points(subset(phl, month==myMon[i] & T_treatment=='warmed' & W_treatment=='drydown')[,'iWUEgeMD']~
           subset(phl, month==myMon[i] & T_treatment=='warmed' & W_treatment=='drydown')[,'iWUEph_corrMD'],
         pch=myChar[i], col=scales::alpha('red', 0.3), bg=scales::alpha('red',0.3))
}
iWUEdf <- as.data.frame(iWUEsumm)
for(i in 4:length(myMon)){
  Hmisc::errbar(x=subset(iWUEdf, month==myMon[i] & T_treatment=='ambient' & W_treatment=='drydown')[,'iWUEphCorrMean'],
                y=subset(iWUEdf, month==myMon[i] & T_treatment=='ambient' & W_treatment=='drydown')[,'iWUEgeMean'],
                yplus=subset(iWUEdf, month==myMon[i] & T_treatment=='ambient' & W_treatment=='drydown')[,'iWUEgeMean']+
                  subset(iWUEdf, month==myMon[i] & T_treatment=='ambient' & W_treatment=='drydown')[,'iWUEgeSE'],
                yminus=subset(iWUEdf, month==myMon[i] & T_treatment=='ambient' & W_treatment=='drydown')[,'iWUEgeMean']-
                  subset(iWUEdf, month==myMon[i] & T_treatment=='ambient' & W_treatment=='drydown')[,'iWUEgeSE'],
                pch=myChar[i], col='blue', bg='white', errbar.col = 'black', add=T, cex=1.6)
  arrows(x0 = subset(iWUEdf, month==myMon[i] & T_treatment=='ambient' & W_treatment=='drydown')[,'iWUEphCorrMean'],
         y0 = subset(iWUEdf, month==myMon[i] & T_treatment=='ambient' & W_treatment=='drydown')[,'iWUEgeMean'],
         x1 = subset(iWUEdf, month==myMon[i] & T_treatment=='ambient' & W_treatment=='drydown')[,'iWUEphCorrMean']+
           subset(iWUEdf, month==myMon[i] & T_treatment=='ambient' & W_treatment=='drydown')[,'iWUEphCorrSE'],
         length = 0.03, angle = 90, code = 2)
  arrows(x0 = subset(iWUEdf, month==myMon[i] & T_treatment=='ambient' & W_treatment=='drydown')[,'iWUEphCorrMean'],
         y0 = subset(iWUEdf, month==myMon[i] & T_treatment=='ambient' & W_treatment=='drydown')[,'iWUEgeMean'],
         x1 = subset(iWUEdf, month==myMon[i] & T_treatment=='ambient' & W_treatment=='drydown')[,'iWUEphCorrMean']-
           subset(iWUEdf, month==myMon[i] & T_treatment=='ambient' & W_treatment=='drydown')[,'iWUEphCorrSE'],
         length = 0.03, angle = 90, code = 2)
}
for(i in 1:length(myMon)){
  Hmisc::errbar(x=subset(iWUEdf, month==myMon[i] & T_treatment=='warmed' & W_treatment=='drydown')[,'iWUEphCorrMean'],
                y=subset(iWUEdf, month==myMon[i] & T_treatment=='warmed' & W_treatment=='drydown')[,'iWUEgeMean'],
                yplus=subset(iWUEdf, month==myMon[i] & T_treatment=='warmed' & W_treatment=='drydown')[,'iWUEgeMean']+
                  subset(iWUEdf, month==myMon[i] & T_treatment=='warmed' & W_treatment=='drydown')[,'iWUEgeSE'],
                yminus=subset(iWUEdf, month==myMon[i] & T_treatment=='warmed' & W_treatment=='drydown')[,'iWUEgeMean']-
                  subset(iWUEdf, month==myMon[i] & T_treatment=='warmed' & W_treatment=='drydown')[,'iWUEgeSE'],
                pch=myChar[i], col='red', bg='white', errbar.col = 'black', add=T, cex=1.6)
  arrows(x0 = subset(iWUEdf, month==myMon[i] & T_treatment=='warmed' & W_treatment=='drydown')[,'iWUEphCorrMean'],
         y0 = subset(iWUEdf, month==myMon[i] & T_treatment=='warmed' & W_treatment=='drydown')[,'iWUEgeMean'],
         x1 = subset(iWUEdf, month==myMon[i] & T_treatment=='warmed' & W_treatment=='drydown')[,'iWUEphCorrMean']+
           subset(iWUEdf, month==myMon[i] & T_treatment=='warmed' & W_treatment=='drydown')[,'iWUEphCorrSE'],
         length = 0.03, angle = 90, code = 2)
  arrows(x0 = subset(iWUEdf, month==myMon[i] & T_treatment=='warmed' & W_treatment=='drydown')[,'iWUEphCorrMean'],
         y0 = subset(iWUEdf, month==myMon[i] & T_treatment=='warmed' & W_treatment=='drydown')[,'iWUEgeMean'],
         x1 = subset(iWUEdf, month==myMon[i] & T_treatment=='warmed' & W_treatment=='drydown')[,'iWUEphCorrMean']-
           subset(iWUEdf, month==myMon[i] & T_treatment=='warmed' & W_treatment=='drydown')[,'iWUEphCorrSE'],
         length = 0.03, angle = 90, code = 2)
}
for(i in 1:length(myMon)){
  Hmisc::errbar(x=subset(iWUEdf, month==myMon[i] & T_treatment=='ambient' & W_treatment=='control')[,'iWUEphCorrMean'],
                y=subset(iWUEdf, month==myMon[i] & T_treatment=='ambient' & W_treatment=='control')[,'iWUEgeMean'],
                yplus=subset(iWUEdf, month==myMon[i] & T_treatment=='ambient' & W_treatment=='control')[,'iWUEgeMean']+
                  subset(iWUEdf, month==myMon[i] & T_treatment=='ambient' & W_treatment=='control')[,'iWUEgeSE'],
                yminus=subset(iWUEdf, month==myMon[i] & T_treatment=='ambient' & W_treatment=='control')[,'iWUEgeMean']-
                  subset(iWUEdf, month==myMon[i] & T_treatment=='ambient' & W_treatment=='control')[,'iWUEgeSE'],
                pch=myChar[i], col='black', bg='blue', errbar.col = 'black', add=T, cex=1.6)
  arrows(x0 = subset(iWUEdf, month==myMon[i] & T_treatment=='ambient' & W_treatment=='control')[,'iWUEphCorrMean'],
         y0 = subset(iWUEdf, month==myMon[i] & T_treatment=='ambient' & W_treatment=='control')[,'iWUEgeMean'],
         x1 = subset(iWUEdf, month==myMon[i] & T_treatment=='ambient' & W_treatment=='control')[,'iWUEphCorrMean']+
           subset(iWUEdf, month==myMon[i] & T_treatment=='ambient' & W_treatment=='control')[,'iWUEphCorrSE'],
         length = 0.03, angle = 90, code = 2)
  arrows(x0 = subset(iWUEdf, month==myMon[i] & T_treatment=='ambient' & W_treatment=='control')[,'iWUEphCorrMean'],
         y0 = subset(iWUEdf, month==myMon[i] & T_treatment=='ambient' & W_treatment=='control')[,'iWUEgeMean'],
         x1 = subset(iWUEdf, month==myMon[i] & T_treatment=='ambient' & W_treatment=='control')[,'iWUEphCorrMean']-
           subset(iWUEdf, month==myMon[i] & T_treatment=='ambient' & W_treatment=='control')[,'iWUEphCorrSE'],
         length = 0.03, angle = 90, code = 2)
}
for(i in 1:length(myMon)){
  Hmisc::errbar(x=subset(iWUEdf, month==myMon[i] & T_treatment=='warmed' & W_treatment=='control')[,'iWUEphCorrMean'],
                y=subset(iWUEdf, month==myMon[i] & T_treatment=='warmed' & W_treatment=='control')[,'iWUEgeMean'],
                yplus=subset(iWUEdf, month==myMon[i] & T_treatment=='warmed' & W_treatment=='control')[,'iWUEgeMean']+
                  subset(iWUEdf, month==myMon[i] & T_treatment=='warmed' & W_treatment=='control')[,'iWUEgeSE'],
                yminus=subset(iWUEdf, month==myMon[i] & T_treatment=='warmed' & W_treatment=='control')[,'iWUEgeMean']-
                  subset(iWUEdf, month==myMon[i] & T_treatment=='warmed' & W_treatment=='control')[,'iWUEgeSE'],
                pch=myChar[i], col='black', bg='red', errbar.col = 'black', add=T, cex=1.6)
  arrows(x0 = subset(iWUEdf, month==myMon[i] & T_treatment=='warmed' & W_treatment=='control')[,'iWUEphCorrMean'],
         y0 = subset(iWUEdf, month==myMon[i] & T_treatment=='warmed' & W_treatment=='control')[,'iWUEgeMean'],
         x1 = subset(iWUEdf, month==myMon[i] & T_treatment=='warmed' & W_treatment=='control')[,'iWUEphCorrMean']+
           subset(iWUEdf, month==myMon[i] & T_treatment=='warmed' & W_treatment=='control')[,'iWUEphCorrSE'],
         length = 0.03, angle = 90, code = 2)
  arrows(x0 = subset(iWUEdf, month==myMon[i] & T_treatment=='warmed' & W_treatment=='control')[,'iWUEphCorrMean'],
         y0 = subset(iWUEdf, month==myMon[i] & T_treatment=='warmed' & W_treatment=='control')[,'iWUEgeMean'],
         x1 = subset(iWUEdf, month==myMon[i] & T_treatment=='warmed' & W_treatment=='control')[,'iWUEphCorrMean']-
           subset(iWUEdf, month==myMon[i] & T_treatment=='warmed' & W_treatment=='control')[,'iWUEphCorrSE'],
         length = 0.03, angle = 90, code = 2)
}
plotrix::ablineclip(lm(iWUEgeMD ~ iWUEph_corrMD, data=phl),
                    x1=min(phl$iWUEph_corrMD, na.rm=T), x2=max(phl$iWUEph_corrMD, na.rm=T))
legend(x=25, y=260, legend=c('(b) Corrected'), text.font = 2, cex = 1.3, bty = 'n')
