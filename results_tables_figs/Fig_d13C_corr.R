source('scripts/iWUE_comparison_Ubierna.R')

myMon <- c('Oct','Dec','Jan','Feb','Mar')
myChar <- c(21:25)
palette(c('blue', 'red'))

windows(10,10)
par(mfrow=c(1,1), las=1, cex=1.25, mar=c(5, 5, 0.5, 0.5))
plot(subset(phl, month==myMon[1] & T_treatment=='ambient')[,'d13Cph']~
       subset(phl, month==myMon[1] & T_treatment=='ambient')[,'d13CAnet'], pch=myChar[1],
     col=scales::alpha('blue', 0.3), bg=scales::alpha('blue',0.3),  
     xlab=expression(delta^13*C[italic(A)*net]~('\211')), xlim=c(-33, -24),
     ylab=expression(delta^13*C[ph]~('\211')), ylim=c(-33, -24), cex.lab=1.3)
abline(0,1, lty=2)
plotrix::ablineclip(lm(d13Cph ~ d13CAnet, data=phl), x1 = min(phl$d13CAnet, na.rm = T),
                    x2 = -25, col='darkgrey', lwd=2)
plotrix::ablineclip(lm(d13Cph ~ d13CAnet, data=subset(phl, month!='Jan' & month!='Dec')), x1 = min(phl$d13CAnet, na.rm = T),
                    x2 = -25, lwd=2)
for(i in 2:length(myMon)){
  points(subset(phl, month==myMon[i] & T_treatment=='ambient' & W_treatment == 'control')[,'d13Cph']~
           subset(phl, month==myMon[i] & T_treatment=='ambient' & W_treatment == 'control')[,'d13CAnet'],
         pch=myChar[i], col=scales::alpha('blue', 0.3), bg=scales::alpha('blue',0.3))
}
for(i in 1:length(myMon)){
  points(subset(phl, month==myMon[i] & T_treatment=='warmed' & W_treatment == 'control')[,'d13Cph']~
           subset(phl, month==myMon[i] & T_treatment=='warmed' & W_treatment == 'control')[,'d13CAnet'],
         pch=myChar[i], col=scales::alpha('red', 0.3), bg=scales::alpha('red',0.3))
}
for(i in 4:length(myMon)){
  points(subset(phl, month==myMon[i] & T_treatment=='ambient' & W_treatment == 'drydown')[,'d13Cph']~
           subset(phl, month==myMon[i] & T_treatment=='ambient' & W_treatment == 'drydown')[,'d13CAnet'],
         pch=myChar[i], col=scales::alpha('blue', 0.3), bg=scales::alpha('white',0.3))
}
for(i in 4:length(myMon)){
  points(subset(phl, month==myMon[i] & T_treatment=='warmed' & W_treatment == 'drydown')[,'d13Cph']~
           subset(phl, month==myMon[i] & T_treatment=='warmed' & W_treatment == 'drydown')[,'d13CAnet'],
         pch=myChar[i], col=scales::alpha('red', 0.3), bg=scales::alpha('white',0.3))
}
iWUEdf <- as.data.frame(iWUEsumm)
for(i in 1:length(myMon)){
  Hmisc::errbar(x=subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'d13CAnetMean'],
                y=subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'d13CphMean'],
                yplus=subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'d13CphMean']+
                  subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'d13CphSE'],
                yminus=subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'d13CphMean']-
                  subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'d13CphSE'],
                pch=myChar[i], col='black', bg='blue', errbar.col = 'black', add=T, cex=2)
  arrows(x0 = subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'d13CAnetMean'],
         y0 = subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'d13CphMean'],
         x1 = subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'d13CAnetMean']+
           subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'d13CAnetSE'],
         length = 0.03, angle = 90, code = 2)
  arrows(x0 = subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'d13CAnetMean'],
         y0 = subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'d13CphMean'],
         x1 = subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'d13CAnetMean']-
           subset(iWUEdf, month==myMon[i] & T_treatment=='ambient')[,'d13CAnetSE'],
         length = 0.03, angle = 90, code = 2)
}
for(i in 1:length(myMon)){
  Hmisc::errbar(subset(iWUEdf, month==myMon[i] & T_treatment=='warmed')[,'d13CAnetMean'],
                subset(iWUEdf, month==myMon[i] & T_treatment=='warmed')[,'d13CphMean'],
                subset(iWUEdf, month==myMon[i] & T_treatment=='warmed')[,'d13CphMean']+
                  subset(iWUEdf, month==myMon[i] & T_treatment=='warmed')[,'d13CphSE'],
                subset(iWUEdf, month==myMon[i] & T_treatment=='warmed')[,'d13CphMean']-
                  subset(iWUEdf, month==myMon[i] & T_treatment=='warmed')[,'d13CphSE'],
                pch=myChar[i], col='black', bg='red', errbar.col = 'black', add=T, cex=2)
  arrows(x0 = subset(iWUEdf, month==myMon[i] & T_treatment=='warmed')[,'d13CAnetMean'],
         y0 = subset(iWUEdf, month==myMon[i] & T_treatment=='warmed')[,'d13CphMean'],
         x1 = subset(iWUEdf, month==myMon[i] & T_treatment=='warmed')[,'d13CAnetMean']+
           subset(iWUEdf, month==myMon[i] & T_treatment=='warmed')[,'d13CAnetSE'],
         length = 0.03, angle = 90, code = 2)
  arrows(x0 = subset(iWUEdf, month==myMon[i] & T_treatment=='warmed')[,'d13CAnetMean'],
         y0 = subset(iWUEdf, month==myMon[i] & T_treatment=='warmed')[,'d13CphMean'],
         x1 = subset(iWUEdf, month==myMon[i] & T_treatment=='warmed')[,'d13CAnetMean']-
           subset(iWUEdf, month==myMon[i] & T_treatment=='warmed')[,'d13CAnetSE'],
         length = 0.03, angle = 90, code = 2)
}
legend('topleft', legend=c('Amb', 'Warm', myMon), pch=c(19, 19, myChar), cex=1.2,
       col=c('blue', 'red', rep('black', 5)), bg=c('blue', 'red', rep('black', 5)), bty='n')
legend('bottomright', legend=c('Control', 'Drought'), pch=c(19, 21), cex=1.2, col=c('darkgrey', 'black'), bty='n')
