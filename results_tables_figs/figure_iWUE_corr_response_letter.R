library(dplyr)
source('scripts/theGoodiWUEcorrV2.R')
gmesSumm <- dplyr::summarise(dplyr::group_by(setDT(subset(allPaired, midday == 'yes' & A_area > 0
                                                          & PAR >= 800 & condAlert == 'no')),
                                             chamber, month), gm = mean(gmes_area, na.rm = T),
                             Ci.Cc = mean(diff_Ci.Cc, na.rm = T))
leaf <- left_join(leafGmes, gmesSumm, by = c('chamber', 'month'))
leaf <- left_join(leaf, phl[, c('chamber', 'month', 'iWUEph_uncorrMD', 'iWUEph_corrMD')],
                  by = c('chamber', 'month'))
leaf <- left_join(leaf, trtkey, by = 'chamber')

leafS <- leaf %>%
  group_by(temp, month) %>%
  summarise_all(list(mean.na, s.err.na))

summary(lm(iWUE_Avg1_fn1 ~ iWUEph_uncorrMD_fn1, data = leafS))
summary(lm(iWUE_Avg1_fn1 ~ iWUEph_corrMD_fn1, data = leafS))

summary(lm(iWUE_Avg2_fn1 ~ iWUEph_uncorrMD_fn1, data = leafS))
summary(lm(iWUE_Avg2_fn1 ~ iWUEph_corrMD_fn1, data = leafS))

summary(lm(iWUE_sun_fn1 ~ iWUEph_uncorrMD_fn1, data = leafS))
summary(lm(iWUE_sun_fn1 ~ iWUEph_corrMD_fn1, data = leafS))

summary(lm(iWUE_shH_fn1 ~ iWUEph_uncorrMD_fn1, data = leafS))
summary(lm(iWUE_shH_fn1 ~ iWUEph_corrMD_fn1, data = leafS))

summary(lm(iWUE_shL_fn1 ~ iWUEph_uncorrMD_fn1, data = leafS))
summary(lm(iWUE_shL_fn1 ~ iWUEph_corrMD_fn1, data = leafS))

myMon <- c('Oct','Dec','Jan','Feb','Mar')
myChar <- c(21:25)
palette(c('blue', 'red'))

windows(12,6)
par(mfrow=c(1,2), las=1, cex=1.25, mar=c(5, 5, 0.5, 0))
plot(iWUE_Avg2_fn1 ~ iWUEph_uncorrMD_fn1, 
     data = subset(leafS, month==myMon[1] & temp=='ambient'),
     pch=19, col = 'white',
     ylab=expression(iWUE[ge-leaf]~(mu*mol~mol^-1)), xlim=c(70, 200),
     xlab=expression(iWUE[Delta~ph]~(mu*mol~mol^-1)), ylim=c(60, 130), cex.lab=1.3)
iWUEdf <- as.data.frame(leafS)
for(i in 1:length(myMon)){
  Hmisc::errbar(x=subset(iWUEdf, month==myMon[i] & temp=='ambient')[,'iWUEph_uncorrMD_fn1'],
                y=subset(iWUEdf, month==myMon[i] & temp=='ambient')[,'iWUE_Avg2_fn1'],
                yplus=subset(iWUEdf, month==myMon[i] & temp=='ambient')[,'iWUE_Avg2_fn1']+
                  subset(iWUEdf, month==myMon[i] & temp=='ambient')[,'iWUE_Avg2_fn2'],
                yminus=subset(iWUEdf, month==myMon[i] & temp=='ambient')[,'iWUE_Avg2_fn1']-
                  subset(iWUEdf, month==myMon[i] & temp=='ambient')[,'iWUE_Avg2_fn2'],
                pch=myChar[i], col='black', bg='blue', errbar.col = 'black', add=T, cex=1.6)
  arrows(x0 = subset(iWUEdf, month==myMon[i] & temp=='ambient')[,'iWUEph_uncorrMD_fn1'],
         y0 = subset(iWUEdf, month==myMon[i] & temp=='ambient')[,'iWUE_Avg2_fn1'],
         x1 = subset(iWUEdf, month==myMon[i] & temp=='ambient')[,'iWUEph_uncorrMD_fn1']+
           subset(iWUEdf, month==myMon[i] & temp=='ambient')[,'iWUEph_uncorrMD_fn2'],
         length = 0.03, angle = 90, code = 2)
  arrows(x0 = subset(iWUEdf, month==myMon[i] & temp=='ambient')[,'iWUEph_uncorrMD_fn1'],
         y0 = subset(iWUEdf, month==myMon[i] & temp=='ambient')[,'iWUE_Avg2_fn1'],
         x1 = subset(iWUEdf, month==myMon[i] & temp=='ambient')[,'iWUEph_uncorrMD_fn1']-
           subset(iWUEdf, month==myMon[i] & temp=='ambient')[,'iWUEph_uncorrMD_fn2'],
         length = 0.03, angle = 90, code = 2)
}

for(i in 1:length(myMon)){
  Hmisc::errbar(x=subset(iWUEdf, month==myMon[i] & temp=='elevated')[,'iWUEph_uncorrMD_fn1'],
                y=subset(iWUEdf, month==myMon[i] & temp=='elevated')[,'iWUE_Avg2_fn1'],
                yplus=subset(iWUEdf, month==myMon[i] & temp=='elevated')[,'iWUE_Avg2_fn1']+
                  subset(iWUEdf, month==myMon[i] & temp=='elevated')[,'iWUE_Avg2_fn2'],
                yminus=subset(iWUEdf, month==myMon[i] & temp=='elevated')[,'iWUE_Avg2_fn1']-
                  subset(iWUEdf, month==myMon[i] & temp=='elevated')[,'iWUE_Avg2_fn2'],
                pch=myChar[i], col='black', bg='red', errbar.col = 'black', add=T, cex=1.6)
  arrows(x0 = subset(iWUEdf, month==myMon[i] & temp=='elevated')[,'iWUEph_uncorrMD_fn1'],
         y0 = subset(iWUEdf, month==myMon[i] & temp=='elevated')[,'iWUE_Avg2_fn1'],
         x1 = subset(iWUEdf, month==myMon[i] & temp=='elevated')[,'iWUEph_uncorrMD_fn1']+
           subset(iWUEdf, month==myMon[i] & temp=='elevated')[,'iWUEph_uncorrMD_fn2'],
         length = 0.03, angle = 90, code = 2)
  arrows(x0 = subset(iWUEdf, month==myMon[i] & temp=='elevated')[,'iWUEph_uncorrMD_fn1'],
         y0 = subset(iWUEdf, month==myMon[i] & temp=='elevated')[,'iWUE_Avg2_fn1'],
         x1 = subset(iWUEdf, month==myMon[i] & temp=='elevated')[,'iWUEph_uncorrMD_fn1']-
           subset(iWUEdf, month==myMon[i] & temp=='elevated')[,'iWUEph_uncorrMD_fn2'],
         length = 0.03, angle = 90, code = 2)
}

legend('bottomleft', legend=myMon, pch=myChar, cex=1.3, bty='n')
legend('topleft', legend=c('(a) Uncorrected'), text.font = 2, cex = 1.3, bty = 'n', pch=NA)

par(las=1, cex=1.25, mar=c(5, 0, 0.5, 5))
plot(iWUE_Avg2_fn1 ~ iWUEph_corrMD_fn1, 
     data = subset(leafS, month==myMon[1] & temp=='ambient'),
     pch=19, col='white', axes = F,
     xlab=expression(iWUE[Delta~ph]~'-'~italic(A)/italic(g)[m]~(mu*mol~mol^-1)),
     xlim=c(70, 200), ylim=c(60, 130), cex.lab=1.3)
axis(1, at=seq(80, 200, 20), labels = seq(80, 200, 20), las=1)
axis(4, at=seq(60, 130, 10), labels = seq(60, 130, 10), las=1)
box()

for(i in 1:length(myMon)){
  Hmisc::errbar(x=subset(iWUEdf, month==myMon[i] & temp=='ambient')[,'iWUEph_corrMD_fn1'],
                y=subset(iWUEdf, month==myMon[i] & temp=='ambient')[,'iWUE_Avg2_fn1'],
                yplus=subset(iWUEdf, month==myMon[i] & temp=='ambient')[,'iWUE_Avg2_fn1']+
                  subset(iWUEdf, month==myMon[i] & temp=='ambient')[,'iWUE_Avg2_fn2'],
                yminus=subset(iWUEdf, month==myMon[i] & temp=='ambient')[,'iWUE_Avg2_fn1']-
                  subset(iWUEdf, month==myMon[i] & temp=='ambient')[,'iWUE_Avg2_fn2'],
                pch=myChar[i], col='black', bg='blue', errbar.col = 'black', add=T, cex=1.6)
  arrows(x0 = subset(iWUEdf, month==myMon[i] & temp=='ambient')[,'iWUEph_corrMD_fn1'],
         y0 = subset(iWUEdf, month==myMon[i] & temp=='ambient')[,'iWUE_Avg2_fn1'],
         x1 = subset(iWUEdf, month==myMon[i] & temp=='ambient')[,'iWUEph_corrMD_fn1']+
           subset(iWUEdf, month==myMon[i] & temp=='ambient')[,'iWUEph_corrMD_fn2'],
         length = 0.03, angle = 90, code = 2)
  arrows(x0 = subset(iWUEdf, month==myMon[i] & temp=='ambient')[,'iWUEph_corrMD_fn1'],
         y0 = subset(iWUEdf, month==myMon[i] & temp=='ambient')[,'iWUE_Avg2_fn1'],
         x1 = subset(iWUEdf, month==myMon[i] & temp=='ambient')[,'iWUEph_corrMD_fn1']-
           subset(iWUEdf, month==myMon[i] & temp=='ambient')[,'iWUEph_corrMD_fn2'],
         length = 0.03, angle = 90, code = 2)
}

for(i in 1:length(myMon)){
  Hmisc::errbar(x=subset(iWUEdf, month==myMon[i] & temp=='elevated')[,'iWUEph_corrMD_fn1'],
                y=subset(iWUEdf, month==myMon[i] & temp=='elevated')[,'iWUE_Avg2_fn1'],
                yplus=subset(iWUEdf, month==myMon[i] & temp=='elevated')[,'iWUE_Avg2_fn1']+
                  subset(iWUEdf, month==myMon[i] & temp=='elevated')[,'iWUE_Avg2_fn2'],
                yminus=subset(iWUEdf, month==myMon[i] & temp=='elevated')[,'iWUE_Avg2_fn1']-
                  subset(iWUEdf, month==myMon[i] & temp=='elevated')[,'iWUE_Avg2_fn2'],
                pch=myChar[i], col='black', bg='red', errbar.col = 'black', add=T, cex=1.6)
  arrows(x0 = subset(iWUEdf, month==myMon[i] & temp=='elevated')[,'iWUEph_corrMD_fn1'],
         y0 = subset(iWUEdf, month==myMon[i] & temp=='elevated')[,'iWUE_Avg2_fn1'],
         x1 = subset(iWUEdf, month==myMon[i] & temp=='elevated')[,'iWUEph_corrMD_fn1']+
           subset(iWUEdf, month==myMon[i] & temp=='elevated')[,'iWUEph_corrMD_fn2'],
         length = 0.03, angle = 90, code = 2)
  arrows(x0 = subset(iWUEdf, month==myMon[i] & temp=='elevated')[,'iWUEph_corrMD_fn1'],
         y0 = subset(iWUEdf, month==myMon[i] & temp=='elevated')[,'iWUE_Avg2_fn1'],
         x1 = subset(iWUEdf, month==myMon[i] & temp=='elevated')[,'iWUEph_corrMD_fn1']-
           subset(iWUEdf, month==myMon[i] & temp=='elevated')[,'iWUEph_corrMD_fn2'],
         length = 0.03, angle = 90, code = 2)
}

plotrix::ablineclip(lm(iWUE_Avg2_fn1 ~ iWUEph_corrMD_fn1, data=leafS),
                    x1=min(leafS$iWUEph_corrMD_fn1, na.rm=T), x2=max(leafS$iWUEph_corrMD_fn1, na.rm=T))
legend('topleft', legend=c('(b) Corrected'), text.font = 2, cex = 1.3, bty = 'n')
legend('topright', legend = c('Amb', 'Warm'), pch = 19, col = c('blue', 'red'), bty = 'n', cex = 1.3)
legend('bottomright', legend=c(expression(italic(R)^2~'='~0.54),
                               expression(italic(P)~'='~0.009)), bty = 'n')

