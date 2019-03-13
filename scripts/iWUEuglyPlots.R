shortGmes <- subset(allPaired, Time >= 10.5 & Time <= 13.5 & iWUE < 400)
middayGmes <- as.data.frame(summarise(group_by(shortGmes, month, chamber), iWUE=mean(iWUE, na.rm=T)))
middayGmes <- merge(middayGmes, phl, by=c('month','chamber'))
palette <- c('blue','red')
windows(8,8)
par(mar=c(4,6,2,2))
plot(middayGmes$iWUE~middayGmes$iWUEph, ylab=expression(Midday~iWUE[WTC]~(mu*mol~mol^-1)),
     xlab=expression(iWUE[ph]~(mu*mol~mol^-1)), pch=19, col=as.factor(middayGmes$temp))
modelo <- lm(middayGmes$iWUE~middayGmes$iWUEph)
abline(modelo)
legend('topright', pch=19, col=c('blue','red'), bty='n', legend=c('Amb','Warm'))
legend('bottomright', bty='n', legend=c(paste0('P = ', round(summary(modelo)$coefficients[8], 3)),
                                        paste0('R2 = ', round(summary(modelo)$r.squared, 2))))
uno <- middayGmes[,c('chamber','month','iWUE')]
uno$method <- 'gasEx' 
dos <- middayGmes[,c('chamber','month','iWUEph')]
dos$method <- 'd13Cph'
names(dos)[3] <- 'iWUE'
iWUEcompare <- rbind(uno, dos)
iWUEcompare <- merge(iWUEcompare, trtkey, by='chamber', all=T)
windows(12,8)
par(mfrow=c(2,3))
myMon <- c('Oct','Dec','Jan','Feb','Mar')
for (i in 1:length(myMon)){
  boxplot(iWUE ~ temp + method, data=subset(iWUEcompare, month==myMon[i]), main=myMon[i], ylim=c(0,300))
}
