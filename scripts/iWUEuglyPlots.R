library(dplyr)
shortGmes <- subset(allPaired, Time >= 10.5 & Time <= 13.5 & iWUE < 400)
middayGmes <- as.data.frame(summarise(group_by(shortGmes, month, chamber), iWUE=mean(iWUE, na.rm=T)))
middayGmes <- merge(middayGmes, phl, by=c('month','chamber'), all=T)
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
middayGmes <- merge(middayGmes, trtkey, by='chamber')
summary(aov(iWUE~month*T_treatment, data=middayGmes))

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
  boxplot(iWUE ~ T_treatment + method, data=subset(iWUEcompare, month==myMon[i]), main=myMon[i], ylim=c(0,300),
          cex.lab=1.7, axes=F)
  axis(1, at=c(1:4),labels=c('Amb Ph', 'Warm Ph', 'Amb GE', 'Warm GE'), cex=1.7)
  axis(2, at=c(seq(0, 300, 50)), labels=c(seq(0, 300, 50)), cex=1.7)
  box()
}
boxplot(iWUE ~ T_treatment, data=subset(iWUEcompare, month=='Apr' & method=='gasEx'), ylim=c(0,300),
        axes=F, main='Apr')
axis(1, at=c(1:2),labels=c('Amb GE', 'Warm GE'), cex=1.7)
axis(2, at=c(seq(0, 300, 50)), labels=c(seq(0, 300, 50)), cex=1.7)
box()

windows(14,8)
par(mfrow=c(1,1), mar=c(4,6,1,1))
with(subset(allPaired, midday=='yes'), boxplot(lgGmes~T_treatment*month, axes=F, cex.lab=1.7,
                                               ylab=expression(Log~(italic(g)[mes]~mol~m^-2~s^-1))))
axis(1, at=c(1:12), labels=c('A-Oct','W-Oct','A-Dec','W-Dec','A-Jan','W-Jan',
                             'A-Feb','W-Feb','A-Mar','W-Mar','A-Apr','W-Apr'))
axis(2, at=c(seq(0, 7, 1)), labels=c(seq(0, 7, 1)))
box()
car::Anova(lmer(lgGmes~T_treatment*month+(1|chamber2), data=subset(allPaired, midday=='yes')))
