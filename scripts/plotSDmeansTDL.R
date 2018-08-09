binded <- plyr::rbind.fill(delta_files)
binded$ID <- as.factor(paste0(binded$datetimeFM, '-', binded$line, '-', binded$chamber))
diag <- as.data.frame(dplyr::summarise(dplyr::group_by(binded, ID), conA=mean(CorrConcA_Avg),
                                       conAsd=sd(CorrConcA_Avg), conB=mean(CorrConcB_Avg),conBsd=sd(CorrConcB_Avg),
                                       d13C=mean(Corrdel13C_Avg), d13Csd=sd(Corrdel13C_Avg)))
diag <- merge(diag, binded[,c('ID','line','chamber','datetimeFM')], by='ID', all.x=T, all.y=F)
diag <- subset(diag, datetimeFM>=start & datetimeFM<=stop)
windows(12,8)
par(mfrow=c(3,2))
plot(subset(diag, line=='ref')[,'datetimeFM'], subset(diag, line=='ref')[,'conA'], ylim=c(350,599),
     ylab='Mean Conc A (ppm)', xlab='', pch=1, col=as.factor(subset(diag, line=='ref')[,'chamber']))
points(subset(diag, line=='samp')[,'datetimeFM'], subset(diag, line=='samp')[,'conA'],
       pch=19, col=as.factor(subset(diag, line=='samp')[,'chamber']))
plot(subset(diag, line=='ref')[,'datetimeFM'], subset(diag, line=='ref')[,'conAsd'], ylim=c(0,75),
     ylab='SD Conc A (ppm)', xlab='', pch=1, col=as.factor(subset(diag, line=='ref')[,'chamber']))
points(subset(diag, line=='samp')[,'datetimeFM'], subset(diag, line=='samp')[,'conAsd'],
       pch=19, col=as.factor(subset(diag, line=='samp')[,'chamber']))
plot(subset(diag, line=='ref')[,'datetimeFM'], subset(diag, line=='ref')[,'conB'], ylim=c(3.2,6.5),
     ylab='Mean Conc B (ppm)', xlab='', pch=1, col=as.factor(subset(diag, line=='ref')[,'chamber']))
points(subset(diag, line=='samp')[,'datetimeFM'], subset(diag, line=='samp')[,'conB'],
       pch=19, col=as.factor(subset(diag, line=='samp')[,'chamber']))
plot(subset(diag, line=='ref')[,'datetimeFM'], subset(diag, line=='ref')[,'conBsd'], ylim=c(0,2),
    ylab='SD Conc B (ppm)', xlab='', pch=1, col=as.factor(subset(diag, line=='ref')[,'chamber']))
points(subset(diag, line=='samp')[,'datetimeFM'], subset(diag, line=='samp')[,'conBsd'],
       pch=19, col=as.factor(subset(diag, line=='samp')[,'chamber']))
plot(subset(diag, line=='ref')[,'datetimeFM'], subset(diag, line=='ref')[,'d13C'], ylim=c(-16,-7),
     ylab='d13C (permil)', xlab='', pch=1, col=as.factor(subset(diag, line=='ref')[,'chamber']))
points(subset(diag, line=='samp')[,'datetimeFM'], subset(diag, line=='samp')[,'d13C'],
       pch=19, col=as.factor(subset(diag, line=='samp')[,'chamber']))
plot(subset(diag, line=='ref')[,'datetimeFM'], subset(diag, line=='ref')[,'d13Csd'], ylim=c(0,2.1),
     ylab='SD d13C (permil)', xlab='', pch=1, col=as.factor(subset(diag, line=='ref')[,'chamber']))
points(subset(diag, line=='samp')[,'datetimeFM'], subset(diag, line=='samp')[,'d13Csd'],
       pch=19, col=as.factor(subset(diag, line=='samp')[,'chamber']))

