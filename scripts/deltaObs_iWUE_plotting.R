campaigns <- c('Oct','Dec','Jan','Feb','Mar','Apr')

myPalEle <- c('red', 'darkorange1', 'deeppink', 'yellow', 'chocolate4', 'darkgoldenrod1')
myPalAmb <- c('blue', 'cornflowerblue', 'navyblue', 'cyan', 'darkturquoise', 'deepskyblue')
chambsA <- c(paste0('C0', seq(1, 9, 2)), 'C11')
chambsE <- c(paste0('C0', seq(2, 8, 2)), 'C10', 'C12')

campsA <- list()
for(i in 1:length(campaigns)){
  campsA[[i]] <- subset(allPaired, month==campaigns[i] & T_treatment=='ambient')
}
campsE <- list()
for(i in 1:length(campaigns)){
  campsE[[i]] <- subset(allPaired, month==campaigns[i] & T_treatment=='warmed')
}

# DELTAobs over time
plotDELTAobs <- function(x, pal, chamNames){
  k <- subset(x, chamber==chamNames[1])
  plot(k$DELTAobs*1000~k$Time, ylim=c(0,29), pch=3, col=pal[1],
       main=paste0(k$month[1], '-', k$T_treatment[1]), xlim=c(5,18),
       xlab='Time (h)', ylab=expression(Delta^13*C[obs]))
  points(subset(x, chamber==chamNames[1])[,'DELTAobsPhCont']~
           subset(x, chamber==chamNames[1])[,'Time'], pch=17, col=pal[1])
  points(subset(x, chamber==chamNames[1])[,'DELTAobsAvgLeafCont']~
           subset(x, chamber==chamNames[1])[,'Time'], pch=19, col=pal[1])
  legend('bottomleft', legend=chamNames, pch=19, col=pal, bty='n')
  for (i in 2:length(chamNames)){
    points(subset(x, chamber==chamNames[i])[,'DELTAobs']*1000~
             subset(x, chamber==chamNames[i])[,'Time'], pch=3, col=pal[i])
    points(subset(x, chamber==chamNames[i])[,'DELTAobsPhCont']~
                    subset(x, chamber==chamNames[i])[,'Time'], pch=17, col=pal[i])
    points(subset(x, chamber==chamNames[i])[,'DELTAobsAvgLeafCont']~
             subset(x, chamber==chamNames[i])[,'Time'], pch=19, col=pal[i])
  }
}
windows(15,8)
par(mfrow=c(2,6))
par(mar=c(2,2,2,2))
lapply(campsA, function(x) plotDELTAobs(x, pal=myPalAmb, chamNames=chambsA))
lapply(campsE, function(x) plotDELTAobs(x, pal=myPalEle, chamNames=chambsE))

# corrected and uncorrected iWUE over time
plotDELTAobs <- function(x, pal, chamNames){
  k <- subset(x, chamber==chamNames[1])
  plot(k$iWUE~k$Time, ylim=c(0,390), pch=19, col=pal[1],
       main=paste0(k$month[1], '-', k$T_treatment[1]), xlim=c(5,18),
       xlab='Time (h)', ylab='iWUE')
  lines(subset(x, chamber==chamNames[1])[,'iWUEph_corr']~
          subset(x, chamber==chamNames[1])[,'Time'], lwd=2.3, col=pal[1])
  lines(subset(x, chamber==chamNames[1])[,'iWUEph_uncorr']~
          subset(x, chamber==chamNames[1])[,'Time'], lwd=0.7, lty=3, col=pal[1])
  legend('topright', legend=chamNames, pch=19, col=pal, bty='n')
  for (i in 2:length(chamNames)){
    points(subset(x, chamber==chamNames[i])[,'iWUE']~
             subset(x, chamber==chamNames[i])[,'Time'], pch=19, col=pal[i])
    lines(subset(x, chamber==chamNames[i])[,'iWUEph_corr']~
            subset(x, chamber==chamNames[i])[,'Time'], lwd=2.3, col=pal[i])
    lines(subset(x, chamber==chamNames[i])[,'iWUEph_uncorr']~
            subset(x, chamber==chamNames[i])[,'Time'], lwd=0.7, lty=3, col=pal[i])
  }
}
windows(15,8)
par(mfrow=c(2,6))
par(mar=c(2,2,2,2))
lapply(campsA, function(x) plotDELTAobs(x, pal=myPalAmb, chamNames=chambsA))
lapply(campsE, function(x) plotDELTAobs(x, pal=myPalEle, chamNames=chambsE))

# corrected and uncorrected iWUE over time
plotDELTAobs <- function(x, pal, chamNames){
  k <- subset(x, chamber==chamNames[1])
  plot(k$iWUE~k$Time, ylim=c(0,390), pch=19, col=pal[1],
       main=paste0(k$month[1], '-', k$T_treatment[1]), xlim=c(5,18),
       xlab='Time (h)', ylab='iWUE')
  points(k$iWUEph_corr2~k$Time, pch=17, col=pal[1])
  lines(subset(x, chamber==chamNames[1])[,'iWUEph_corr']~
          subset(x, chamber==chamNames[1])[,'Time'], lwd=2.3, col=pal[1])
  lines(subset(x, chamber==chamNames[1])[,'iWUEph_uncorr']~
          subset(x, chamber==chamNames[1])[,'Time'], lwd=0.7, lty=3, col=pal[1])
  legend('topright', legend=chamNames, pch=19, col=pal, bty='n')
  for (i in 2:length(chamNames)){
    points(subset(x, chamber==chamNames[i])[,'iWUE']~
             subset(x, chamber==chamNames[i])[,'Time'], pch=19, col=pal[i])
    points(subset(x, chamber==chamNames[i])[,'iWUEph_corr2']~
             subset(x, chamber==chamNames[i])[,'Time'], pch=17, col=pal[i])
    lines(subset(x, chamber==chamNames[i])[,'iWUEph_corr']~
            subset(x, chamber==chamNames[i])[,'Time'], lwd=2.3, col=pal[i])
    lines(subset(x, chamber==chamNames[i])[,'iWUEph_uncorr']~
            subset(x, chamber==chamNames[i])[,'Time'], lwd=0.7, lty=3, col=pal[i])
  }
}
windows(15,8)
par(mfrow=c(2,6))
par(mar=c(2,2,2,2))
lapply(campsA, function(x) plotDELTAobs(x, pal=myPalAmb, chamNames=chambsA))
lapply(campsE, function(x) plotDELTAobs(x, pal=myPalEle, chamNames=chambsE))

plotDELTAobs <- function(x, pal, chamNames){
  k <- subset(x, chamber==chamNames[1])
  plot(k$iWUE~k$Time, ylim=c(0,390), pch=19, col=pal[1],
       main=paste0(k$month[1], '-', k$T_treatment[1]), xlim=c(5,18),
       xlab='Time (h)', ylab='iWUE')
  points(k$iWUEph_corr2~k$Time, pch=17, col=pal[1])
  legend('topright', legend=chamNames, pch=19, col=pal, bty='n')
  for (i in 2:length(chamNames)){
    points(subset(x, chamber==chamNames[i])[,'iWUE']~
             subset(x, chamber==chamNames[i])[,'Time'], pch=19, col=pal[i])
    points(subset(x, chamber==chamNames[i])[,'iWUEph_corr']~
             subset(x, chamber==chamNames[i])[,'Time'], pch=17, col=pal[i])
    lines(subset(x, chamber==chamNames[i])[,'iWUEph_corr']~
            subset(x, chamber==chamNames[i])[,'Time'], lwd=2.3, col=pal[i])
    lines(subset(x, chamber==chamNames[i])[,'iWUEph_uncorr']~
            subset(x, chamber==chamNames[i])[,'Time'], lwd=0.7, lty=3, col=pal[i])
  }
}
windows(15,8)
par(mfrow=c(2,6))
par(mar=c(2,2,2,2))
lapply(campsA, function(x) plotDELTAobs(x, pal=myPalAmb, chamNames=chambsA))
lapply(campsE, function(x) plotDELTAobs(x, pal=myPalEle, chamNames=chambsE))
