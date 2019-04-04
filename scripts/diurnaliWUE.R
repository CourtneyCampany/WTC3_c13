campaigns <- c('Oct','Dec','Jan','Feb','Mar','Apr')
myPch <- c(15, 16, 17, 18, 19, 8)
myPalEle <- c('red', 'darkorange1', 'deeppink', 'yellow', 'chocolate4', 'darkgoldenrod1')
myPalAmb <- c('blue', 'cornflowerblue', 'navyblue', 'cyan', 'darkturquoise', 'deepskyblue')

# diurnal gmes
windows(15,8)
par(mfrow=c(2,2))
par(mar=c(7,7,2,2))

plotGmes <- function(x, campaigns, myLevel, myCol, myPch){
  k <- subset(x, month==campaigns[1] & T_treatment==myLevel)
  plot(k$gmes_area~k$Time, ylim=c(0,1.1), pch=myPch[1], col=myCol[1],
       main=k$T_treatment[1], xlim=c(5,19), cex.lab=1.8,
       xlab='Time (h)', ylab=expression(italic(g)[mes]~(mol~m^-2~s^-1)))
  legend('topleft', legend=campaigns, pch=myPch, col='black', bty='n')
  for (i in 2:length(campaigns)){
    points(subset(x, month==campaigns[i] & T_treatment==myLevel)[,'gmes_area']~
             subset(x, month==campaigns[i] & T_treatment==myLevel)[,'Time'], pch=myPch[i], col=myCol[i])
    }
}

plotGmes(allPaired, campaigns = campaigns, myLevel='ambient', myCol=myPalAmb, myPch = myPch)
plotGmes(allPaired, campaigns = campaigns, myLevel='warmed', myCol=myPalEle, myPch = myPch)

plotGmes <- function(x, campaigns, myLevel, myCol, myPch){
  k <- subset(x, month==campaigns[1] & T_treatment==myLevel)
  plot(log(k$gmes_area*1000)~k$Time, ylim=c(-1.3,7), pch=myPch[1], col=myCol[1],
       main=k$T_treatment[1], xlim=c(5,19), cex.lab=1.8,
       xlab='Time (h)', ylab=expression(Lg~(italic(g)[mes]~mmol~m^-2~s^-1)))
  legend('topleft', legend=campaigns, pch=myPch, col='black', bty='n')
  for (i in 2:length(campaigns)){
    points(log(subset(x, month==campaigns[i] & T_treatment==myLevel)[,'gmes_area']*1000)~
             subset(x, month==campaigns[i] & T_treatment==myLevel)[,'Time'], pch=myPch[i], col=myCol[i])
  }
}
plotGmes(allPaired, campaigns = campaigns, myLevel='ambient', myCol=myPalAmb, myPch = myPch)
plotGmes(allPaired, campaigns = campaigns, myLevel='warmed', myCol=myPalEle, myPch = myPch)

# diurnal iWUEge (Ca-Ci)
windows(15,8)
par(mfrow=c(2,2))
par(mar=c(7,7,2,2))

plotGmes <- function(x, campaigns, myLevel, myCol, myPch){
  k <- subset(x, month==campaigns[1] & T_treatment==myLevel)
  plot(k$iWUE~k$Time, ylim=c(0,305), pch=myPch[1], col=myCol[1],
       main=k$T_treatment[1], xlim=c(5,19), cex.lab=1.8,
       xlab='Time (h)', ylab=expression(italic(A)[net]/italic(g)[sc]~(mu*mol~mol^-1)))
  abline(k[which(!is.na(k$iWUEph_corrMD))[1],'iWUEph_corrMD'], 0, col=myCol[1])
  legend('topleft', legend=campaigns, pch=myPch, col=myCol, bty='n')
  for (i in 2:length(campaigns)){
    points(subset(x, month==campaigns[i] & T_treatment==myLevel)[,'iWUE']~
             subset(x, month==campaigns[i] & T_treatment==myLevel)[,'Time'], pch=myPch[i], col=myCol[i])
    abline(x[which(!is.na(x$iWUEph_corrMD))[1],'iWUEph_corrMD'], 0, col=myCol[i])
  }
}

plotGmes(allPaired[which(!is.na(allPaired$gmes_area)),], campaigns = campaigns, myLevel='ambient',
         myCol=myPalAmb, myPch = myPch)
plotGmes(allPaired[which(!is.na(allPaired$gmes_area)),], campaigns = campaigns, myLevel='warmed',
         myCol=myPalEle, myPch = myPch)

plotGmes <- function(x, campaigns, myLevel, myCol, myPch){
  k <- subset(x, month==campaigns[1] & T_treatment==myLevel)
  plot(k$iWUE~k$Time, ylim=c(0,305), pch=myPch[1], col=myCol,
       main=k$T_treatment[1], xlim=c(5,19), cex.lab=1.8,
       xlab='Time (h)', ylab=expression(italic(A)[net]/italic(g)[sc]~(mu*mol~mol^-1)))
  legend('topleft', legend=campaigns, pch=myPch, col='black', bty='n')
  for (i in 2:length(campaigns)){
    points(subset(x, month==campaigns[i] & T_treatment==myLevel)[,'iWUE']~
             subset(x, month==campaigns[i] & T_treatment==myLevel)[,'Time'], pch=myPch[i], col=myCol)
  }
}

plotGmes(allPaired[which(!is.na(allPaired$gmes_area)),], campaigns = campaigns, myLevel='ambient',
         myCol='blue', myPch = myPch)
plotGmes(allPaired[which(!is.na(allPaired$gmes_area)),], campaigns = campaigns, myLevel='warmed',
         myCol='red', myPch = myPch)
