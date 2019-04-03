campaigns <- c('Oct','Dec','Jan','Feb','Mar','Apr')

myPalEle <- c('red', 'darkorange1', 'deeppink', 'yellow', 'chocolate4', 'darkgoldenrod1')
myPalAmb <- c('blue', 'cornflowerblue', 'navyblue', 'cyan', 'darkturquoise', 'deepskyblue')

campsA <- list()
for(i in 1:length(campaigns)){
  campsA[[i]] <- subset(allPaired, month==campaigns[i] & T_treatment=='ambient')
}
campsE <- list()
for(i in 1:length(campaigns)){
  campsE[[i]] <- subset(allPaired, month==campaigns[i] & T_treatment=='warmed')
}
myPch <- c(15, 16, 17, 18, 19, 8)

# diurnal gmes
windows(15,8)
par(mfrow=c(2,2))
par(mar=c(7,7,2,2))

plotGmes <- function(x, campaigns, myLevel, myCol, myPch){
  k <- subset(x, month==campaigns[1] & T_treatment==myLevel)
  plot(k$gmes_area~k$Time, ylim=c(0,1.1), pch=myPch[1], col=myCol,
       main=k$T_treatment[1], xlim=c(5,19), cex.lab=1.8,
       xlab='Time (h)', ylab=expression(italic(g)[mes]~(mol~m^-2~s^-1)))
  legend('topleft', legend=campaigns, pch=myPch, col='black', bty='n')
  for (i in 2:length(campaigns)){
    points(subset(x, month==campaigns[i] & T_treatment==myLevel)[,'gmes_area']~
             subset(x, month==campaigns[i] & T_treatment==myLevel)[,'Time'], pch=myPch[i], col=myCol)
    }
}

plotGmes(allPaired, campaigns = campaigns, myLevel='ambient', myCol='blue', myPch = myPch)
plotGmes(allPaired, campaigns = campaigns, myLevel='warmed', myCol='red', myPch = myPch)

plotGmes <- function(x, campaigns, myLevel, myCol, myPch){
  k <- subset(x, month==campaigns[1] & T_treatment==myLevel)
  plot(log(k$gmes_area*1000)~k$Time, ylim=c(-1.3,7), pch=myPch[1], col=myCol,
       main=k$T_treatment[1], xlim=c(5,19), cex.lab=1.8,
       xlab='Time (h)', ylab=expression(Lg~(italic(g)[mes]~mmol~m^-2~s^-1)))
  legend('topleft', legend=campaigns, pch=myPch, col='black', bty='n')
  for (i in 2:length(campaigns)){
    points(log(subset(x, month==campaigns[i] & T_treatment==myLevel)[,'gmes_area']*1000)~
             subset(x, month==campaigns[i] & T_treatment==myLevel)[,'Time'], pch=myPch[i], col=myCol)
  }
}
plotGmes(allPaired, campaigns = campaigns, myLevel='ambient', myCol='blue', myPch = myPch)
plotGmes(allPaired, campaigns = campaigns, myLevel='warmed', myCol='red', myPch = myPch)

# diurnal iWUEge (Ca-Ci)
windows(15,8)
par(mfrow=c(1,2))
par(mar=c(7,7,2,2))

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
