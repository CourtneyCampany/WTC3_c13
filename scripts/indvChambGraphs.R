myPal <- palette(c('cornflowerblue','deeppink','black','darkgrey', 'darkolivegreen','darkorange1'))
campaigns <- c('Oct','Dec','Jan','Feb','Mar','Apr')

# gmes over time
plotGmesHour <- function(x){
  k <- subset(x, month==campaigns[1])
  plot(k$gmes_area~k$Time, ylim=c(0,0.31), pch=19, col=myPal[1],
       main=paste0(x$chamber[1],'-', x$T_treatment[2]), xlim=c(5,18),
       xlab='Time (h)', ylab=expression(italic(g)[mes]~mol~m^-2~s^-1))
  legend('topleft', legend=campaigns, pch=19, col=myPal, bty='n')
  #abline(0.3,0)
  for (i in 2:length(campaigns)){
    points(subset(x, month==campaigns[i])[,'gmes_area']~
             subset(x, month==campaigns[i])[,'Time'], pch=19, col=myPal[i])
  }
}
windows(12,8)
par(mfrow=c(3,4))
par(mar=c(2,2,2,2))
lapply(gmesL, plotGmesHour)

#gmes and temperature
plotGmesTemp <- function(x){
  k <- subset(x, month==campaigns[1])
  plot(k$gmes_area~k$Tair_al, ylim=c(0,0.31), pch=19, col=myPal[1],
       main=paste0(x$chamber[1],'-', x$T_treatment[2]), xlim=c(4,44),
       xlab=expression(italic(T)[air]~(degree*C)), ylab=expression(lg(italic(g)[mes]~mol~m^-2~s^-1)))
  legend('topleft', legend=campaigns, pch=19, col=myPal, bty='n')
  for (i in 2:length(campaigns)){
    points(subset(x, month==campaigns[i])[,'gmes_area']~
             subset(x, month==campaigns[i])[,'Tair_al'], pch=19, col=myPal[i])
  }
}
windows(12,8)
par(mfrow=c(3,4))
par(mar=c(2,2,2,2))
lapply(gmesL, plotGmesTemp)

#gmes and VPD
plotGmesVPD <- function(x){
  k <- subset(x, month==campaigns[1])
  plot(k$gmes_area~k$VPDair, ylim=c(0,0.31), pch=19, col=myPal[1],
       main=paste0(x$chamber[1],'-', x$T_treatment[2]), xlim=c(0,6),
       xlab=expression(italic(T)[air]~(degree*C)), ylab=expression(lg(italic(g)[mes]~mol~m^-2~s^-1)))
  legend('topright', legend=campaigns, pch=19, col=myPal, bty='n')
  for (i in 2:length(campaigns)){
    points(subset(x, month==campaigns[i])[,'gmes_area']~
             subset(x, month==campaigns[i])[,'VPDair'], pch=19, col=myPal[i])
  }
}
windows(12,8)
par(mfrow=c(3,4))
par(mar=c(2,2,2,2))
lapply(gmesL, plotGmesVPD)

# Anet and gmes
plotGmesAnet <- function(x){
  k <- subset(x, month==campaigns[1])
  plot(k$A_area~k$gmes_area, ylim=c(0,25), pch=19, col=myPal[1],
       main=paste0(x$chamber[1],'-', x$T_treatment[2]), xlim=c(0,0.31),
       ylab=expression(italic(A)[net]~(mu*mol~m^-2~s^-1)), xlab=expression(italic(g)[mes]~(mol~m^-2~s^-1)))
  legend('topleft', legend=campaigns, pch=19, col=myPal, bty='n')
  for (i in 2:length(campaigns)){
    points(subset(x, month==campaigns[i])[,'A_area']~
             subset(x, month==campaigns[i])[,'gmes_area'], pch=19, col=myPal[i])
  }
}
windows(12,8)
par(mfrow=c(3,4))
par(mar=c(2,2,2,2))
lapply(gmesL, plotGmesAnet)

# Anet vs gs
plotGsAnet <- function(x){
  k <- subset(x, month==campaigns[1])
  plot(k$A_area~k$gsc_area, ylim=c(0,25), pch=19, col=myPal[1],
       main=paste0(x$chamber[1],'-', x$T_treatment[2]), xlim=c(0,0.5),
       ylab=expression(italic(A)[net]~(mu*mol~m^-2~s^-1)), xlab=expression(italic(g)[sc]~(mol~m^-2~s^-1)))
  legend('topleft', legend=campaigns, pch=19, col=myPal, bty='n')
  for (i in 2:length(campaigns)){
    points(subset(x, month==campaigns[i])[,'A_area']~
             subset(x, month==campaigns[i])[,'gsc_area'], pch=19, col=myPal[i])
  }
}
windows(12,8)
par(mfrow=c(3,4))
par(mar=c(2,2,2,2))
lapply(gmesL, plotGsAnet)

# gmes vs gs
plotGmesGs <- function(x){
  k <- subset(x, month==campaigns[1])
  plot(k$gmes_area~k$gsc_area, ylim=c(0,0.31), pch=19, col=myPal[1],
       main=paste0(x$chamber[1],'-', x$T_treatment[2]), xlim=c(0,0.5),
       ylab=expression(italic(g)[mes]~(mol~m^-2~s^-1)), xlab=expression(italic(g)[sc]~(mol~m^-2~s^-1)))
  legend('topright', legend=campaigns, pch=19, col=myPal, bty='n')
  for (i in 2:length(campaigns)){
    points(subset(x, month==campaigns[i])[,'gmes_area']~
             subset(x, month==campaigns[i])[,'gsc_area'], pch=19, col=myPal[i])
  }
}
windows(12,8)
par(mfrow=c(3,4))
par(mar=c(2,2,2,2))
lapply(gmesL, plotGmesGs)
