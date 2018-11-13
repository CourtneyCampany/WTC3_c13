source('scripts/basicFunTEG.R')
source('scrips/canopy_gmes.R')
library(dplyr)
windows(16,8)
par(mfrow=c(4,6))
myPalEle <- c('red', 'darkorange1', 'deeppink', 'yellow', 'chocolate4', 'darkgoldenrod1')
myPalAmb <- c('blue', 'cornflowerblue', 'navyblue', 'cyan', 'darkturquoise', 'deepskyblue')
chambsA <- c(paste0('C0', seq(1, 9, 2)), 'C11')
chambsE <- c(paste0('C0', seq(2, 8, 2)), 'C10', 'C12')
camps <- c('Oct', 'Dec', 'Jan', 'Feb', 'Mar', 'Apr')
campsLA <- list()
for (i in 1:length(camps)){
  campsLA[[i]] <- list()
  for (j in 1:length(chambsA)){
    campsLA[[i]][[j]] <- subset(allPaired, condAlert == 'no' & month == camps[i] & chamber == chambsA[j])
  }
}
campsLE <- list()
for (i in 1:length(camps)){
  campsLE[[i]] <- list()
  for (j in 1:length(chambsE)){
    campsLE[[i]][[j]] <- subset(allPaired, condAlert == 'no' & month == camps[i] & chamber == chambsE[j])
  }
}
for (i in 1:length(camps)){
  par(mar=c(0,4,2,1))
  plot(campsLA[[i]][[1]][,'A_area']~campsLA[[i]][[1]][,'datetimeFM'], pch=19, col=myPalAmb[1],
       xlab = '', ylab = expression(italic(A)~(mu*mol~m^-2~s^-1)), main = campsLA[[i]][[1]][1,'month'], ylim=c(-4, 13),
       xlim=c(min(do.call(rbind, campsLA[[i]])[,'datetimeFM']),max(do.call(rbind, campsLA[[i]])[,'datetimeFM'])))
  #legend('bottomright', pch=19, legend=chambsA, col=myPalAmb, bty='n')
  for (j in 2:length(chambsA)){
    points(campsLA[[i]][[j]][,'A_area']~campsLA[[i]][[j]][,'datetimeFM'], pch=19, col=myPalAmb[j])
  }
}
for (i in 1:length(camps)){
  par(mar=c(0,4,2,1))
  plot(campsLE[[i]][[1]][,'A_area']~campsLE[[i]][[1]][,'datetimeFM'], pch=19, col=myPalEle[1],
       xlab = '', ylab = expression(italic(A)~(mu*mol~m^-2~s^-1)), ylim=c(-4, 13),
       xlim=c(min(do.call(rbind, campsLA[[i]])[,'datetimeFM']),max(do.call(rbind, campsLA[[i]])[,'datetimeFM'])))
  #legend('bottomright', pch=19, legend=chambsE, col=myPalEle, bty='n')
  for (j in 2:length(chambsE)){
    points(campsLE[[i]][[j]][,'A_area']~campsLE[[i]][[j]][,'datetimeFM'], pch=19, col=myPalEle[j])
  }
}

for (i in 1:length(camps)){
  par(mar=c(0,4,2,1))
  plot(campsLA[[i]][[1]][,'E_area']*1000~campsLA[[i]][[1]][,'datetimeFM'], pch=19, col=myPalAmb[1],
       xlab = '', ylab = expression(italic(E)~(mmol~m^-2~s^-1)), ylim=c(0, 2.2),
       xlim=c(min(do.call(rbind, campsLA[[i]])[,'datetimeFM']),max(do.call(rbind, campsLA[[i]])[,'datetimeFM'])))
  #legend('bottomright', pch=19, legend=chambsA, col=myPalAmb, bty='n')
  for (j in 2:length(chambsA)){
    points(campsLA[[i]][[j]][,'E_area']~campsLA[[i]][[j]][,'datetimeFM'], pch=19, col=myPalAmb[j])
  }
}
for (i in 1:length(camps)){
  par(mar=c(2,4,0,1))
  plot(campsLE[[i]][[1]][,'E_area']*1000~campsLE[[i]][[1]][,'datetimeFM'], pch=19, col=myPalEle[1],
       xlab = '', ylab = expression(italic(E)~(mmol~m^-2~s^-1)), ylim=c(0, 2.2),
       xlim=c(min(do.call(rbind, campsLA[[i]])[,'datetimeFM']),max(do.call(rbind, campsLA[[i]])[,'datetimeFM'])))
  #legend('bottomright', pch=19, legend=chambsE, col=myPalEle, bty='n')
  for (j in 2:length(chambsE)){
    points(campsLE[[i]][[j]][,'E_area']~campsLE[[i]][[j]][,'datetimeFM'], pch=19, col=myPalEle[j])
  }
}

gmesE <- list()
for (i in 1:length(chambsE)){
  gmesE[[i]] <- subset(allPaired, chamber==cambsE[i] & condAlert=='no')
}
plot(gmesA[,'A_area']~gmesA[,'dateTimeFM'])

gmesSumm <- tbl_df(allPaired) %>%
  group_by(chamber, month) %>%
  summarise(gsMean = mean.na(gsc_area), gsMax = max.na(gsc_area), gsMax2 = max2(gsc_area),
            gmMean = )



summ <- dplyr::summarise %>%
  

as.data.frame(summarise(group_by(svRing, Date,Ring),volRing=sum(volRing,na.rm=TRUE),
                        svMean=mean(ringSV,na.rm=TRUE),svMax=max(ringSV,na.rm=TRUE),rn=length(SWA.sum)))