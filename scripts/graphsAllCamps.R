source('scripts/basicFunTEG.R')
source('scrips/canopy_gmes.R')
library(dplyr)

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

# code for plotting photosyntheis and trasnpiration per chamber and campaign
windows(16,8)
par(mfrow=c(4,6))
for (i in 1:length(camps)){
  par(mar=c(0,4,2,1))
  plot(campsLA[[i]][[1]][,'A_area']~campsLA[[i]][[1]][,'datetimeFM'], pch=19, col=myPalAmb[1],
       xlab = '', ylab = expression(italic(A)~(mu*mol~m^-2~s^-1)), main = campsLA[[i]][[1]][1,'month'], ylim=c(-4, 13),
       xlim=c(min(do.call(rbind, campsLA[[i]])[,'datetimeFM']),max(do.call(rbind, campsLA[[i]])[,'datetimeFM'])))
  for (j in 2:length(chambsA)){
    points(campsLA[[i]][[j]][,'A_area']~campsLA[[i]][[j]][,'datetimeFM'], pch=19, col=myPalAmb[j])
  }
}
for (i in 1:length(camps)){
  par(mar=c(0,4,0,1))
  plot(campsLE[[i]][[1]][,'A_area']~campsLE[[i]][[1]][,'datetimeFM'], pch=19, col=myPalEle[1],
       xlab = '', ylab = expression(italic(A)~(mu*mol~m^-2~s^-1)), ylim=c(-4, 13),
       xlim=c(min(do.call(rbind, campsLA[[i]])[,'datetimeFM']),max(do.call(rbind, campsLA[[i]])[,'datetimeFM'])))
  #legend('bottomright', pch=19, legend=chambsE, col=myPalEle, bty='n')
  for (j in 2:length(chambsE)){
    points(campsLE[[i]][[j]][,'A_area']~campsLE[[i]][[j]][,'datetimeFM'], pch=19, col=myPalEle[j])
  }
}

for (i in 1:length(camps)){
  par(mar=c(0,4,0,1))
  plot(campsLA[[i]][[1]][,'E_area']~campsLA[[i]][[1]][,'datetimeFM'], pch=19, col=myPalAmb[1],
       xlab = '', ylab = expression(italic(E)~(mmol~m^-2~s^-1)), ylim=c(0, 3),
       xlim=c(min(do.call(rbind, campsLA[[i]])[,'datetimeFM']),max(do.call(rbind, campsLA[[i]])[,'datetimeFM'])))
  for (j in 2:length(chambsA)){
    points(campsLA[[i]][[j]][,'E_area']~campsLA[[i]][[j]][,'datetimeFM'], pch=19, col=myPalAmb[j])
  }
}
for (i in 1:length(camps)){
  par(mar=c(3,4,0,1))
  plot(campsLE[[i]][[1]][,'E_area']~campsLE[[i]][[1]][,'datetimeFM'], pch=19, col=myPalEle[1],
       xlab = '', ylab = expression(italic(E)~(mmol~m^-2~s^-1)), ylim=c(0, 3.5),
       xlim=c(min(do.call(rbind, campsLA[[i]])[,'datetimeFM']),max(do.call(rbind, campsLA[[i]])[,'datetimeFM'])))
  for (j in 2:length(chambsE)){
    points(campsLE[[i]][[j]][,'E_area']~campsLE[[i]][[j]][,'datetimeFM'], pch=19, col=myPalEle[j])
  }
}

#plot gs and Ci per campaign and chamber
windows(16,8)
par(mfrow=c(4,6))
for (i in 1:length(camps)){
  par(mar=c(0,4,2,1))
  plot(campsLA[[i]][[1]][,'gsc_area']~campsLA[[i]][[1]][,'datetimeFM'], pch=19, col=myPalAmb[1],
       xlab = '', ylab = expression(italic(g)[sc]~(mol~m^-2~s^-1)), main = campsLA[[i]][[1]][1,'month'], ylim=c(0, 0.7),
       xlim=c(min(do.call(rbind, campsLA[[i]])[,'datetimeFM']),max(do.call(rbind, campsLA[[i]])[,'datetimeFM'])))
  for (j in 2:length(chambsA)){
    points(campsLA[[i]][[j]][,'gsc_area']~campsLA[[i]][[j]][,'datetimeFM'], pch=19, col=myPalAmb[j])
  }
}
for (i in 1:length(camps)){
  par(mar=c(0,4,0,1))
  plot(campsLE[[i]][[1]][,'gsc_area']~campsLE[[i]][[1]][,'datetimeFM'], pch=19, col=myPalEle[1],
       xlab = '', ylab = expression(italic(g)[sc]~(mol~m^-2~s^-1)), ylim=c(0, 0.7),
       xlim=c(min(do.call(rbind, campsLA[[i]])[,'datetimeFM']),max(do.call(rbind, campsLA[[i]])[,'datetimeFM'])))
   for (j in 2:length(chambsE)){
    points(campsLE[[i]][[j]][,'gsc_area']~campsLE[[i]][[j]][,'datetimeFM'], pch=19, col=myPalEle[j])
  }
}

for (i in 1:length(camps)){
  par(mar=c(0,4,0,1))
  plot(campsLA[[i]][[1]][,'Ci']~campsLA[[i]][[1]][,'datetimeFM'], pch=19, col=myPalAmb[1],
       xlab = '', ylab = expression(italic(C)[i]~(mu*mol~mol^-1)), ylim=c(5, 540),
       xlim=c(min(do.call(rbind, campsLA[[i]])[,'datetimeFM']),max(do.call(rbind, campsLA[[i]])[,'datetimeFM'])))
  for (j in 2:length(chambsA)){
    points(campsLA[[i]][[j]][,'Ci']~campsLA[[i]][[j]][,'datetimeFM'], pch=19, col=myPalAmb[j])
  }
}
for (i in 1:length(camps)){
  par(mar=c(3,4,0,1))
  plot(campsLE[[i]][[1]][,'Ci']~campsLE[[i]][[1]][,'datetimeFM'], pch=19, col=myPalEle[1],
       xlab = '', ylab = expression(italic(C)[i]~(mu*mol~mol^-1)), ylim=c(5, 540),
       xlim=c(min(do.call(rbind, campsLA[[i]])[,'datetimeFM']),max(do.call(rbind, campsLA[[i]])[,'datetimeFM'])))
    for (j in 2:length(chambsE)){
    points(campsLE[[i]][[j]][,'Ci']~campsLE[[i]][[j]][,'datetimeFM'], pch=19, col=myPalEle[j])
  }
}

#plot Ci/Ca and DELTAi per campaign and chamber
windows(16,8)
par(mfrow=c(4,6))
for (i in 1:length(camps)){
  par(mar=c(0,4,2,1))
  plot(campsLA[[i]][[1]][,'Ci.Ca']~campsLA[[i]][[1]][,'datetimeFM'], pch=19, col=myPalAmb[1],
       xlab = '', ylab = expression(italic(C)[i]/italic(C)[a]), main = campsLA[[i]][[1]][1,'month'], ylim=c(0, 1.1),
       xlim=c(min(do.call(rbind, campsLA[[i]])[,'datetimeFM']),max(do.call(rbind, campsLA[[i]])[,'datetimeFM'])))
  for (j in 2:length(chambsA)){
    points(campsLA[[i]][[j]][,'Ci.Ca']~campsLA[[i]][[j]][,'datetimeFM'], pch=19, col=myPalAmb[j])
  }
}
for (i in 1:length(camps)){
  par(mar=c(0,4,0,1))
  plot(campsLE[[i]][[1]][,'Ci.Ca']~campsLE[[i]][[1]][,'datetimeFM'], pch=19, col=myPalEle[1],
       xlab = '', ylab = expression(italic(C)[i]/italic(C)[a]), ylim=c(0, 1.1),
       xlim=c(min(do.call(rbind, campsLA[[i]])[,'datetimeFM']),max(do.call(rbind, campsLA[[i]])[,'datetimeFM'])))
  for (j in 2:length(chambsE)){
    points(campsLE[[i]][[j]][,'Ci.Ca']~campsLE[[i]][[j]][,'datetimeFM'], pch=19, col=myPalEle[j])
  }
}

for (i in 1:length(camps)){
  par(mar=c(0,4,0,1))
  plot(campsLA[[i]][[1]][,'DELTAi']~campsLA[[i]][[1]][,'datetimeFM'], pch=19, col=myPalAmb[1],
       xlab = '', ylab = expression(Delta[i]~('\u2030')), ylim=c(4, 30),
       xlim=c(min(do.call(rbind, campsLA[[i]])[,'datetimeFM']),max(do.call(rbind, campsLA[[i]])[,'datetimeFM'])))
  for (j in 2:length(chambsA)){
    points(campsLA[[i]][[j]][,'DELTAi']~campsLA[[i]][[j]][,'datetimeFM'], pch=19, col=myPalAmb[j])
  }
}
for (i in 1:length(camps)){
  par(mar=c(3,4,0,1))
  plot(campsLE[[i]][[1]][,'DELTAi']~campsLE[[i]][[1]][,'datetimeFM'], pch=19, col=myPalEle[1],
       xlab = '', ylab = expression(Delta[i]~('\u2030')), ylim=c(4, 30),
       xlim=c(min(do.call(rbind, campsLA[[i]])[,'datetimeFM']),max(do.call(rbind, campsLA[[i]])[,'datetimeFM'])))
  for (j in 2:length(chambsE)){
    points(campsLE[[i]][[j]][,'DELTAi']~campsLE[[i]][[j]][,'datetimeFM'], pch=19, col=myPalEle[j])
  }
}

#plot DELTAobs and gmes per campaign and chamber
windows(16,8)
par(mfrow=c(4,6))
for (i in 1:length(camps)){
  par(mar=c(0,4,2,1))
  plot(campsLA[[i]][[1]][,'DELTAobs']~campsLA[[i]][[1]][,'datetimeFM'], pch=19, col=myPalAmb[1],
       xlab = '', ylab = expression(Delta[obs]~('\u2030')), main = campsLA[[i]][[1]][1,'month'], ylim=c(0, 0.5),
       xlim=c(min(do.call(rbind, campsLA[[i]])[,'datetimeFM']),max(do.call(rbind, campsLA[[i]])[,'datetimeFM'])))
  for (j in 2:length(chambsA)){
    points(campsLA[[i]][[j]][,'Ci.Ca']~campsLA[[i]][[j]][,'datetimeFM'], pch=19, col=myPalAmb[j])
  }
}
for (i in 1:length(camps)){
  par(mar=c(0,4,0,1))
  plot(campsLE[[i]][[1]][,'DELTAobs']~campsLE[[i]][[1]][,'datetimeFM'], pch=19, col=myPalEle[1],
       xlab = '', ylab = expression(Delta[obs]~('\u2030')), ylim=c(0, 0.5),
       xlim=c(min(do.call(rbind, campsLA[[i]])[,'datetimeFM']),max(do.call(rbind, campsLA[[i]])[,'datetimeFM'])))
  for (j in 2:length(chambsE)){
    points(campsLE[[i]][[j]][,'DELTAobs']~campsLE[[i]][[j]][,'datetimeFM'], pch=19, col=myPalEle[j])
  }
}

for (i in 1:length(camps)){
  par(mar=c(0,4,0,1))
  plot(campsLA[[i]][[1]][,'gmes_area']~campsLA[[i]][[1]][,'datetimeFM'], pch=19, col=myPalAmb[1],
       xlab = '', ylab = expression(italic(g)[mes]~(mol~m^-2~s^-1)), ylim=c(0, 1),
       xlim=c(min(do.call(rbind, campsLA[[i]])[,'datetimeFM']),max(do.call(rbind, campsLA[[i]])[,'datetimeFM'])))
  for (j in 2:length(chambsA)){
    points(campsLA[[i]][[j]][,'gmes_area']~campsLA[[i]][[j]][,'datetimeFM'], pch=19, col=myPalAmb[j])
  }
}
for (i in 1:length(camps)){
  par(mar=c(3,4,0,1))
  plot(campsLE[[i]][[1]][,'gmes_area']~campsLE[[i]][[1]][,'datetimeFM'], pch=19, col=myPalEle[1],
       xlab = '', ylab = expression(italic(g)[mes]~(mol~m^-2~s^-1)), ylim=c(0, 1),
       xlim=c(min(do.call(rbind, campsLA[[i]])[,'datetimeFM']),max(do.call(rbind, campsLA[[i]])[,'datetimeFM'])))
  for (j in 2:length(chambsE)){
    points(campsLE[[i]][[j]][,'gmes_area']~campsLE[[i]][[j]][,'datetimeFM'], pch=19, col=myPalEle[j])
  }
}