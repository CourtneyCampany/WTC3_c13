source('scripts/canopy_gmes.R')
march <- subset(allPaired, datetimeFM>=lubridate::ymd_hm('2014-03-22 06:00') & 
                  datetimeFM<=lubridate::ymd_hm('2014-03-23 22:00'))
march <- doBy::orderBy(~datetimeFM, march)

windows(9,6)
par(cex.axis=1, cex.lab=1,las=1,mgp=c(3,1,0),mfrow=c(2,3)) 
par(mar=c(3,6,2,2))
plot(FluxCO2~datetimeFM,data=march[march$chamber =='C12',], type='l', col="red", 
     lwd=2, xaxt='n',ylab=expression(atop(Chamber~CO[2]~flux~~(m*mol~s^-1))))
lines(FluxCO2~datetimeFM,data=march[march$chamber =="C09",], type='l', 
       col="cornflowerblue", lwd=2)
legend("topright", c("CH12-ET", "CH09-AT"), lty=1, col=c("red", "cornflowerblue"), 
       title="March 23",bty='n', inset=.02, lwd=2)
plot(FluxH2O~datetimeFM,data=march[march$chamber =='C12',], type='l', col="red", 
     lwd=2, xaxt='n',ylab=expression(atop(Chamber~H[2]*O~flux~~(mol~s^-1))))
lines(FluxH2O~datetimeFM,data=march[march$chamber =="C09",], type='l', 
      col="cornflowerblue", lwd=2)

plot(Ci~datetimeFM,data=march[march$chamber =='C12',], type='l', col="red", 
     lwd=2, xaxt='n',ylab=expression(atop(Chamber~italic(C)[i]~~(mu*mol~mol^-1))))
lines(Ci~datetimeFM,data=march[march$chamber =="C09",], type='l', 
      col="cornflowerblue", lwd=2)
lines(totalCO2~datetimeFM,data=march[march$chamber =="C12",], type='l', 
      col="red", lwd=2, lty=2)
lines(totalCO2~datetimeFM,data=march[march$chamber =="C09",], type='l', 
      col="cornflowerblue", lwd=2, lty=2)
legend('bottomright', c("CH12-ET", "CH09-AT", 'Ca', 'Ci'), lty=c(1,1,1,2), lwd=2,
       col=c("red", "cornflowerblue", 'black', 'black'), bty='n', inset=.02)

par(mar=c(3,6,2,2))
plot(Corrdel13C_Avg~datetimeFM,data=march[march$chamber =='C12',], type='l', col="red", 
     lwd=2, xaxt='n', ylab=expression(atop(Chamber~{delta}^13*C~~('\211'))), ylim=c(-14,-8))
lines(Corrdel13C_Avg_ref~datetimeFM,data=march[march$chamber =="C12",], type='l', lty=2,
      col="cornflowerblue", lwd=2)
lines(Corrdel13C_Avg~datetimeFM,data=march[march$chamber =="C09",], type='l', 
      col="cornflowerblue", lwd=2)
lines(Corrdel13C_Avg_ref~datetimeFM,data=march[march$chamber =="C09",], type='l', lty=2,
      col="cornflowerblue", lwd=2)
legend("topright", c("CH12-ET", "CH09-AT","reference", "sample" ), lty=c(1,1,3,1), 
       col=c("red", "cornflowerblue","black", "black"), bty='n', inset=.02, lwd=2)
plot(DELTAobs~datetimeFM,data=march[march$chamber =='C12',], type='l', col="red", 
     lwd=2, xaxt='n', ylab=expression(atop(Chamber~{delta}^13*C~~('\211'))), ylim=c(-2,40))
lines(DELTAi~datetimeFM,data=march[march$chamber =="C12",], type='l', lty=2,
      col="cornflowerblue", lwd=2)
lines(DELTAobs~datetimeFM,data=march[march$chamber =="C09",], type='l', 
      col="cornflowerblue", lwd=2)
lines(DELTAi~datetimeFM,data=march[march$chamber =="C09",], type='l', lty=2,
      col="cornflowerblue", lwd=2)
legend("topright", c("CH12-ET", "CH09-AT","reference", "sample" ), lty=c(1,1,3,1), 
       col=c("red", "cornflowerblue","black", "black"), bty='n', inset=.02, lwd=2)



par(mar=c(3,6,0,2))
plot(del13_samp~datetimeFM,data=march[march$chamber =='C09',], type='l', col="cornflowerblue", lwd=2, 
     xlab="", ylab=expression(atop(Chamber~flux,~{delta}^13*C~~('\211'))), ylim=c(-13.5, -7.5))
points(del13_samp~datetimeFM,data=testcham[testcham$chamber ==12,], type='l', col="red", lwd=2)
points(del13_ref~datetimeFM,data=testcham[testcham$chamber ==9,], type='l', col="cornflowerblue", lwd=2, lty=3)
points(del13_ref~datetimeFM,data=testcham[testcham$chamber ==12,], type='l', col="red", lwd=2, lty=3)

legend("topright", c("CH12-ET", "CH09-AT","reference", "sample" ), lty=c(1,1,3,1), 
       col=c("red", "cornflowerblue","black", "black"), bty='n', inset=.02, lwd=2)  

dev.copy2pdf(file="gmes_calc/flux_example.pdf")
dev.off()