##Example fluxes figure

testcham <- read.csv("gmes_calc/march_ch9_ch12.csv")
  testcham$chamber <- as.factor(testcham$chamber)
  library(lubridate)
  testcham$datetimeFM <- lubridate::ymd_hms(testcham$datetimeFM,tz='UTC')

windows()
# png(filename = "flux_example.png", width = 8, height = 6, units = "in", res= 600)
par(cex.axis=1, cex.lab=1,las=1,mgp=c(3,1,0),mfrow=c(2,1)) 

par(mar=c(0,6,2,2))
plot(FluxCO2~datetimeFM,data=testcham[testcham$chamber ==12,], type='l', col="red", 
     lwd=2, xaxt='n',ylab=expression(atop(Chamber~CO[2]~flux,~~(m*mol~m^-2~s^-1))))
points(FluxCO2~datetimeFM,data=testcham[testcham$chamber ==9,], type='l', 
       col="cornflowerblue", lwd=2)
legend("topright", c("CH12-ET", "CH09-AT"), lty=1, col=c("red", "cornflowerblue"), 
       title="March 23",bty='n', inset=.02, lwd=2)

par(mar=c(3,6,0,2))
plot(del13_samp~datetimeFM,data=testcham[testcham$chamber ==9,], type='l', col="cornflowerblue", lwd=2, 
     xlab="", ylab=expression(atop(Chamber~flux,~{delta}^13*C~~('\211'))), ylim=c(-13.5, -7.5))
points(del13_samp~datetimeFM,data=testcham[testcham$chamber ==12,], type='l', col="red", lwd=2)
points(del13_ref~datetimeFM,data=testcham[testcham$chamber ==9,], type='l', col="cornflowerblue", lwd=2, lty=3)
points(del13_ref~datetimeFM,data=testcham[testcham$chamber ==12,], type='l', col="red", lwd=2, lty=3)

legend("topright", c("CH12-ET", "CH09-AT","reference", "sample" ), lty=c(1,1,3,1), 
       col=c("red", "cornflowerblue","black", "black"), bty='n', inset=.02, lwd=2)  

dev.copy2pdf(file="gmes_calc/flux_example.pdf")
dev.off()