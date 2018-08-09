#look at the raw values for WTC flux calculations
library(HIEv)
setToken()
rawWTC <- downloadCSV(filename="WTC_Temp_CM_WTCFLUX_20130910-20140530_L0_V1.csv")
rawWTC$DateTime <- ymd_hms(as.character(rawWTC$REFdataraw_Datetime))
rawWTCmarch <- subset(rawWTC, DateTime>=ymd_hm('2014-03-22 06:00') & DateTime<=ymd_hm('2014-03-23 22:00'))
plot(rawWTCmarch$Air_in~rawWTCmarch$DateTime, pch=19, col=as.factor(rawWTCmarch$chamber), ylab='Flow (L/s)', xlab='')
points(rawWTCmarch$Air_out~rawWTCmarch$DateTime, pch=1, col=as.factor(rawWTCmarch$chamber))
legend('bottomleft', pch=c(1,19), legend=c('out', 'in'), bty='n')
legend('bottomright', pch=c(rep.int(15, 12)), legend=c(paste0('Ch ', 1:12)), bty='n', col=c(1:12))
#it looks like there is something wrong with the flow in Chamber 4 over this measurement period
#the TDL measured on chamber 4 on Saturday (22 March)
#also flow in/out of the chamber are pretty close
plot((rawWTCmarch$Air_in-rawWTCmarch$Air_out)~rawWTCmarch$DateTime, pch=19, 
     col=as.factor(rawWTCmarch$chamber), ylab='Flow diff (L/s)', xlab='')
abline(0,0, lty=2)
legend('bottomright', pch=c(rep.int(19, 12)), legend=c(paste0('Ch ', 1:12)), bty='n', col=c(1:12))
#for now, we assume Flow in = Flow out (we need the flow for calculatin xi)