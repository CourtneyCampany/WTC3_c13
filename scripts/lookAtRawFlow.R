#look at the raw values for WTC flux calculations
library(HIEv)
setToken()
WTCraw <- downloadCSV(filename="WTC_Temp_CM_WTCFLUX_20130910-20140530_L0_V1.csv")
WTCraw$DateTime <- ymd_hms(as.character(WTCraw$REFdataraw_Datetime))
plot(WTCrawmarch$Air_in~WTCrawmarch$DateTime, pch=19, col=as.factor(WTCrawmarch$chamber), ylab='Flow (L/s)', xlab='')
points(WTCrawmarch$Air_out~WTCrawmarch$DateTime, pch=1, col=as.factor(WTCrawmarch$chamber))
legend('bottomleft', pch=c(1,19), legend=c('out', 'in'), bty='n')
legend('bottomright', pch=c(rep.int(15, 12)), legend=c(paste0('Ch ', 1:12)), bty='n', col=c(1:12))
#it looks like there is something wrong with the flow in Chamber 4 over this measurement period
#the TDL measured on chamber 4 on Saturday (22 March)
#also flow in/out of the chamber are pretty close
plot((WTCrawmarch$Air_in-WTCrawmarch$Air_out)~WTCrawmarch$DateTime, pch=19, 
     col=as.factor(WTCrawmarch$chamber), ylab='Flow diff (L/s)', xlab='')
abline(0,0, lty=2)
legend('bottomright', pch=c(rep.int(19, 12)), legend=c(paste0('Ch ', 1:12)), bty='n', col=c(1:12))
#for now, we assume Flow in = Flow out (we need the flow for calculatin xi)