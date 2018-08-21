library(HIEv)
setToken()
WTCraw <- downloadCSV(filename="WTC_Temp_CM_WTCFLUX_20130910-20140530_L0_V1.csv")
WTCraw$datetime <- ymd_hms(as.character(WTCraw$fluxesTable_Datetime))
source('scripts/selectDatesRawWTCflux.R')
#WTCraw <- subset(WTCraw, DateTime>=ymd_hm('2014-03-22 06:00') & DateTime<=ymd_hm('2014-03-23 22:00'))
# In a open system: Anet = (flow in x Cin) - (flow out x Cout)
# In the WTC: Anet = (flow in x Cin) - (flow out x Cout) - (Change in CO2 storage)
# Hence: Cin = [Anet + (flow out x Cout) + (Change CO2 storage)]/flow in
# assuming the flow addition by the injection is negligible
#WTCraw$Cin <- (WTCraw$FluxCO2+WTCraw$CO2out+WTCraw$DeltaCO2)*1000/(WTCraw$Air_in/22.4)
# also in the WTC: flow in x Cin_actual = Injection + (flow in x Cin_amb)
#renders exactly the same result
WTCrawShort$Cin <- (WTCrawShort$CO2in + WTCrawShort$CO2Injection)*1000/(WTCrawShort$Air_in/22.4)
WTCrawShort$datetimeFM <- HIEv::nearestTimeStep(WTCrawShort$datetime, nminutes = 15, align = 'ceiling')
source('scripts/chamber13C_calc.R')
deltaPaired <- merge(deltaPaired, WTCrawShort[,c('datetimeFM', 'chamber','Air_in','CO2in',
                                                 'CO2Injection','Cin')], by=c('chamber','datetimeFM'),all.x=T, all.y=F)
deltaPaired$CO2refWTC <- deltaPaired$CO2in*1000*22.4/deltaPaired$Air_in
deltaPaired[which(deltaPaired$totalCO2_ref>=500 & deltaPaired$CO2refWTC<=400),c('totalCO2_ref','Corrdel13C_Avg_ref')] <- NA
plot(deltaPaired$totalCO2_ref~deltaPaired$CO2refWTC, pch=19, ylim=c(375,575), xlim=c(375,575),
     ylab='TDL Amb CO2 (ppm)', xlab='WTC Amb CO2 (ppm)')
abline(1,1)
summary(lm(totalCO2_ref~CO2refWTC, data=deltaPaired))
# the reference ambient line for the TDL and the WTC system are very nicely correlated
# the d13C of the pure CO2 injected is: -33.4 permil
deltaPaired$del13C_theor_ref <- (deltaPaired$CO2in * deltaPaired$Corrdel13C_Avg_ref +
                                   deltaPaired$CO2Injection*(-33.4))/(deltaPaired$CO2in + deltaPaired$CO2Injection)
