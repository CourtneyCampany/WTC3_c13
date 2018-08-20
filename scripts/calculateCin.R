library(HIEv)
setToken()
rawWTC <- downloadCSV(filename="WTC_Temp_CM_WTCFLUX_20130910-20140530_L0_V1.csv")
rawWTC$DateTime <- ymd_hms(as.character(rawWTC$REFdataraw_Datetime))
rawWTCmarch <- subset(rawWTC, DateTime>=ymd_hm('2014-03-22 06:00') & DateTime<=ymd_hm('2014-03-23 22:00'))
# In a open system: Anet = (flow in x Cin) - (flow out x Cout)
# In the WTC: Anet = (flow in x Cin) - (flow out x Cout) - (Change in CO2 storage)
# Hence: Cin = [Anet + (flow out x Cout) + (Change CO2 storage)]/flow in
rawWTC$Cin <- (rawWTC$FluxCO2+rawWTC$CO2out+rawWTC$DeltaCO2)*1000/(rawWTC$Air_in/22.4)
# also in the WTC: flow in x Cin_actual = Injection + (flow in x Cin_amb)
#rawWTC$Cin <- (rawWTC$CO2Injection + rawWTC$CO2in)*1000/(rawWTC$Air_in/22.4)
#renders exactly the same result

