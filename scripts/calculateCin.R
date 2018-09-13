calcVapSat <- function(temp){0.61365 * exp(17.502 * temp/(240.97 + temp))}
# library(devtools)
# install_bitbucket("remkoduursma/HIEv")
library(HIEv)

setToken()
# download the file with the raw flow data
WTCraw <- downloadCSV(filename="WTC_TEMP_CM_WTCFLUX_20130910-20140530_L1_v1.csv")
#alternatively use a file from a local directory
#WTCraw <- read.csv(filename="WTC_TEMP_CM_WTCFLUX_20130910-20140530_L1_v1.csv")
WTCraw$datetime <- ymd_hms(as.character(WTCraw$DateTime))
#select the dates for the TDL measurements
source('scripts/selectDatesRawWTCflux.R')
# assuming the flow addition by the injection is negligible
# in the WTC: flow in x Cin_actual = Injection + (flow in x Cin_amb)
WTCrawShort$Cin <- (WTCrawShort$CO2in + WTCrawShort$CO2Injection)*1000/(WTCrawShort$Air_in/22.4)
WTCrawShort$datetimeFM <- HIEv::nearestTimeStep(WTCrawShort$datetime, nminutes = 15, align = 'ceiling')
WTCrawShort$chamber <- as.character(WTCrawShort$chamber)
# filter data suspicious for condensation
WTCrawShort$satVap <- calcVapSat(WTCrawShort$Taref_al)/101.3 #101.3 kPa is the standard atmospheric pressure
WTCrawShort$H2Oin_conc <- WTCrawShort$H2Oin*22.4/(WTCrawShort$Air_in)
WTCrawShort$condAlert <- ifelse(WTCrawShort$RH_al < WTCrawShort$RHref_al | WTCrawShort$H2Oout < WTCrawShort$H2Oin | 
                                WTCrawShort$H2Oin_con >= WTCrawShort$satVap ,'yes', 'no')
# get cleaned data from the TDL with 15-min averages
# this script has additional lines with respect to the one Court Campany wrote
source('scripts/chamber13C_calc.R')
deltaPaired <- merge(deltaPaired, WTCrawShort[,c('datetimeFM', 'chamber','FluxH2O','FluxCO2','condAlert','T_treatment','Water_treatment',
                                                 'PAR','CO2Injection','H2Oin','H2Oout','Cin','CO2in','CO2out','Air_in','Air_out','VPDair')],
                     by=c('chamber','datetimeFM'), all.x=T, all.y=F)
deltaPaired$CO2refWTC <- deltaPaired$CO2in*1000*22.4/deltaPaired$Air_in
deltaPaired$CO2sampleWTC <- deltaPaired$CO2out*1000*22.4/deltaPaired$Air_out
deltaPaired[which(deltaPaired$totalCO2_ref>=500 & deltaPaired$CO2refWTC<=400),c('totalCO2_ref','Corrdel13C_Avg_ref')] <- NA
plot(deltaPaired$CO2refWTC~deltaPaired$totalCO2_ref, pch=19, ylim=c(375,575), xlim=c(375,575),
     ylab='WTC Amb CO2 (ppm)', xlab='TDL Amb CO2 (ppm)')
abline(1,1)
corrCO2amb <- lm(deltaPaired$CO2refWTC~totalCO2_ref, data=deltaPaired)
legend('bottomright', legend=c(expression(R^2~0.86), 
                               paste0('Intercept:', round(corrCO2amb$coefficients[1], digits = 0), ' ppm'),
                               paste0('slope: ', round(corrCO2amb$coefficients[2], digits = 2))), bty='n')

# the reference ambient line for the TDL and the WTC system are very nicely correlated
# the TDL seems to be underestimating by 19% with respect to the WTC measurement
# I need to double check if this correction improves after taking into account the fact that
# the TDL is measuring [CO2] in dry air and the WTC system is not dry air

# convert concentrations measured by the TDL (in ppm) to flow rates (in mmol/s)
deltaPaired$totalCO2_ref_flow <- deltaPaired$totalCO2_ref * deltaPaired$Air_in/(1000*22.4)
# the d13C of the pure CO2 injected is: -31.9 permil (on 9 Oct 2013)
deltaPaired$del13C_theor_ref <- (deltaPaired$totalCO2_ref_flow * deltaPaired$Corrdel13C_Avg_ref +
                                   deltaPaired$CO2Injection*(-31.9))/(deltaPaired$totalCO2_ref_flow + deltaPaired$CO2Injection)
rm(delta_files, delta_FM, delta_FM_all, delta_FM_all2, dfRef, dfSample, flux_months, TDL, WTCraw, WTCrawShort)