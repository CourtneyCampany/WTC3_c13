# equation 3.8 in Campbell & Norman 'Introduction to environ. biophysics'
calcVapSat <- function(temp){0.61365 * exp(17.502 * temp/(240.97 + temp))}
# equation 3.14 in Campbell & Norman 'Introduction to environ. biophysics'
calcDewPoint <- function(waterP){240.97 * log(waterP/0.611)/(17.502-log(waterP/0.611))}
# library(devtools)
# install_bitbucket("remkoduursma/HIEv")
library(HIEv)

setToken()
# download the file with the raw flow data
# there is something wrong with my HIEv account
#WTCraw <- downloadCSV(filename="WTC_TEMP_CM_WTCFLUX_20130910-20140530_L1_v1.csv")
#alternatively use a file from a local directory
WTCraw <- read.csv('data/WTC_TEMP_CM_WTCFLUX_20130910-20140530_L1_v1.csv')
WTCraw$datetime <- ymd_hms(as.character(WTCraw$DateTime))
#select the dates for the TDL measurements
source('scripts/selectDatesRawWTCflux.R')
# assuming the flow addition by the injection is negligible
# in the WTC: flow in x Cin_actual = Injection + (flow in x Cin_amb)
WTCrawShort$Cin <- (WTCrawShort$CO2in + WTCrawShort$CO2Injection)*1000/(WTCrawShort$Air_in/22.4)
WTCrawShort$datetimeFM <- HIEv::nearestTimeStep(WTCrawShort$datetime, nminutes = 15, align = 'ceiling')
WTCrawShort$chamber <- as.character(WTCrawShort$chamber)
# filter data suspicious for condensation
# calculate water vapor pressure inside the chamber in kPa, eq. 14-20 in the LI-COR 6400 manual
WTCrawShort$waterP_kPa <- WTCrawShort$H2Oout*22.4*WTCrawShort$Patm/WTCrawShort$Air_out
WTCrawShort$dewPointInsideChamb <- calcDewPoint(WTCrawShort$waterP_kPa)
# calculate difference in water concentration between air flow in and out
WTCrawShort$H2OmyFlux <- ((WTCrawShort$H2Oout/WTCrawShort$Air_out)-(WTCrawShort$H2Oin/WTCrawShort$Air_in))*22.4
WTCrawShort$condAlert <- ifelse(WTCrawShort$dewPointInsideChamb <= (WTCrawShort$Taref_al-0.5) |
                                WTCrawShort$H2OmyFlux <= 0, 'yes', 'no')
WTCrawShort$condAlert1 <- ifelse(WTCrawShort$RH_al <= WTCrawShort$RHref_al, 'yes', 'no')
WTCrawShort$condAlert2 <- ifelse(WTCrawShort$H2OmyFlux <= 0, 'yes', 'no')
WTCrawShort$condAlert3 <- ifelse(WTCrawShort$dewPointInsideChamb <= WTCrawShort$Taref_al, 'yes', 'no')
WTCrawShort$condAlert4 <- ifelse(WTCrawShort$dewPointInsideChamb <= (WTCrawShort$Taref_al-0.5), 'yes', 'no')
# get cleaned data from the TDL with 15-min averages
# this script has additional lines with respect to the one Court Campany wrote
source('scripts/chamber13C_calc.R')
deltaPaired <- merge(deltaPaired, WTCrawShort[,c('datetimeFM', 'chamber','FluxH2O','FluxCO2','Tair_al',
                                                 'RH_al','RHref_al',
                                                 'Taref_al','condAlert','T_treatment','Water_treatment',
                                                 'PAR','CO2Injection','H2Oin','H2Oout','Cin',
                                                 'CO2in','CO2out','Air_in','Air_out','VPDair')],
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
                                   deltaPaired$CO2Injection*(-31.9))/
  (deltaPaired$totalCO2_ref_flow + deltaPaired$CO2Injection)
rm(flux_files, flux_names, cham13format_func, delta_files, delta_FM, delta_FM_all,
   delta_FM_all2, dfRef, dfSample, flux_months, TDL, WTCraw, WTCrawShort, corrCO2amb)