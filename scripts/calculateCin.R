# equation 3.8 in Campbell & Norman 'Introduction to environ. biophysics'
calcVapSat <- function(temp){0.61365 * exp(17.502 * temp/(240.97 + temp))}
# combining eq. 3.8 & 3.11 in Campbell & Norman
calcWaterP <- function(RH, temp){RH*0.01*0.611*exp(17.502*temp/(240.97 + temp))}
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
WTCraw$datetimeFM <- HIEv::nearestTimeStep(ymd_hms(as.character(WTCraw$DateTime)), nminutes = 15, align = 'ceiling')
WTCraw$chamber <- as.character(WTCraw$chamber)
# get cleaned data from the TDL with 15-min averages
# this script has additional lines with respect to the one Court Campany wrote
source('scripts/chamber13C_calc.R')
deltaPaired <- as.data.frame(dplyr::left_join(deltaPaired, WTCraw[,c('datetimeFM', 'chamber','FluxH2O','FluxCO2',
                                                          'Tair_al', 'RH_al','RHref_al','Patm',
                                                          'Taref_al','T_treatment','Water_treatment',
                                                          'PAR','CO2Injection','H2Oin','H2Oout',
                                                          'CO2in','CO2out','Air_in','Air_out','VPDair')],
                                     by=c('chamber','datetimeFM')))
deltaPaired <- deltaPaired[which(!is.na(deltaPaired$T_treatment)),]
# assuming the flow addition by the injection is negligible
# in the WTC: flow in x Cin_actual = Injection + (flow in x Cin_amb)
deltaPaired$Cin <- (deltaPaired$CO2in + deltaPaired$CO2Injection)*1000/(deltaPaired$Air_in/22.4)
# filter data suspicious for condensation
deltaPaired$waterP_kPa_air_inside <- calcWaterP(RH=deltaPaired$RH_al, temp=deltaPaired$Tair_al)
# Alternatively using eq. 14-20 in the LI-COR 6400 manual
# deltaPaired$waterP_kPa <- deltaPaired$H2Oout*22.4*deltaPaired$Patm/deltaPaired$Air_out
deltaPaired$dewPointInsideChamb <- calcDewPoint(deltaPaired$waterP_kPa_air_inside)
# calculate difference in water concentration between air flow in and out
deltaPaired$H2OmyFlux <- ((deltaPaired$H2Oout/deltaPaired$Air_out)-(deltaPaired$H2Oin/deltaPaired$Air_in))*22.4
deltaPaired$condAlert <- ifelse(deltaPaired$dewPointInsideChamb >= (deltaPaired$Taref_al-1.5) |
                                   deltaPaired$H2OmyFlux <= 0, 'yes', 'no')
# optional
# source('scripts/visualizeCondensation.R')
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
# calculate raw isotopic discrimination (no filter) eq. S4 in Stangl et al. (unpublished)
deltaPaired$DELTA <- (deltaPaired$del13C_theor_ref * deltaPaired$Cin -
                        deltaPaired$Corrdel13C_Avg * deltaPaired$CO2sampleWTC)/
  (deltaPaired$Cin - deltaPaired$CO2sampleWTC)
deltaPaired$DELTA2 <- (deltaPaired$del13C_theor_ref - deltaPaired$Corrdel13C_Avg)/(1 + deltaPaired$Corrdel13C_Avg)

rm(flux_files, flux_names, cham13format_func, delta_files, delta_FM, delta_FM_all,
   delta_FM_all2, dfRef, dfSample, flux_months, TDL, WTCraw, corrCO2amb)