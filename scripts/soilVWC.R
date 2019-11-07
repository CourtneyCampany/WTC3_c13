source('scripts/basicFunTEG.R')
library('HIEv')
trtkey <- read.csv('data/treatment_key2.csv')
veryBig <- rbindlist(lapply(as.list(paste0('data/WTC_TEMP_CM_WTCMET_data_L1_v1/',
                                       list.files('data/WTC_TEMP_CM_WTCMET_data_L1_v1/'))), read.csv))
veryBig$DateTime <- lubridate::ymd_hms(as.character(veryBig$DateTime))
veryBig$Date <- as.Date(veryBig$DateTime)
veryBig <- subset(veryBig, Date <= as.Date('2014-04-30') & Date >= as.Date('2013-10-01'))
soilVWCdaily <- dplyr::summarise(dplyr::group_by(veryBig, chamber, Date),
                                             VWC10=mean.na(VW_Avg.1.), VWC30=mean.na(VW_Avg.2.),
                                 VWCHL=mean.na(VW_Avg.3.), VWC100=mean.na(VW_Avg.4.))
soilVWCdaily <- dplyr::left_join(soilVWCdaily, trtkey, by = c('chamber'))
soilVWCtreat <- as.data.frame(dplyr::summarise(dplyr::group_by(soilVWCdaily, Date, T_treatment, Water_treatment, Date),
                                 VWC_10=mean.na(VWC10), VWC_10se=s.err.na(VWC10),
                                 VWC_30=mean.na(VWC30), VWC_30se=s.err.na(VWC30),
                                 VWC_HL=mean.na(VWCHL), VWC_HLse=s.err.na(VWCHL),
                                 VWC_100=mean.na(VWC100), VWC_100se=s.err.na(VWC100)))
wp <- read.csv('data/WTC_TEMP_CM_WATERPOTENTIAL-PREDAWN-MIDDAY_20130515-20140424_L2.csv')[,c('date','chamber','predawn')]
wp$Date <- dmy(as.character(wp$date))
wp <- merge(wp, trtkey, by=c('chamber'))
wp <- subset(wp, Date >= as.Date('2013-10-01') & Date <= as.Date('2014-04-30'))
wpTreat <- as.data.frame(dplyr::summarise(dplyr::group_by(wp, Date, T_treatment, Water_treatment),
                                          wp=mean.na(predawn*-1), wpSE=s.err.na(predawn*-1)))

windows(10,12)
par(mfrow=c(4, 1))
