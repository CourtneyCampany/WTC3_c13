source('scripts/basicFunTEG.R')
trtkey <- read.csv('data/treatment_key.csv')
trtkey$Date <- as.Date(as.character(trtkey$Date))
veryBig <- rbindlist(lapply(as.list(paste0('data/WTC_TEMP_CM_WTCMET_data_L1_v1/',
                                       list.files('data/WTC_TEMP_CM_WTCMET_data_L1_v1/'))), read.csv))
veryBig$DateTime <- lubridate::ymd_hms(as.character(veryBig$DateTime))
veryBig$Date <- as.Date(veryBig$DateTime)
veryBig <- subset(veryBig, Date <= as.Date('2014-04-30') & Date >= as.Date('2013-10-01'))
soilVWCdaily <- dplyr::summarise(dplyr::group_by(veryBig, chamber, Date),
                                             VWC10=mean.na(VW_Avg.1.), VWC30=mean.na(VW_Avg.2.),
                                 VWCHL=mean.na(VW_Avg.3.), VWC100=mean.na(VW_Avg.4.))
soilVWCdaily <- dplyr::left_join(soilVWCdaily, trtkey, by = c('chamber', 'Date'))
soilVWCtreat <- dplyr::summarise(dplyr::group_by(soilVWCdaily, Date, T_treatment, Water_treatment, Date),
                                 VWC_10=mean.na(VWC10), VWC_30=mean.na(VWC30),
                                 VWC_HL=mean.na(VWCHL), VWC_100=mean.na(VWC100))
