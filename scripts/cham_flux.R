
##this script downloads chamberflux data and trims it to times of c13 flux measurements


##John has the 1 hr data cleaned and on Figshare----------------------------

# #- Download an the data. This downloads the zipfile from figshare
# download.file("https://ndownloader.figshare.com/files/4857112", "data.zip", mode="wb")
# # Extract data to a folder named "data".
# unzip("data.zip",overwrite=F)

##subset data by times of c13 measurements
# flux <- read.csv("data/chamber_flux_all.csv")
#   flux$Date <- as.Date(flux$Date, tz='Australia/Sydney')
#   library(lubridate)
#   flux$DateTime <- ymd_hms(flux$DateTime, tz='Australia/Sydney')

# hieV 15m ------------------------------------------------------------------

# library(HIEv)
# setToken("u2xEk2wTte3AsdBxGTr5")
# 
# wtc_search <- searchHIEv(filename="WTC_Temp_CM_WTCFLUX")
# 
# flux_hiev <- downloadCSV(filename="WTC_Temp_CM_WTCFLUX_20130910-20140530_L0_V1.csv")
#   flux_hiev$date <- as.Date(flux_hiev$date, tz='Australia/Sydney')
#   library(lubridate)
#   flux_hiev$datetime <- strptime(flux_hiev$datetime, format = "%Y-%m-%d %H:%M:%S",tz='Australia/Sydney')
# 
#   vars <- c("FluxH2O","FluxCO2","datetime","date","VPDair","period","WUE",
#             "chamber","PAR","RefCO2","TAref","Tair_al", "RH_al")
# 
# 
# flux_hiev2 <- flux_hiev[,vars]
  
# subset both 15m and hourly datasets, then comment and delete big data files--------------------------

date_pick <- function(dat,x,y){dat[dat$datetime >= x & dat$datetime <= y,]}

#15m
oct_d1_15m <- date_pick(flux_hiev2,"2013-10-18 08:30:00", "2013-10-19 08:58:00")
  oct_d1_15m <- oct_d1_15m[-1:-48,]
  oct_d1_15m$campaign <- 1
  oct_d1_15m$day <- 1
  
oct_d2_15m <- date_pick(flux_hiev2,"2013-10-20 07:45:00","2013-10-21 08:15:00")
  oct_d2_15m <- oct_d2_15m[-1:-48,]
  oct_d2_15m$campaign <- 1
  oct_d2_15m$day <- 2
  
dec_d1_15m <- date_pick(flux_hiev2,"2013-12-06 10:18:00", "2013-12-07 10:18:00")
  dec_d1_15m <- dec_d1_15m[-1:-48,]
  dec_d1_15m$campaign <- 2
  dec_d1_15m$day <- 1
  
dec_d2_15m <- date_pick(flux_hiev2,"2013-12-07 11:17:00","2013-12-08 11:17:00")
  dec_d2_15m <- dec_d2_15m[-1:-48,]
  dec_d2_15m$campaign <- 2
  dec_d2_15m$day <- 2
  
jan_d1_15m <- date_pick(flux_hiev2,"2014-01-17 09:16:00", "2014-01-18 09:17:00")
  jan_d1_15m <- jan_d1_15m[-1:-48,]
  jan_d1_15m$campaign <- 3
  jan_d1_15m$day <- 1

jan_d2_15m <- date_pick(flux_hiev2,"2014-01-18 10:00:00","2014-01-19 10:15:00")
  jan_d2_15m <- jan_d2_15m[-1:-48,]
  jan_d2_15m$campaign <- 3
  jan_d2_15m$day <- 2

feb_d1_15m <- date_pick(flux_hiev2,"2014-02-22 06:05:00", "2014-02-23 06:33:00")
  feb_d1_15m <- feb_d1_15m[-1:-48,]
  feb_d1_15m$campaign <- 4
  feb_d1_15m$day <- 1
  
feb_d2_15m <- date_pick(flux_hiev2,"2014-02-23 06:50:00","2014-02-24 07:18:00")
  feb_d2_15m <- feb_d2_15m[-1:-48,]
  feb_d2_15m$campaign <- 4
  feb_d2_15m$day <- 2

mar_d1_15m <- date_pick(flux_hiev2,"2014-03-22 06:23:00", "2014-03-23 06:36:00")
  mar_d1_15m <- mar_d1_15m[-1:-48,]
  mar_d1_15m$campaign <- 5
  mar_d1_15m$day <- 1

mar_d2_15m <- date_pick(flux_hiev2,"2014-03-23 07:38:00","2014-03-24 08:06:00")
  mar_d2_15m <- mar_d2_15m[-1:-48,]
  mar_d2_15m$campaign <- 5
  mar_d2_15m$day <- 2
  
chamberflux_15 <- Reduce(function(...) merge(..., all=TRUE),
  list(oct_d1_15m,oct_d2_15m,dec_d1_15m,dec_d2_15m,jan_d1_15m,jan_d2_15m,feb_d1_15m,feb_d2_15m,
                         mar_d1_15m,mar_d2_15m))

write.csv(chamberflux_15, "data/chamberflux_15m.csv", row.names = FALSE)

#1hr
date_pick2 <- function(x,y){flux[flux$DateTime >= x & flux$DateTime <= y,]}

oct_d1_60m <- date_pick2("2013-10-18 08:00:00", "2013-10-19 09:00:00")
  oct_d1_60m <- oct_d1_60m[-1:-12,]
  oct_d1_60m$campaign <- 1
  oct_d1_60m$day <- 1

oct_d2_60m <- date_pick2("2013-10-20 07:00:00","2013-10-21 09:00:00")
  oct_d2_60m <- oct_d2_60m[-1:-12,]
  oct_d2_60m$campaign <- 1
  oct_d2_60m$day <- 2

dec_d1_60m <- date_pick2("2013-12-06 10:00:00", "2013-12-07 11:00:00")
  dec_d1_60m <- dec_d1_60m[-1:-12,]
  dec_d1_60m$campaign <- 2
  dec_d1_60m$day <- 1


dec_d2_60m <- date_pick2("2013-12-07 11:00:00","2013-12-08 12:00:00")
  dec_d2_60m <- dec_d2_60m[-1:-12,]
  dec_d2_60m$campaign <- 2
  dec_d2_60m$day <- 2

jan_d1_60m <- date_pick2("2014-01-17 09:00:00", "2014-01-18 10:00:00")
  jan_d1_60m <- jan_d1_60m[-1:-12,]
  jan_d1_60m$campaign <- 3
  jan_d1_60m$day <- 1

jan_d2_60m <- date_pick2("2014-01-18 10:00:00","2014-01-19 11:00:00")
  jan_d2_60m <- jan_d2_60m[-1:-12,]
  jan_d2_60m$campaign <- 3
  jan_d2_60m$day <- 2


feb_d1_60m <- date_pick2("2014-02-22 06:00:00", "2014-02-23 07:00:00")
  feb_d1_60m <- feb_d1_60m[-1:-12,]
  feb_d1_60m$campaign <- 4
  feb_d1_60m$day <- 1

feb_d2_60m <- date_pick2("2014-02-23 05:00:00","2014-02-24 08:00:00")
  feb_d2_60m <- feb_d2_60m[-1:-12,]
  feb_d2_60m$campaign <- 4
  feb_d2_60m$day <- 2

mar_d1_60m <- date_pick2("2014-03-22 06:00:00", "2014-03-23 07:00:00")
  mar_d1_60m <- mar_d1_60m[-1:-12,]
  mar_d1_60m$campaign <- 5
  mar_d1_60m$day <- 1

mar_d2_60m <- date_pick2("2014-03-23 07:00:00","2014-03-24 09:00:00")
  mar_d2_60m <- mar_d2_60m[-1:-12,]
  mar_d2_60m$campaign <- 5
  mar_d2_60m$day <- 2


chamberflux_60 <- Reduce(function(...) merge(..., all=TRUE),
        list(oct_d1_60m,oct_d2_60m,dec_d1_60m,dec_d2_60m,jan_d1_60m,jan_d2_60m,feb_d1_60m,feb_d2_60m,
              mar_d1_60m,mar_d2_60m))

write.csv(chamberflux_60, "data/chamberflux_60m.csv", row.names = FALSE)





