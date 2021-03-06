
##need to combine all chamber fluxes
##assign campaign id
# Teresa: old code did not include the flux data from the last campaign (April 2014)
#chamberflux_15 <- read.csv("data/chamberflux_15m.csv")
library(HIEv)
setToken()
# download the file with the flux data
chamberflux_15 <- downloadCSV(filename="WTC_TEMP_CM_WTCFLUX_20130914-20140526_L2_V2.csv")
#alternatively use a file from a local directory
#chamberflux_15 <- read.csv(filename="WTC_TEMP_CM_WTCFLUX_20130914-20140526_L2_V2.csv")
  chamberflux_15$datetime <- lubridate::ymd_hms(chamberflux_15$DateTime,tz='UTC')
  chamberflux_15$date <- as.Date(chamberflux_15$date,tz='UTC')

  campaignassign <- function(x) {
    
    x$month <- factor(format(chamberflux_15$datetime, "%B"))
    
    x$flux_campaign <- ifelse(x$month == "October", 1, "missing")
    x$flux_campaign <- ifelse(x$month == "December", 2,  x$flux_campaign)
    x$flux_campaign <- ifelse(x$month == "January", 3,  x$flux_campaign)
    x$flux_campaign <- ifelse(x$month == "February", 4,  x$flux_campaign)
    x$flux_campaign <- ifelse(x$month == "March", 5,  x$flux_campaign)
    x$flux_campaign <- ifelse(x$month == "April", 6, x$flux_campaign)
    #timestep for calculating correct mean
    x$datetimeFM <- HIEv::nearestTimeStep(x$datetime, nminutes=15, align="ceiling")
    x$id <- as.factor(paste(x$flux_campaign, x$day, sep="-"))
    return(x)
}
 
chamberflux2_15 <- campaignassign(chamberflux_15)
TDLdates <- read.csv('data/TDLdates.csv')
TDLdates$Start <- lubridate::dmy(as.character(TDLdates$Start))
TDLdates$End <- lubridate::dmy(as.character(TDLdates$End))
empty <- list()
for (i in 1:nrow(TDLdates)){
  empty[[i]] <- subset(chamberflux2_15, as.Date(datetime)>=as.Date(TDLdates$Start[i]) & as.Date(datetime)<=as.Date(TDLdates$End[i]))
  empty[[i]] <- doBy::orderBy(~datetime, empty[[i]])
}
chamberflux3_15 <- plyr::rbind.fill(empty)
rm(empty, TDLdates)
#plot some fluxes
#windows(15,8)
#par(mfrow=c(2,5))
#for(i in 1:5){
#  plot(subset(chamberflux2_15, flux_campaign==i)[,'datetimeFM'], 
#       subset(chamberflux2_15, flux_campaign==i)[,'FluxCO2'], pch=19, ylim=c(-0.05,0.25),
#       col=as.factor(chamberflux2_15$chamber), ylab='Photo (mmol/s)', xlab='',
#       main=paste0('Campaing ', i))
#}
#plot(subset(chamberflux2_15, flux_campaign==1)[,'datetimeFM'], 
#     subset(chamberflux2_15, flux_campaign==1)[,'FluxH2O'], pch=19, ylim=c(-0.009, 0.06),
#     col=as.factor(chamberflux2_15$chamber), ylab='E (mol/s)', xlab='')
#legend('topright', legend = paste0('Chm ', 1:12), pch=19, col=1:12, bty='n')
#for(i in 2:5){
#  plot(subset(chamberflux2_15, flux_campaign==i)[,'datetimeFM'], 
#       subset(chamberflux2_15, flux_campaign==i)[,'FluxH2O'], pch=19, ylim=c(-0.009, 0.06),
#       col=as.factor(chamberflux2_15$chamber), ylab='E (mol/s)', xlab='')
#}
   
# which(is.na(chamberflux_15$flux_campaign == "missing"))

#need to split into list my measurement campaign (id) and calculate 15min mean--------------------------

# Teresa: I am not entirely sure this step is necessary because the WTC fluxes are already measured
#every 15 min each, thus there should only be one measurement per chamber every 15 min

#chambflux <- split(chamberflux2_15, chamberflux2_15$id)

#cham_FM <- lapply(chambflux, function(x) 
  #doBy::summaryBy(. ~ datetimeFM + chamber, FUN=mean, keep.names = TRUE, data=x))

#cham_FM_all <- plyr::rbind.fill(cham_FM)

chamberflux_15 <- read.csv("data/chamberflux_15m.csv")
 chamberflux_15$datetime <- lubridate::ymd_hms(chamberflux_15$datetime,tz='UTC')
 chamberflux_15$date <- as.Date(chamberflux_15$date,tz='UTC')

campaignassign <- function(x) {
  
  x$month <- factor(format(chamberflux_15$datetime, "%B"))
  
  x$flux_campaign <- ifelse(x$month == "October", 1, "missing")
  x$flux_campaign <- ifelse(x$month == "December", 2,  x$flux_campaign)
  x$flux_campaign <- ifelse(x$month == "January", 3,  x$flux_campaign)
  x$flux_campaign <- ifelse(x$month == "February", 4,  x$flux_campaign)
  x$flux_campaign <- ifelse(x$month == "March", 5,  x$flux_campaign)
  #timestep for calculating correct mean
  x$datetimeFM <- HIEv::nearestTimeStep(x $datetime, nminutes=15, align="ceiling")
  x$id <- as.factor(paste(x$flux_campaign, x$day, sep="-"))
  return(x)
}

chamberflux2_15 <- campaignassign(chamberflux_15)

write.csv(chamberflux2_15, "calculated_data/chamflux_fm.csv", row.names = FALSE)
rm(chamberflux2_15, chamberflux_15)