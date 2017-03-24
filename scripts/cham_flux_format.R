
##need to combine all chamber fluxes
##assign campaign id

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
   
# which(is.na(chamberflux_15$flux_campaign == "missing"))

#need to split into list my measurement campaign (id) and calculate 15min mean--------------------------

chambflux <- split(chamberflux2_15, chamberflux2_15$id)

cham_FM <- lapply(chambflux, function(x) 
  doBy::summaryBy(. ~ datetimeFM + chamber, FUN=mean, keep.names = TRUE, data=x))

cham_FM_all <- plyr::rbind.fill(cham_FM)

write.csv(chamberflux2_15, "calculated_data/chamflux_fm.csv", row.names = FALSE)

