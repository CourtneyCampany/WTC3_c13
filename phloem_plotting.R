phloem <- read.csv("data/phloem_c13.csv")
  phloem$chamber <- as.factor(phloem$chamber)

trtkey <- read.csv("data/temp_trt.csv")

#add warming treatments
phloem2 <- merge(phloem, trtkey)

#add monthly 
campaignassign <- function(x) {
  
  x$month <- factor(format(chamberflux_15$datetime, "%B"))
  
  x$month <- ifelse(x$month == "October", 1, "missing")
  x$month <- ifelse(x$month == "December", 2,  x$flux_campaign)
  x$month <- ifelse(x$month == "January", 3,  x$flux_campaign)
  x$month <- ifelse(x$month == "February", 4,  x$flux_campaign)
  x$month <- ifelse(x$flux_campaign == "March", 5,  x$flux_campaign)
  #timestep for calculating correct mean
  x$datetimeFM <- HIEv::nearestTimeStep(x $datetime, nminutes=15, align="ceiling")
  x$id <- as.factor(paste(x$flux_campaign, x$day, sep="-"))
  return(x)
}

