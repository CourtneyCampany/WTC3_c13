
##need to combine all chamber fluxes
##assign campaign id

chamberflux_15 <- read.csv("data/chamberflux_15m.csv")
  chamberflux_15$datetime <- lubridate::ymd_hms(chamberflux_15$datetime)
  chamberflux_15$Date <- as.Date(chamberflux_15$Date)

  campaignassign <- function(x) {
    
    x$month <- factor(format(chamberflux_15$datetime, "%B"), + levels = month.name)
    
    x$campaign <- ifelse(x$month == "October", 1, "missing")
    x$campaign <- ifelse(x$month == "December", 2,  x$campaign)
    x$campaign <- ifelse(x$month == "January", 3,  x$campaign)
    x$campaign <- ifelse(x$month == "February", 4,  x$campaign)
    x$campaign <- ifelse(x$month == "March", 5,  x$campaign)
    
    z <- x[,-month]
    
  return(z)
}
  
which(is.na(chamberflux_15$campaign == "missing"))


# read in chamber flux data ----------------------------------------------

chamflux <- read.csv("data/chamberflux_15m.csv")
library(lubridate)  
chamflux$date <- as.Date(chamflux$date, tz='Australia/Sydney')
chamflux$datetime <- ymd_hms(chamflux$datetime, tz='Australia/Sydney')
chamflux$id <- paste(chamflux$campaign, chamflux$day, sep="-")

# simple plots ------------------------------------------------------------

chamlist <- ls()

chamlist <- split(chamflux, "id")


plot(FluxCO2 ~ datetime, col=chamber, data=chamflux[chamflux$id == "4-2",])


##calculate ci from flux data

test <-chamflux[chamflux$id == "4-2",]
