
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
