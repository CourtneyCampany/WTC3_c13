
##this script downloads chamberflux data and trims it to times of c13 flux measurementslibrary(plyr)

library(devtools)
library(HIEv)
library(doBy)

setToken("u2xEk2wTte3AsdBxGTr5")

wtc_search <- searchHIEv(filename="wTCMET")

cham_flux <- downloadCSV(filename="WTC_TEMP_CM_WTCMET_20131001")

