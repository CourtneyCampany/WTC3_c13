source('scripts/basicFunTEG.R')
#library(HIEv)
#setToken()
# something is off with my HIEv account
# morpho <- downloadCSV(filename="WTC_TEMP_CM_HARVEST-CANOPY_20140526-20140528_L1_v1.csv")
# alternatively read file from local directory
morpho <- read.csv('data/WTC_TEMP_CM_WTCFLUX_20130914-20140526_L2_V2.csv')
treeLeaf <- doBy::summaryBy(leafArea ~ chamber + Date, FUN=mean, data=morpho)
colnames(treeLeaf)[3] <- 'leafArea'
treeLeaf$chamber <- as.character(treeLeaf$chamber)
treeLeaf$Date <- as.Date(ymd(as.character(treeLeaf$Date)))
rm(morpho)