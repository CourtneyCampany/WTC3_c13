TDLdates <- read.csv('data/TDLdates.csv')
TDLdates$Start <- lubridate::dmy(as.character(TDLdates$Start))
TDLdates$End <- lubridate::dmy(as.character(TDLdates$End))
empty <- list()
for (i in 1:nrow(TDLdates)){
  empty[[i]] <- subset(WTCraw, as.Date(datetime)>=as.Date(TDLdates$Start[i]) & as.Date(datetime)<=as.Date(TDLdates$End[i]))
  empty[[i]] <- doBy::orderBy(~datetime, empty[[i]])
}
WTCrawShort <- plyr::rbind.fill(empty)
rm(empty, TDLdates)