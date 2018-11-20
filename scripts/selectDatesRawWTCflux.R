TDLdates <- read.csv('data/TDLdates.csv')
TDLdates$Start <- lubridate::dmy_hm(as.character(TDLdates$Start))
TDLdates$End <- lubridate::dmy_hm(as.character(TDLdates$End))
empty <- list()
for (i in 1:nrow(TDLdates)){
  empty[[i]] <- subset(WTCraw, datetime>=TDLdates$Start[i] & datetime<=TDLdates$End[i])
  empty[[i]] <- doBy::orderBy(~datetime, empty[[i]])
}
WTCrawShort <- plyr::rbind.fill(empty)
rm(empty, TDLdates)