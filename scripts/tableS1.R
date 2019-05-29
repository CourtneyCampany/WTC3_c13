library(HIEv)
setToken()
myDates <- read.csv('data/crapyDates.csv')
myDates$Date <- dmy(as.character(myDates$Date))
ROSnames <- paste0('data/ROS/', list.files('data/ROS/'))
ROS <- plyr::rbind.fill(lapply(ROSnames, readTOA5))
ROSdate <- dplyr::summarise(dplyr::group_by(ROS, Date), Tmean=mean(AirTC_Avg),
                            Tmax=max(AirTC_Avg), Tmin=min(AirTC_Avg), PARmax=max(PPFD_Avg),
                             PARtotal=sum(PPFD_Avg*5*60/1e06))
tableS1 <- dplyr::left_join(myDates, ROSdate, by='Date')
write.csv(tableS1, file='output/tableS1.csv', row.names = F)
rm(myDates, ROS, ROSnames, ROSdate, tableS1)