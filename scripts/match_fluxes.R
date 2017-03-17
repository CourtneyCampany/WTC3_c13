# The goal is to time match chaber13C flux dataframes with chamber CO2 data frames
# First attempt will be to establish equal 15min averages for both, then merge.


test <- read.csv("data/chamber_13C_clean/chflux_jan_d1.csv")


tdlformat_func <- function(x){
  x$Date <- strptime(x$Date, format = "%d/%m/%Y",  tz="UTC")
  x$calendar <- as.character(x$Date)
  x$clock <- as.character(x$time)
  x$datetime <- paste(x$calendar, x$clock, sep=" ")  
    x$datetime <- strptime(x$datetime,  format = "%Y-%m-%d  %I:%M:%S %p",  tz="UTC")
  
  dfr <- x[,c("SiteOutput", "chamber", "line", "CorrConcA_Avg","CorrConcB_Avg",  "Corrdel13C_Avg", 
              "Date","datetime","flux_campaign")]
  
  return(dfr)
}

test2 <- tdlformat_func(test)

ch1 <- test2[test2$chamber==1,]

#cut datetime by hour
ch1$hour <- cut(ch1$datetime, breaks = "hour")
 ch1_agg <- doBy::summaryBy(. ~ hour+line, data=ch1, FUN=mean, keep.names=TRUE)

ch1$by15 = cut(ch1$datetime, breaks="15 min")
##can I set this by start of the interval or start of the hour?
