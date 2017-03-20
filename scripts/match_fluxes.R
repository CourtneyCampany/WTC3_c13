# The goal is to time match chaber13C flux dataframes with chamber CO2 data frames

delta_FM <- read.csv("calculated_data/deltaflux_fm.csv")

chamflux_FM <- read.csv("calculated_data/chamflux_fm.csv") 


##try to merge the best we can at 15 min, need unique id
#both dataframes should be formatted at proper 15m interval

test <- merge(delta_FM, chamflux_FM, by=c("id", "datetimeFM"))


test <- merge(delta_FM, chamflux_FM)


test1 <- delta_FM[delta_FM$id=="1-1",]
test2 <- chamflux_FM[chamflux_FM$id=="1-1",]


test3 <- merge(test1, test2, by=c("chamber","datetimeFM"))


###issue is with ref and sample in chamb flux(need to split by reg and sample and make new columns)
##then merge will work