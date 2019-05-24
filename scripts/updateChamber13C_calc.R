##This script makes neat 15 minute averages of delta 13C chamber flux 

# read delta flux files into a list ---------------------------------------
flux_files <- list.files(path = "data/chamber_13C_clean/", pattern="chflux", full.names = TRUE)
##make names of list with file names minus extension
flux_names <- gsub("raw data/chamber_13C_clean/", "", flux_files)
flux_names <- gsub(".csv", "", flux_files)
flux_names <- gsub("chflux_", "", flux_files)

flux_months <- lapply(flux_files, function(x) read.csv(x, stringsAsFactors = FALSE))
##add names to list
names(flux_months) <- flux_names

# format delta flux files -------------------------------------------------
cham13format_func <- function(x){
  x$Date <- strptime(x$Date, format = "%d/%m/%Y",  tz="UTC")
  x$calendar <- as.character(x$Date)
  x$clock <- as.character(x$time)
  x$start <- paste(x$calendar, x$clock, sep=" ")  
  x$start <- strptime(x$start,  format = "%Y-%m-%d  %I:%M:%S %p",  tz="UTC")
  x$end <- x$start + 20
  x$totalCO2 <- (x$CorrConcA_Avg+x$CorrConcB_Avg)/(1-0.00474)
  
  #remove reference gases
  x2 <- x[x$SiteOutput != 3 & x$SiteOutput != 4, ]  
  
  dfr <- x2[,c("chamber", "line","totalCO2",  "Corrdel13C_Avg", "start",'end')]
  
  return(dfr)
}

#formated list of delta files by campaign
delta_files <- lapply(flux_months, cham13format_func)
long <- plyr::rbind.fill(delta_files)
long <- long[which(!is.na(long$start)),]
longRef <- subset(long, line == "ref")[,c('start','end','chamber','totalCO2','Corrdel13C_Avg')]
longRef$pairID <- paste0(longRef$chamber, '-', longRef$end)
names(longRef)[1:(ncol(longRef) - 1)] <- paste0(names(longRef[1:(ncol(longRef)-1)]), '_ref')
longSmp <- subset(long, line == "samp")[,c('start','end','chamber','totalCO2','Corrdel13C_Avg')]
longSmp$pairID <- paste0(longSmp$chamber, '-', longSmp$start)
deltaPaired <- dplyr::left_join(longSmp, longRef, by='pairID')
deltaPaired$datetimeFM <- ymd_hms(as.character(deltaPaired$datetimeFM))
deltaPaired$chamber2 <- as.character(deltaPaired$chamber)
deltaPaired$chamber <- ifelse(nchar(deltaPaired$chamber2)==2,
                              paste0('C',deltaPaired$chamber2), paste0('C0',deltaPaired$chamber2))
deltaPaired[which(deltaPaired$totalCO2<=0),c('totalCO2','Corrdel13C_Avg')] <- NA
deltaPaired[which(deltaPaired$totalCO2_ref<=0),c('totalCO2_ref','Corrdel13C_Avg_ref')] <- NA
deltaPaired[which(deltaPaired$totalCO2_ref>=600),c('totalCO2_ref','Corrdel13C_Avg_ref')] <- NA


#test <- plyr::rbind.fill(delta_files)
#Optional graph mean and sd for 15 min averages between start and stop dates
#start <- lubridate::ymd_hm('2014-03-22 06:00')
#stop <- lubridate::ymd_hm('2014-03-23 22:00')
#source('scripts/plotSDmeansTDL.R')
# calculate 15m means per chamber per line -----------------------------------------
delta_FM <- lapply(delta_files, function(x) 
  doBy::summaryBy(. ~ datetimeFM + chamber + line, FUN=mean, keep.names = TRUE, data=x))

delta_FM_all <- plyr::rbind.fill(delta_FM)
delta_FM_all$id <- as.factor(paste(delta_FM_all$flux_campaign, delta_FM_all$day, sep="-"))

#remove the 3 na rows and rows where missing pair(ref/samp) exists with manual testing below
#sort(which(is.na(delta_FM_all)))
# test <- delta_FM_all2[delta_FM_all2$id=="5-2",]
# plyr::count(test$line)
# sapply(delta_FM_all, function(x) sum(is.na(x)))

delta_FM_all2 <- delta_FM_all[-c(3504,4661,7013, 8168,9337,9338,10511,12534,12535,14028),]
# plyr::count(delta_FM_all2$line)
# delta_FM_all3 <- delta_FM_all2[complete.cases(delta_FM_all2),]

write.csv(delta_FM_all2, "calculated_data/deltaflux_fm.csv", row.names=FALSE)

TDL <- delta_FM_all2
TDL$pairID <- as.factor(paste0(TDL$datetimeFM, '-', TDL$chamber))
TDL$totalCO2 <- (TDL$CorrConcA_Avg+TDL$CorrConcB_Avg)/(1-0.00474)
dfSample <- subset(TDL, line=='samp')
dfRef <- subset(TDL, line=='ref')[,c('CorrConcA_Avg','CorrConcB_Avg','Corrdel13C_Avg','totalCO2','pairID')]
colnames(dfRef)[1:(ncol(dfRef)-1)] <- paste0(colnames(dfRef)[1:(ncol(dfRef)-1)], '_ref')
deltaPaired <- merge(dfSample, dfRef, by='pairID', all=T)[,c('datetimeFM','chamber','totalCO2','Corrdel13C_Avg',
                                                             'totalCO2_ref','Corrdel13C_Avg_ref')]
deltaPaired$datetimeFM <- ymd_hms(as.character(deltaPaired$datetimeFM))
deltaPaired$chamber2 <- as.character(deltaPaired$chamber)
deltaPaired$chamber <- ifelse(nchar(deltaPaired$chamber2)==2,
                              paste0('C',deltaPaired$chamber2), paste0('C0',deltaPaired$chamber2))
deltaPaired[which(deltaPaired$totalCO2<=0),c('totalCO2','Corrdel13C_Avg')] <- NA
deltaPaired[which(deltaPaired$totalCO2_ref<=0),c('totalCO2_ref','Corrdel13C_Avg_ref')] <- NA
deltaPaired[which(deltaPaired$totalCO2_ref>=600),c('totalCO2_ref','Corrdel13C_Avg_ref')] <- NA
