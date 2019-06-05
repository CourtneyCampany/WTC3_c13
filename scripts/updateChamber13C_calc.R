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
deltaPairedPre <- dplyr::left_join(longSmp, longRef, by='pairID')[,c('start_ref','end', 'chamber',
                                                                  'totalCO2','totalCO2_ref',
                                                                  'Corrdel13C_Avg','Corrdel13C_Avg_ref')]
names(deltaPairedPre)[1] <- 'start'
deltaPairedPre$chamber2 <- as.character(deltaPairedPre$chamber)
deltaPairedPre$chamberTDL <- ifelse(nchar(deltaPairedPre$chamber2) == 2, paste0('C', deltaPairedPre$chamber2),
                              paste0('C0', deltaPairedPre$chamber2))
deltaPairedPre[which(deltaPairedPre$totalCO2<=0),c('totalCO2','Corrdel13C_Avg')] <- NA
deltaPairedPre[which(deltaPairedPre$totalCO2_ref<=0),c('totalCO2_ref','Corrdel13C_Avg_ref')] <- NA
deltaPairedPre[which(deltaPairedPre$totalCO2_ref>=600),c('totalCO2_ref','Corrdel13C_Avg_ref')] <- NA
deltaPairedPre <- deltaPairedPre[, c("start","end","totalCO2", "totalCO2_ref",
                               "Corrdel13C_Avg","Corrdel13C_Avg_ref","chamberTDL")]
rm(long, longRef, longSmp, delta_files, flux_files, flux_names, cham13format_func, flux_months)
