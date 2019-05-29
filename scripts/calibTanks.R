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
  x$datetime <- paste(x$calendar, x$clock, sep=" ")  
  x$datetime <- strptime(x$datetime,  format = "%Y-%m-%d  %I:%M:%S %p",  tz="UTC")
  #15minute time interval
  x$datetimeFM <- HIEv::nearestTimeStep(x$datetime, nminutes=15, align="ceiling")
  #unique id
  x$id <- as.factor(paste(x$flux_campaign, x$day, sep="-"))
  x$totalCO2 <- (x$CorrConcA_Avg+x$CorrConcB_Avg)/(1-0.00474)
  dfr <- x[,c("SiteOutput", "chamber", "line", "CorrConcA_Avg","CorrConcB_Avg",  "Corrdel13C_Avg", 
               "flux_campaign",  "Date","datetime","datetimeFM", "day", "id", 'totalCO2')]
  
  return(dfr)
}

#formated list of delta files by campaign
delta_files <- lapply(flux_months, cham13format_func)
tres <- lapply(delta_files, function(x) subset(x, SiteOutput == 3 & totalCO2 <= 1000))
cuatro <- lapply(delta_files, function(x) subset(x, SiteOutput == 4))
tresCO2Mean <- lapply(tres, function(x) mean(x$totalCO2))
tresCO2SE <- lapply(tres, function(x) s.err(x$totalCO2))
tresd13Mean <- lapply(tres, function(x) mean(x$Corrdel13C_Avg))
tresd13SE <- lapply(tres, function(x) s.err(x$Corrdel13C_Avg))
cuatroCO2Mean <- lapply(cuatro, function(x) mean(x$totalCO2))
cuatroCO2SE <- lapply(cuatro, function(x) s.err(x$totalCO2))
cuatrod13Mean <- lapply(cuatro, function(x) mean(x$Corrdel13C_Avg))
cuatrod13SE <- lapply(cuatro, function(x) s.err(x$Corrdel13C_Avg))
calibTanks <- data.frame(row.names = 1:length(delta_files))
for (i in 1:length(delta_files)){
  calibTanks$tresCO2mean[i] <- tresCO2Mean[[i]]
  calibTanks$tresCO2se[i] <- tresCO2SE[[i]]
  calibTanks$tresd13mean[i] <- tresd13Mean[[i]]
  calibTanks$tresd13se[i] <- tresd13SE[[i]]
  calibTanks$cuatroCO2mean[i] <- cuatroCO2Mean[[i]]
  calibTanks$cuatroCO2se[i] <- cuatroCO2SE[[i]]
  calibTanks$cuatrod13mean[i] <- cuatrod13Mean[[i]]
  calibTanks$cuatrod13se[i] <- cuatrod13SE[[i]]
}
# these are the values I write for the methods for the calibration tanks
round(mean(calibTanks$tresCO2mean))
round(mean(calibTanks$tresd13mean))
round(mean(calibTanks$cuatroCO2mean))
round(mean(calibTanks$cuatrod13mean))
rm(delta_files, flux_files, calibTanks, tres, cuatro, tresCO2Mean, tresCO2SE, cuatroCO2Mean, cuatroCO2SE,
   tresd13Mean, tresd13SE, cuatrod13Mean, cuatrod13SE)