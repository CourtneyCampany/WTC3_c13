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
  
  #remove reference gases
  x2 <- x[x$SiteOutput != 3 & x$SiteOutput != 4, ]  

  dfr <- x2[,c("SiteOutput", "chamber", "line", "CorrConcA_Avg","CorrConcB_Avg",  "Corrdel13C_Avg", 
               "flux_campaign",  "Date","datetime","datetimeFM", "day", "id")]
  
  return(dfr)
}

#formated list of delta files by campaign
delta_files <- lapply(flux_months, cham13format_func)

# test <- plyr::rbind.fill(delta_files)

# calculate 15m means per chamber per line -----------------------------------------
delta_FM <- lapply(delta_files, function(x) 
  doBy::summaryBy(. ~ datetimeFM + chamber + line, FUN=mean, keep.names = TRUE, data=x))

delta_FM_all <- plyr::rbind.fill(delta_FM)
delta_FM_all$id <- as.factor(paste(delta_FM_all$flux_campaign, delta_FM_all$day, sep="-"))

write.csv(delta_FM_all, "calculated_data/deltaflux_fm.csv", row.names=FALSE)
