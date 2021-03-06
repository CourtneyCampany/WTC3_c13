# The goal is to time match chaber13C flux dataframes with chamber CO2 data frames

delta_FM <- read.csv("calculated_data/deltaflux_fm.csv")
  delta_FM$datetimeFM <- lubridate::ymd_hms(delta_FM$datetimeFM,tz='UTC')

chamflux_FM <- read.csv("calculated_data/chamflux_fm.csv") 
  chamflux_FM$datetime <- lubridate::ymd_hms(chamflux_FM$datetime,tz='UTC')
  chamflux_FM$date <- as.Date(chamflux_FM$date,tz='UTC')
  chamflux_FM$datetimeFM <- lubridate::ymd_hms(chamflux_FM$datetimeFM,tz='UTC')
  chamflux_FM$chamber <- as.factor(chamflux_FM$chamber)

##try to merge the best we can at 15 min, need unique id
#both dataframes should be formatted at proper 15m interval

###issue is with ref and sample in chamb flux(need to split by reg and sample and make new columns)
##then merge will work

xsicalc_func <- function(xsi_dfr){
  
  xsi_dfr$CO2_total <- (xsi_dfr$CorrConcA_Avg + xsi_dfr$CorrConcB_Avg)/(1-0.00474)
  
  #seperate ref and sample lines for calculations
  xsi_a <- xsi_dfr[xsi_dfr$line=="ref",]
    colnames(xsi_a)[(names(xsi_a) == "Corrdel13C_Avg")] <- "del13_ref"
    colnames(xsi_a)[(names(xsi_a) == "CO2_total")] <- "CO2_total_ref"
  
  xsi_b <- xsi_dfr[xsi_dfr$line=="samp",]
    colnames(xsi_b)[(names(xsi_b) == "Corrdel13C_Avg")] <- "del13_samp"
    colnames(xsi_b)[(names(xsi_b) == "CO2_total")] <- "CO2_total_samp"
  
  #new dfr with xsi, deltadiff, DELTA, and timestamp for matching
  deltadiff<- xsi_b$del13_samp - xsi_a$del13_ref
  xsi <- xsi_b$CO2_total_samp/(xsi_a$CO2_total_ref - xsi_b$CO2_total_samp)
  
  xsi_calc <-data.frame(cbind(deltadiff, xsi))
  xsi_calc$DELTA <- (1000 * xsi_calc$xsi * xsi_calc$deltadiff)/
                        (1000+xsi_b$del13_samp-(xsi_calc$xsi*xsi_calc$deltadiff))
  xsi_calc$datetimeFM <- lubridate::ymd_hms(xsi_a$datetimeFM,tz='UTC')
  xsi_calc$chamber <- as.factor(xsi_a$chamber)
  xsi_calc$id <- xsi_a$id
  xsi_calc$del13_samp <- xsi_b$del13_samp
  xsi_calc$del13_ref <- xsi_a$del13_ref
  return(xsi_calc)
}

cham_xsi <- xsicalc_func(delta_FM)

### what is the delta for phloem (just sample?)
### merge this data set with cham flux and calculate gmes canopy
### can add back delta sample to this also to get Aweighted discrimination

#dates wrong for campaign 2

cham_gmes <- merge(cham_xsi, chamflux_FM, by=c("chamber", "id", "datetimeFM"))
  cham_gmes$fluxC_gfm <- with(cham_gmes, FluxCO2*15*60*10^-3*12)
  cham_gmes$fluxCO2_fm <- with(cham_gmes, FluxCO2*15*60*10^-3)
  cham_gmes$fluxCO2_umols <- with(cham_gmes, FluxCO2*10^-3)
  
treatments <- read.csv("data/temp_trt.csv")

cham_gmes2 <- merge(cham_gmes, treatments)
write.csv(cham_gmes2, "calculated_data/merge_test.csv", row.names=FALSE)

#need CI
#need C2sfc
#need Tleaf
#need Trmmol
#need Press

# chamber match testing ---------------------------------------------------

##testing 
testcham <- cham_gmes2[cham_gmes2$month =="March",]
write.csv(testcham, "calculated_data/sample_march.csv", row.names=TRUE)

test1 <- cham_xsi[cham_xsi$id=="1-1" ,]
  test1 <- test1[order(test1$datetimeFM),]
  
test2 <- chamflux_FM[chamflux_FM$id=="1-1",]
  test2 <- test2[order(test2$datetimeFM),]
  
test_merge <- merge(test1, test2, by=c("chamber", "id", "datetimeFM"))
  test_merge$fluxC_gfm <- with(test_merge, FluxCO2*15*60*10^-3*12) #grams C per 15m
  test_merge$fluxCO2_fm <- with(test_merge, FluxCO2*15*60*10^-3)
  
  test_merge2 <- merge(test_merge, treatments)

# Chamber CO2 flux (mmol/s) 

# mmol/s to 
# ALEAF*15*60*10^-3*12)
  
##save the example paired chambers  to a dataframe to test gmes calc
# march_ch9_ch12 <- testcham[testcham$chamber %in% c(9,12),]
# write.csv(march_ch9_ch12, "gmes_calc/march_ch9_ch12.csv", row.names = FALSE)

  