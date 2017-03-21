# The goal is to time match chaber13C flux dataframes with chamber CO2 data frames

delta_FM <- read.csv("calculated_data/deltaflux_fm.csv")

chamflux_FM <- read.csv("calculated_data/chamflux_fm.csv") 


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
  xsi_calc$time <- xsi_a$datetimeFM
  xsi_calc$chamber <- xsi_a$chamber
  xsi_calc$id <- xsi_a$id
  xsi_calc$del13_samp <- xsi_b$del13_samp
  
  return(xsi_calc)
}

cham_xsi <- xsicalc_func(delta_FM)

### what is the delta for phloem (just sample?)
### merge this data set with cham flux and calculate gmes canopy
### can add back delta sample to this also to get Aweighted discrimination

cham_gmes <- merge(cham_xsi, chamflux_FM, by=c("chamber", "id", "datetimeFM"), all=TRUE)

