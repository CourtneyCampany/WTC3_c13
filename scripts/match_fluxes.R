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

#need CI
#need C2sfc
#need Tleaf
#need Trmmol
#need Press

# chamber match testing ---------------------------------------------------


testcham <- cham_gmes2[cham_gmes2$month =="March",]

test1 <- cham_xsi[cham_xsi$id=="1-1" ,]
  test1 <- test1[order(test1$datetimeFM),]
  
test2 <- chamflux_FM[chamflux_FM$id=="1-1",]
  test2 <- test2[order(test2$datetimeFM),]
  
test_merge <- merge(test1, test2, by=c("chamber", "id", "datetimeFM"))
  test_merge$fluxC_gfm <- with(test_merge, FluxCO2*15*60*10^-3*12)
  test_merge$fluxCO2_fm <- with(test_merge, FluxCO2*15*60*10^-3)
  
  test_merge2 <- merge(test_merge, treatments)

# Chamber CO2 flux (mmol/s) 

# mmol/s to 
# ALEAF*15*60*10^-3*12)

#two panel example
  
windows()
# png(filename = "flux_example.png", width = 8, height = 6, units = "in", res= 600)
par(cex.axis=1, cex.lab=1,las=1,mgp=c(3,1,0),mfrow=c(2,1)) 

par(mar=c(0,6,2,2))
plot(FluxCO2~datetimeFM,data=testcham[testcham$chamber ==12,], type='l', col="red", lwd=2, xaxt='n',
     ylab=expression(atop(Chamber~CO[2]~flux,~~(m*mol~m^-2~s^-1))))
  points(FluxCO2~datetimeFM,data=testcham[testcham$chamber ==9,], type='l', col="cornflowerblue", lwd=2)
  legend("topright", c("CH12-ET", "CH09-AT"), lty=1, col=c("red", "cornflowerblue"), title="March 23",
         bty='n', inset=.02, lwd=2)
  
par(mar=c(3,6,0,2))
plot(del13_samp~datetimeFM,data=testcham[testcham$chamber ==9,], type='l', col="cornflowerblue", lwd=2, 
     xlab="", ylab=expression(atop(Chamber~flux,~{delta}^13*C~~('\211'))), ylim=c(-13.5, -7.5))
  points(del13_samp~datetimeFM,data=testcham[testcham$chamber ==12,], type='l', col="red", lwd=2)
  points(del13_ref~datetimeFM,data=testcham[testcham$chamber ==9,], type='l', col="cornflowerblue", lwd=2, lty=3)
  points(del13_ref~datetimeFM,data=testcham[testcham$chamber ==12,], type='l', col="red", lwd=2, lty=3)

  legend("topright", c("CH12-ET", "CH09-AT","reference", "sample" ), lty=c(1,1,3,1), 
         col=c("red", "cornflowerblue","black", "black"), bty='n', inset=.02, lwd=2)  
  
dev.copy2pdf(file="flux_example.pdf")
dev.off()
  


# start calculations ------------------------------------------------------

gmcalc_func <- function(x, a=4.4, ab= 2.9, b=29, f=16.2,del_growth = -8 , delR=-5, 
                        k25r=0.728, k25g=38.89, Ea_r = 72.311, Ea_g = 20.437,Rgc=8.314472){
  
  e = delR - del_growth
  
  x$CiCa <- x$Ci/x$CO2R
  x$a_prime <- (ab*(x$CO2S-x$C2sfc)+a*(x$C2sfc-x$Ci))/(x$CO2S-x$Ci)
  
  x$Rd <- k25r * exp(Ea_r*((x$Tleaf+273.15)-298)/(298*Rgc*(x$Tleaf+273.15)))
  x$Gstar <- k25g * exp(Ea_g*((x$Tleaf+273.15)-298)/(298*Rgc*(x$Tleaf+273.15)))
  
  x$rd_term <- e*x$Rd*(x$Ci-x$Gstar)/((x$Photo+x$Rd)*x$CO2S)
  x$f_term <- f*x$Gstar/x$CO2S
  
  x$TleafminusTair <- x$Tleafx$Tair
  x$TblockminusTair <- x$TBlkx$Tair
  
  x$CO2Rdry <- x$CO2R/(1-x$H2OR/1000)
  x$CO2Sdry <- x$CO2S/(1-x$H2OS/1000)
  
  x$t <- (1+x$a_prime/1000)*x$Trmmol/x$CndCO2/1000/2
  x$t2 <- 1/(1-x$t)
  x$t3 <- (1+x$t)/(1-x$t)
  
  x$Di <- x$a_prime * x$t2+ (x$t3 * b-x$a_prime * x$t2) * x$CiCa
  x$DiminusDo <- x$Dix$DELTA
  
  x$rd_term2 <- x$t3- x$rd_term
  x$f_term2 <- x$t3x$f_term
  
  x$gm <- x$t3 * (b1.8x$Rd * e / (x$Rd+x$Photo)) * x$Photo/x$CO2S/(x$DiminusDox$rd_term2x$f_term2)
  x$gm_bar <- x$gm*100/x$Press
  
  #different fractionation components as outputs--------------------------------------------------------------------
  
  #fractionation where Ci=Cc in the absence ot respiratory fractionation
  x$delta_i <- (x$t2*x$a_prime)+(x$t2*((1+x$t)*b-x$a_prime)*x$CiCa)
  
  #fractionation associated with the diffusion of CO2 from intercellular airspace to chloroplast 
  x$delta_gm = x$t3*(bx$a_prime(e*x$Rd)/(x$Photo+x$Rd)) * (x$Photo/(x$gm * x$CO2R))
  
  #most of the fractionation associated with respiration
  x$delta_e <- x$t3 * (((e * x$Rd)/((x$Photo + x$Rd) * x$CO2R))*(x$Cix$Gstar))
  
  #fractionation associated with photorespiration
  x$delta_f <- x$t3 * (f * (x$Gstar/x$CO2R))
  
  return(x)
}

gm <- gmcalc_func(cham_gmes2)
