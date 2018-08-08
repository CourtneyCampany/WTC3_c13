##this function was built for leaf level calculations


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
