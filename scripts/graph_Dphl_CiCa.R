source('scripts/canopy_gmes_more3.R')
source('master_scripts/phloem_plotting.R')
phl$fchamber <- as.factor(phl$chamber)
phl$month <- factor(phl$month, levels=c('Oct','Dec','Jan','Feb','Mar','Apr'))
del13CcampAvgMD <- dplyr::summarise(dplyr::group_by(setDT(subset(allPaired, A_area > 0 & E_area > 0
                                                                 & PAR >= 800 & midday=='yes' & iWUE < 400)),
                                                    month, chamber), 
                                    Ci.CaMD=mean.na(Ci.Ca), d13chMD=mean.na(del13Cch))
phl <- merge(del13CcampAvgMD, phl, by=c('month','chamber'), all.y = T, all.x = F)

chambs <- read.csv('data/trtkey2.csv')

phl <- merge(phl, chambs, by=c('chamber', 'month'), all.x = T, all.y = F)
# calculate DELTAobs with d13Cph
phl$DELTAph <- (phl$d13chMD - phl$d13Cph) * 1000/(1000 + phl$d13Cph)
windows(12, 8)
par(mfrow=c(1,2), mar = c(5, 6, 2, 1))
myMon <- c('Oct', 'Dec', 'Jan', 'Feb', 'Mar')
mySym <- c(21:25)
plot(subset(phl, month == 'Oct' & T_treatment == 'ambient')[, 'DELTAph'] ~
       subset(phl, month == 'Oct' & T_treatment == 'ambient')[, 'Ci.CaMD'],
     pch = 21, col = 'blue', bg = 'blue', ylim = c(16, 22), xlim = c(0.3, 0.8),
     ylab = expression(Delta[phloem]~(permil)), xlab = 'Ci/Ca', cex = 1.5, cex.lab = 1.5)
for(i in 2:length(myMon)){
  points(subset(phl, month == myMon[i] & T_treatment == 'ambient')[, 'DELTAph'] ~
           subset(phl, month == myMon[i] & T_treatment == 'ambient')[, 'Ci.CaMD'],
         pch = mySym[i], col = 'blue', bg ='blue', cex = 1.5)
}
for(i in 1:length(myMon)){
  points(subset(phl, month == myMon[i] & T_treatment == 'warmed')[, 'DELTAph'] ~
           subset(phl, month == myMon[i] & T_treatment == 'warmed')[, 'Ci.CaMD'],
         pch = mySym[i], col = 'red', bg ='red', cex = 1.5)
}
plot(subset(phl, month == 'Oct' & T_treatment == 'ambient')[, 'DELTAph'] ~
       subset(phl, month == 'Oct' & T_treatment == 'ambient')[, 'Ci.CaMD'],
     pch = 21, col = 'blue', bg = 'blue', ylim = c(0, 22), xlim = c(0, 0.8),
     ylab = expression(Delta[phloem]~(permil)), xlab = 'Ci/Ca', cex = 1.5, cex.lab = 1.5)
for(i in 2:length(myMon)){
  points(subset(phl, month == myMon[i] & T_treatment == 'ambient')[, 'DELTAph'] ~
           subset(phl, month == myMon[i] & T_treatment == 'ambient')[, 'Ci.CaMD'],
         pch = mySym[i], col = 'blue', bg ='blue', cex = 1.5)
}
for(i in 1:length(myMon)){
  points(subset(phl, month == myMon[i] & T_treatment == 'warmed')[, 'DELTAph'] ~
           subset(phl, month == myMon[i] & T_treatment == 'warmed')[, 'Ci.CaMD'],
         pch = mySym[i], col = 'red', bg ='red', cex = 1.5)
}
model <- lm(DELTAph ~ 0 + Ci.CaMD, data = phl, offset(rep(4.4, length(Ci.CaMD))))
abline(4.4, model$coefficients)
legend('bottomright', pch = c(19, 19, 21:25), col = c('blue', 'red', rep('black', 5)),
       bty = 'n', legend = c('amb', 'warm', myMon))
legend('bottomleft', legend = c('Intercept: 4.4', 'Slope: 23.4 = 27.8 - 4.4'), bty = 'n')
