#selecting raw data for march stuff

marchTDLraw <- rbind(delta_files[[9]], delta_files[[10]])
#plot raw values
windows(12,8)
par(mfrow=c(2,3))
plot(subset(marchTDLraw, line=='ref' & chamber=='1')[,'CorrConcA_Avg']~
       subset(marchTDLraw, line=='ref' & chamber=='1')[,'datetimeFM'],
     type='l', ylim=c(375, 590), ylab='Corr Conc A (ppm)', xlab='', main='Reference')
chambernames <- as.character.Date(c(2:12))
for (i in )