# Get processed data from Drake's repository
cue.day.trt <- read.csv('data/cue.day.trt.csv')
cue.day.trt$Date <- as.Date(lubridate::ymd(as.character(cue.day.trt$Date)))

dates <- read.csv('data/TDLdates.csv')
dates$dum <- rep(18, times=nrow(dates))
dates$Date <- as.Date(lubridate::dmy_hm(as.character(dates$Start)))
cue.day.trt <- merge(cue.day.trt, dates[,c('Date','dum')], by='Date', all=T)

windows(10,8)
par(mfrow=c(1,1))
par(mar=c(4,6,2,2))
plot(subset(cue.day.trt, T_treatment=='elevated')[,'T_day.mean']~
       subset(cue.day.trt, T_treatment=='elevated')[,'Date'], type='l', col="deeppink",
     ylab=expression(italic(T)[day]~(degree*C)), xlab='', ylim=c(12,35), lwd=2.5, cex.lab=1.8, cex.axis=1.5)
lines(subset(cue.day.trt, T_treatment=='ambient')[,'T_day.mean']~
        subset(cue.day.trt, T_treatment=='ambient')[,'Date'], col="cornflowerblue", lwd=2.5)
points(cue.day.trt$dum~cue.day.trt$Date, pch=24, col='black', bg='black', cex=1.8)
points(cue.day.trt$dum~cue.day.trt$Date, pch=25, col='black', bg='black', cex=1.8)
legend('topright', legend=c('Amb.', 'Warm'), lwd=2, col=c('cornflowerblue','deeppink'), bty='n')

par(mar=c(4.5,6,0,2))
plot(subset(cue.day.trt, T_treatment=='elevated')[,'GPP_la.mean']*1/12~
       subset(cue.day.trt, T_treatment=='elevated')[,'Date'], type='l', col="deeppink",
     ylab=expression(italic(A)[net]~mol~m^-2~day^-1), xlab='', ylim=c(0,0.6), lwd=2.5, cex.lab=1.9)
lines(subset(cue.day.trt, T_treatment=='ambient')[,'GPP_la.mean']*1/12~
        subset(cue.day.trt, T_treatment=='ambient')[,'Date'], col="cornflowerblue", lwd=2.5)
points(cue.day.trt$dum~cue.day.trt$Date, pch=8, cex=2.5)
legend('topright', legend=c('Redrawn from Drake et al. (2016)','TDL & Phloem Campaign'), pch=c(NA, 8), bty='n')


