# Get processed data from Drake's repository
cue.day.trt <- read.csv('data/cue.day.trt.csv')
cue.day.trt$Date <- as.Date(lubridate::ymd(as.character(cue.day.trt$Date)))

windows(12,8)
par(mfrow=c(2,1))
par(mar=c(0,4.5,4.5,2))
plot(subset(cue.day.trt, T_treatment=='elevated')[,'T_day.mean']~
       subset(cue.day.trt, T_treatment=='elevated')[,'Date'], type='l', col="deeppink",
     ylab=expression(T[day]~(degree*C)), xlab='', ylim=c(12,35), lwd=2.5)
lines(subset(cue.day.trt, T_treatment=='ambient')[,'T_day.mean']~
        subset(cue.day.trt, T_treatment=='ambient')[,'Date'], col="cornflowerblue", lwd=2.5)
legend('topright', legend=c('Amb.', 'Warm'), lwd=2, col=c('cornflowerblue','deeppink'), bty='n')

par(mar=c(4.5,4.5,0,2))
plot(subset(cue.day.trt, T_treatment=='elevated')[,'GPP_la.mean']*1/12~
       subset(cue.day.trt, T_treatment=='elevated')[,'Date'], type='l', col="deeppink",
     ylab=expression(A[net]~mol~m^-2~s^-1), xlab='', ylim=c(0,0.6), lwd=2.5)
lines(subset(cue.day.trt, T_treatment=='ambient')[,'GPP_la.mean']*1/12~
        subset(cue.day.trt, T_treatment=='ambient')[,'Date'], col="cornflowerblue", lwd=2.5)

