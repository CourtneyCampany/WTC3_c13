source("scripts/calculateCin.R")
palette(1:12)
feb <- subset(deltaPaired, DateTime >= as.Date('2014-02-01') & DateTime <= ('2014-02-28'))
plot(Air_in ~ DateTime, data = feb, col = as.factor(feb$chamber))
plot(Air_out ~ DateTime, data = feb)

c03 <- subset(deltaPaired, chamber == 'C03')
c04 <- subset(deltaPaired, chamber == 'C04' & Air_in >= 6)
c01 <- subset(deltaPaired, chamber == 'C01')
c06 <- subset(deltaPaired, chamber == 'C06')
firstDay <- c('2013-10-01', '2013-12-01', '2014-01-01', '2014-02-01', '2014-03-01', '2014-04-01')
lastDay <- c('2013-10-31', '2013-12-31', '2014-01-31', '2014-02-28', '2014-03-31', '2014-04-30')
names <- c('Oct-13', 'Dec-13', 'Jan-14', 'Feb-14', 'Mar-14', 'Apr-14')
windows(12, 8)
par(mfrow=c(2, 3), mar = c(3, 5, 4, 1))
for (i in 1:3){
  plot(Air_in ~ DateTime,
       data = subset(c01, DateTime >= as.Date(firstDay[i]) & DateTime <= as.Date(lastDay[i])),
       ylim = c(6, 7.55), type = 'l', col = 'blue', cex.lab = 1.3,
       xlab = '', ylab = expression(italic(f)~(std~L~s^-1)), main = names[i])
  lines(Air_out ~ DateTime,
        data = subset(c01, DateTime >= as.Date(firstDay[i]) & DateTime <= as.Date(lastDay[i])),
        col = 'blue', lty = 2)
  lines(Air_in ~ DateTime,
        data = subset(c06, DateTime >= as.Date(firstDay[i]) & DateTime <= as.Date(lastDay[i])),
        col = 'red')
  lines(Air_out ~ DateTime,
        data = subset(c06, DateTime >= as.Date(firstDay[i]) & DateTime <= as.Date(lastDay[i])),
        col = 'red', lty = 2)
}

legend('bottomright', bty = 'n', legend = c('in', 'out', 'amb', 'warm'), lty = c(1, 2, 1, 1),
       col = c('black', 'black', 'blue', 'red'))
windows(12, 8)
par(mfrow=c(2, 3), mar = c(3, 5, 4, 1))
for (i in 1:length(firstDay)){
  plot(CO2Injection ~ DateTime,
       data = subset(c03, DateTime >= as.Date(firstDay[i]) & DateTime <= as.Date(lastDay[i])),
       ylim = c(0, 0.16), type = 'l', col = 'blue', cex.lab = 1.3,
       xlab = '', ylab = expression(italic(C)[inj]~(mmol~s^-1)), main = names[i])
  lines(CO2Injection ~ DateTime,
        data = subset(c04, DateTime >= as.Date(firstDay[i]) & DateTime <= as.Date(lastDay[i])),
        col = 'red')
}
legend('topright', bty = 'n', legend = c('in', 'out', 'amb', 'warm'), lty = c(1, 2, 1, 1),
       col = c('black', 'black', 'blue', 'red'))

