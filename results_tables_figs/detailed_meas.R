source('scripts/canopy_gmes_more3.R')


windows(10, 14)
par(mfrow=c(6, 2))
palette(c('blue', 'cyan', 'cornflowerblue'))
mar <- subset(allPaired, Date == as.Date('2014-03-22'))
marAmb <- subset(mar, T_treatment == 'ambient')
marAmb <- doBy::orderBy(~DateTime, marAmb)
marWarm <- subset(mar, T_treatment == 'warmed')
marWarm <- doBy::orderBy(~DateTime, marWarm)
par(mar=c(1.5, 6, 1.5, 0))
plot(marAmb$del13Camb ~ marAmb$Time, pch = 1, col = as.factor(marAmb$chamber),
     ylim = c(-14, -8.5), axes = F, xlim = c(7.5, 18),
     ylab = expression(delta^13*C~('\u2030')), xlab = ' ', cex.lab = 1.5)
axis(2, at=seq(-14, -9, 1), label = seq(-14, -9, 1))
axis(1, at=c(8, 11, 14, 17), label = c(8, 11, 14, 17))
box()
points(marAmb$del13Cch ~ marAmb$Time, pch = 19, col = as.factor(marAmb$chamber))
legend('bottomright', legend = c('amb', 'chm'), bty = 'n', pch = c(1, 19))
palette(c('red', 'orange', 'magenta'))
par(mar=c(1.5, 0, 1.5, 6))
plot(marWarm$del13Camb ~ marWarm$Time, pch = 1, col = as.factor(marWarm$chamber),
     ylim = c(-14, -8.5), xlim = c(7.5, 18),
     ylab = ' ', xlab = ' ', axes = F)
axis(4, at=seq(-14, -9, 1), label = seq(-14, -9, 1))
axis(1, at=c(8, 11, 14, 17), label = c(8, 11, 14, 17))
box()
points(marWarm$del13Cch ~ marWarm$Time, pch = 19, col = as.factor(marWarm$chamber))
box()

palette(c('blue', 'cyan', 'cornflowerblue'))
par(mar=c(1.5, 6, 1.5, 0))
plot(marAmb$DELTAi ~ marAmb$Time, ylim = c(4, 29), xlim = c(7.5, 18),
     pch = 1, col = as.factor(marAmb$chamber), cex.lab = 1.5, axes = F,
     ylab = expression(Delta^13*C~('\u2030')), xlab = '' )
points(marAmb$DELTAobs ~ marAmb$Time, pch = 19, col = as.factor(marAmb$chamber))
axis(2, at=seq(5, 25, 5), label=seq(5, 25, 5))
axis(1, at=c(8, 11, 14, 17), label = c(8, 11, 14, 17))
box()
legend('bottomright', bty = 'n', pch = c(1, 19),
       legend = c(expression(Delta[i]), expression(Delta[obs])))
palette(c('red', 'orange', 'magenta'))
par(mar=c(1.5, 0, 1.5, 6))
plot(marWarm$DELTAi ~ marWarm$Time, ylim = c(4, 29), xlim = c(7.5, 18),
     pch = 1, col = as.factor(marWarm$chamber), cex.lab = 1.5, axes = F,
     ylab = ' ', xlab = '' )
points(marWarm$DELTAobs ~ marWarm$Time, pch = 19, col = as.factor(marWarm$chamber))
axis(4, at=seq(5, 25, 5), label=seq(5, 25, 5))
axis(1, at=c(8, 11, 14, 17), label = c(8, 11, 14, 17))
box()

palette(c('blue', 'cyan', 'cornflowerblue'))
par(mar=c(1.5, 6, 1.5, 0))
plot(marAmb$A_area ~ marAmb$Time, ylim = c(-2, 13), axes = F,
     pch = 19, col = as.factor(marAmb$chamber), cex.lab = 1.5,  xlim = c(7.5, 18),
     ylab = expression(italic(A)[net]~(mu*mol~m^-2~s^-1)), xlab = '' )
axis(2, at=seq(0, 10, 5), label=seq(0, 10, 5))
axis(1, at=c(8, 11, 14, 17), label = c(8, 11, 14, 17))
box()
palette(c('red', 'orange', 'magenta'))
par(mar=c(1.5, 0, 1.5, 6))
plot(marWarm$A_area ~ marWarm$Time, ylim = c(-2, 13),xlim = c(7.5, 18),
     pch = 19, col = as.factor(marWarm$chamber), cex.lab = 1.5, axes = F,
     ylab = ' ', xlab = '' )
axis(4, at=seq(0, 10, 5), label=seq(0, 10, 5))
axis(1, at=c(8, 11, 14, 17), label = c(8, 11, 14, 17))
box()

palette(c('blue', 'cyan', 'cornflowerblue'))
par(mar=c(1.5, 6, 1.5, 0))
plot(marAmb$E_area ~ marAmb$Time, ylim = c(0, 2.6), axes = F, xlim = c(7.5, 18),
     pch = 19, col = as.factor(marAmb$chamber), cex.lab = 1.5, 
     ylab = expression(italic(E)~(mmol~m^-2~s^-1)), xlab = '' )
axis(2, at=seq(0, 2.5, .5), label=seq(0, 2.5, .5))
axis(1, at=c(8, 11, 14, 17), label = c(8, 11, 14, 17))
box()
palette(c('red', 'orange', 'magenta'))
par(mar=c(1.5, 0, 1.5, 6))
plot(marWarm$E_area ~ marWarm$Time, ylim = c(0, 2.6), xlim = c(7.5, 18),
     pch = 19, col = as.factor(marWarm$chamber), cex.lab = 1.5, axes = F,
     ylab = ' ', xlab = '' )
axis(4, at=seq(0, 2.5, 0.5), label=seq(0, 2.5, .5))
axis(1, at=c(8, 11, 14, 17), label = c(8, 11, 14, 17))
box()

palette(c('blue', 'cyan', 'cornflowerblue'))
par(mar=c(2.5, 6, 0.5, 0))
plot(marAmb$Ci ~ marAmb$Time, ylim = c(25, 415), axes = F, xlim = c(7.5, 18),
     pch = 19, col = as.factor(marAmb$chamber), cex.lab = 1.5, 
     ylab = expression(italic(C)[i]~(mu*mol~mol^-1)), xlab = '' )
axis(2, at=seq(100, 400, 100), label=seq(100, 400, 100))
axis(1, at=c(8, 11, 14, 17), label = c(8, 11, 14, 17))
box()
palette(c('red', 'orange', 'magenta'))
par(mar=c(2.5, 0, 0.5, 6))
plot(marWarm$Ci ~ marWarm$Time, ylim = c(25, 415), xlim = c(7.5, 18),
     pch = 19, col = as.factor(marWarm$chamber), cex.lab = 1.5, axes = F,
     ylab = ' ', xlab = '' )
axis(4, at=seq(100, 400, 100), label=seq(100, 400, 100))
axis(1, at=c(8, 11, 14, 17), label = c(8, 11, 14, 17))
box()

palette(c('blue', 'cyan', 'cornflowerblue'))
par(mar=c(4, 6, 0, 0))
plot(marAmb$gmes_area ~ marAmb$Time, ylim = c(0, 1.05), axes = F,
     pch = 19, col = as.factor(marAmb$chamber), cex.lab = 1.25,xlim = c(7.5, 18),
     ylab = expression(italic(g)[m]~(mol~m^-2~s^-1)), xlab = 'Time (h)' )
axis(1, at=c(8, 11, 14, 17), label = c(8, 11, 14, 17))
axis(2, at=seq(0, 1, .2), label=seq(0, 1, .2))
box()
palette(c('red', 'orange', 'magenta'))
par(mar=c(4, 0, 0, 6))
plot(marWarm$gmes_area ~ marWarm$Time, ylim = c(0, 1.05),
     pch = 19, col = as.factor(marWarm$chamber), cex.lab = 1.25, axes = F,
     ylab = ' ', xlab = 'Time (h)' , xlim = c(7.5, 18))
axis(1, at=c(8, 11, 14, 17), label = c(8, 11, 14, 17))
axis(4, at=seq(0, 1, .2), label=seq(0, 1, .2))
box()
