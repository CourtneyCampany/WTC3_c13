source('scripts/theGoodiWUEcorrV2.R')
windows(12, 8)
par(mfrow=c(1,3), mar = c(6, 6, 4, 0), cex = 1.2)
palette <- c('cornflowerblue', 'red')
plot(phl$d13Cph ~ phl$d13CsunLeaf, pch = 19, col = as.factor(phl$temp),
     xlab = '', ylab = expression(delta^13*C[ph]~('\211')),
     xlim = c(-33, -26), main = 'Sun leaves', cex.lab = 1.5)
abline(lm(phl$d13Cph ~ phl$d13CsunLeaf))
legend('topleft', legend = c(expression(italic(R)^2~'='~0.18), expression(italic(P)~'<'~0.001)),
       bty = 'n')
par(mar = c(6, 3, 4, 3), cex = 1.2)
plot(phl$d13Cph ~ phl$d13CshLeaf, pch = 19, col = as.factor(phl$temp),
     xlab = expression(delta^13*C[leaf]~('\211')), ylab = '', 
     xlim = c(-33, -26), main = 'Shade leaves', cex.lab = 1.5)
abline(lm(phl$d13Cph ~ phl$d13CshLeaf))
legend('topleft', legend = c(expression(italic(R)^2~'='~0.06), expression(italic(P)~'='~0.031)),
       bty = 'n')
par(mar = c(6, 0, 4, 6), cex = 1.2)
plot(phl$d13Cph ~ phl$d13CleafAvg, pch = 19, col = as.factor(phl$temp),
     xlab = '', ylab = '', 
     xlim = c(-33, -26), main = 'Mean Sun & Shade', cex.lab = 1.5)
abline(lm(phl$d13Cph ~ phl$d13CleafAvg))
legend('topleft', legend = c(expression(italic(R)^2~'='~0.18), expression(italic(P)~'<'~0.001)),
       bty = 'n')
legend('bottomright', legend = c('Amb', 'Warm'), pch = 19,
       col = c('cornflowerblue', 'red'), bty = 'n')