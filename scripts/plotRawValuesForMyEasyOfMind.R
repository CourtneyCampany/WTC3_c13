#selecting raw data for march stuff
marchTDLraw <- rbind(delta_files[[9]], delta_files[[10]])
#function for ploting raw TDL values
myRawPlot <- function(x, chamberNo, var){
  plot(subset(x, line=='ref' & chamber==chamberNo)[,var]~
         subset(x, line=='ref' & chamber==chamberNo)[,'datetimeFM'],
       ylim=c(min(x[,var]),max(x[,var])), ylab='', xlab='',
       main=paste0(var, ' Chamber ', chamberNo), type='l', lty=2)
  lines(subset(x, line=='samp' & chamber==chamberNo)[,var]~
         subset(x, line=='samp' & chamber==chamberNo)[,'datetimeFM'])
  legend('topleft', legend=c('Ref','Sample'), lty=c(2,1), bty='n')
}

#plot raw values for all chambers in 3 different graphs
windows(10,8)
par(mfrow=c(4,3))
for (i in 1:4){
  myRawPlot(marchTDLraw, as.character(i), 'CorrConcA_Avg')
  myRawPlot(marchTDLraw, as.character(i), 'CorrConcB_Avg')
  myRawPlot(marchTDLraw, as.character(i), 'Corrdel13C_Avg')
}
windows(10,8)
par(mfrow=c(4,3))
for (i in 5:8){
  myRawPlot(marchTDLraw, as.character(i), 'CorrConcA_Avg')
  myRawPlot(marchTDLraw, as.character(i), 'CorrConcB_Avg')
  myRawPlot(marchTDLraw, as.character(i), 'Corrdel13C_Avg')
}
windows(10,8)
par(mfrow=c(4,3))
for (i in 9:12){
  myRawPlot(marchTDLraw, as.character(i), 'CorrConcA_Avg')
  myRawPlot(marchTDLraw, as.character(i), 'CorrConcB_Avg')
  myRawPlot(marchTDLraw, as.character(i), 'Corrdel13C_Avg')
}