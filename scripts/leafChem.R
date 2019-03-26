trtkey <- read.csv("data/temp_trt.csv")
trtkey$chamber <- as.character(trtkey$chamber)
trtkey$chamber <- ifelse(nchar(trtkey$chamber)==1, paste0('C0', trtkey$chamber),
                         paste0('C',trtkey$chamber))
leafChem2 <- read.csv('data/leaf_chem.csv')[,c('chamber','leaf','Month','c13')]
names(leafChem2)[c(3,4)] <- c('month','d13Cleaf')
leafChem2$chamber <- paste0('C', substring(as.character(leafChem2$chamber),3,4))
leafChem2 <- merge(leafChem2, trtkey, by='chamber', all=T)
leafChemSun <- subset(leafChem2, leaf=='sun')[,c('chamber', 'month', 'd13Cleaf')]
names(leafChemSun)[ncol(leafChemSun)] <- 'd13CsunLeaf'
leafChemShade <- subset(leafChem2, leaf=='shade')[,c('chamber', 'month', 'd13Cleaf')]
names(leafChemShade)[ncol(leafChemShade)] <- 'd13CshLeaf'
leafChem <- merge(leafChemShade, leafChemSun, by=c('chamber','month'), all=T)
leafChem$d13CleafAvg <- rowMeans(leafChem[,c('d13CshLeaf','d13CsunLeaf')], na.rm=T)

leafChem2$id <- as.factor(paste0(leafChem2$month, '-', substring(leafChem2$temp, 1, 3), '-',
                       substring(leafChem2$leaf, 1, 2)))
leafChem2$id <- factor(leafChem2$id, levels=c('Oct-amb-su','Oct-amb-sh','Oct-ele-su','Oct-ele-sh',
                                              'Dec-amb-su','Dec-amb-sh','Dec-ele-su','Dec-ele-sh',
                                              'Jan-amb-su','Jan-amb-sh','Jan-ele-su','Jan-ele-sh',
                                              'Feb-amb-su','Feb-amb-sh','Feb-ele-su','Feb-ele-sh',
                                              'Mar-amb-su','Mar-amb-sh','Mar-ele-su','Mar-ele-sh',
                                              'Apr-amb-su','Apr-amb-sh','Apr-ele-su','Apr-ele-sh'))

windows(10,8)
par(mfrow=c(1,1), mar=c(4,6,1,1))
boxplot(d13Cleaf ~ id, data = leafChem2, col=c('blue','cornflowerblue','red','coral2'), axes=F,
        ylab=expression(Leaf~{delta}^13*C~~('\211')), cex.lab=1.5)
axis(2, at=c(seq(-33, -26, 1)), labels=c(at=c(seq(-33, -26, 1))))
axis(1, at=c(3, 7, 11, 15, 19, 23), labels=c('Oct','Dec','Jan','Feb','Mar','Apr'))
box()
legend("bottomleft", c("Amb-Su",'Amb-Sh',"Warm-Su",'Warm-Sh'), pch=22, pt.bg=c('blue','cornflowerblue','red','coral2'),
       bty='n', pt.cex=1.5)

summary(aov(d13Cleaf~month*leaf*temp, data=leafChem2))
