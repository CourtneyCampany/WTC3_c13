source('scripts/canopy_gmes_more2.R')
photoSumm <- dplyr::summarise(dplyr::group_by(setDT(subset(allPaired, midday=='yes' & A_area > 0 & PAR >= 800
                                                           & deltaSubstrate >= -60 & deltaSubstrate <= 0)),
                                              chamber, month), d13CAnet=mean(deltaSubstrate, na.rm=T))
phloem <- read.csv("data/phloem_c13.csv")
phloem$chamber2 <- phloem$chamber
phloem$chamber <- ifelse((nchar(as.character(phloem$chamber2)) == 1), paste0('C0', phloem$chamber2),
                         paste0('C', phloem$chamber2))

#add monthly 
campaignassign <- function(x) {
  x$campaign <- as.factor(x$campaign)
  x$month <- ifelse(x$campaign == 1,"Oct", "missing")
  x$month <- ifelse(x$campaign == 2, "Dec",  x$month)
  x$month <- ifelse(x$campaign == 3, "Jan", x$month)
  x$month <- ifelse(x$campaign == 4, "Feb",  x$month)
  x$month <- ifelse(x$campaign == 5, "Mar", x$month)
  x$month <- as.factor(x$month)
  return(x)
}
phloem <- campaignassign(phloem)
names(phloem)[1] <- 'd13Cph'
phloem <- merge(phloem, photoSumm, by=c('chamber','month'), all=T)
chambs <- data.frame(row.names = 1:12)
chambs$temp <- rep(c('ambient', 'warmed'), 6)
chambs$chamber <- c(paste0('C0', 1:9), paste0('C', 10:12))
phloem <- merge(phloem, chambs, by='chamber', all=T)
phloem$id <- as.factor(paste0(phloem$month, '-', phloem$temp))
phloem$id <- factor(phloem$id, levels=c("Oct-ambient", "Oct-warmed", "Dec-ambient", "Dec-warmed", 
                                   "Jan-ambient", "Jan-warmed", "Feb-ambient", "Feb-warmed",
                                   "Mar-ambient","Mar-warmed", 'Apr-ambient', 'Apr-warmed'))

monthlab <- c("Oct", "Dec", "Jan", "Feb", "Mar", 'Apr')


#bar plot by month and treatment

windows(7, 10)
palette(c("blue", "red"))
par(mfrow=c(2, 1), mar=c(0,6,5,0.5), las=1, cex=1.1)
boxplot(d13Cph ~ id, data = phloem, col=palette(), outline=FALSE, xaxt='n',
        ylab=expression(delta^13*C[ph]~~('\211')), xlab='', cex.lab=1.2, ylim=c(-33, -24.5))
legend("bottomleft", c("Ambient", "Elevated"), pch=22, pt.bg=palette(),  bty='n', pt.cex=1.5)
legend('topleft', legend = '(a)', text.font = 2, bty='n')
par(mar=c(5, 6, 0, 0.5))
boxplot(d13CAnet ~ id, data = phloem, col=palette(), outline=FALSE, xaxt='n',
        ylab=expression(delta^13*C[Anet]~~('\211')), xlab='', cex.lab=1.2, ylim=c(-33, -24.5))
axis(1, at=c(1.5, 3.5, 5.5, 7.5, 9.5, 11.5), labels=monthlab, cex.lab=1.2)
legend('topleft', legend='(b)', text.font = 2, bty='n')
