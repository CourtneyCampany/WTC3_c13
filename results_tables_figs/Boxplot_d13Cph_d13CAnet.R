source('scripts/canopy_gmes_more3.R')
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
chambs <- read.csv('data/trtkey2.csv')
phloem <- merge(phloem, chambs, by=c('chamber', 'month'), all=T)
phloem1 <- subset(phloem, month =='Oct' | month == 'Dec' | month == 'Jan')
phloem1 <- droplevels(phloem1)
phloem1$id <- as.factor(paste0(phloem1$month, '-', phloem1$T_treatment))
phloem1$id <- factor(phloem1$id, levels=c("Oct-ambient", "Oct-warmed", "Dec-ambient", "Dec-warmed", 
                                   "Jan-ambient", "Jan-warmed"))
phloem2 <- subset(phloem, month !='Oct' & month != 'Dec' & month != 'Jan')
phloem2 <- droplevels(phloem2)
phloem2$id <- as.factor(paste0(phloem2$month, '-', phloem2$T_treatment, '-', phloem2$W_treatment))
phloem2$id <- factor(phloem2$id, levels=c("Feb-ambient-control", "Feb-ambient-drydown", "Feb-warmed-control",
                                         "Feb-warmed-drydown", "Mar-ambient-control", "Mar-ambient-drydown",
                                         "Mar-warmed-control", "Mar-warmed-drydown", 'Apr-ambient-control',
                                         "Apr-ambient-drydown",'Apr-warmed-control', 'Apr-warmed-drydown'))

monthlab1 <- c("Oct", "Dec", "Jan")
monthlab2 <- c("Feb", "Mar", 'Apr')

#bar plot by month and treatment

windows(10, 10)
palette(c("blue", "red"))
par(mfrow=c(2, 2), mar=c(0,6,5,0), las=1, cex=1.1)
boxplot(d13Cph ~ id, data = phloem1, col=palette(), outline=FALSE, xaxt='n',
        ylab=expression(delta^13*C[ph]~~('\211')), xlab='', cex.lab=1.2, ylim=c(-33, -24.5))
legend('topleft', legend = '(a)', text.font = 2, bty='n')
par(mar = c(0, 0, 5, 6))
palette(c('blue', 'cornflowerblue', 'red', 'coral2'))
boxplot(d13Cph ~ id, data = phloem2, col=palette(), xaxt='n',
        ylab= ' ', xlab='', ylim=c(-33, -24.5), outline = FALSE, axes = F)
legend("topright", c("Ambient-Control", 'Ambient-Drought', 'Warm-Control', "Warm-Drought"), pch=22, pt.bg=palette(),  bty='n', pt.cex=1.5)
box()
legend('topleft', legend = '(b)', text.font = 2, bty='n')
par(mar=c(5, 6, 0, 0))
palette(c("blue", "red"))
boxplot(d13CAnet ~ id, data = phloem1, col=palette(), outline=FALSE, xaxt='n',
        ylab=expression(delta^13*C[Anet]~~('\211')), xlab='', cex.lab=1.2, ylim=c(-33, -24.5))
axis(1, at=c(1.5, 3.5, 5.5), labels=monthlab1, cex.lab=1.2)
legend('topleft', legend='(c)', text.font = 2, bty='n')
par(mar = c(5, 0, 0, 6))
palette(c('blue', 'cornflowerblue', 'red', 'coral2'))
boxplot(d13CAnet ~ id, data = phloem2, col=palette(), xaxt='n',
        ylab= ' ', xlab='', ylim=c(-33, -24.5), outline = FALSE, axes = F)
axis(1, at=c(3, 7, 11), labels=monthlab2, cex.lab=1.2)
box()
legend('topleft', legend='(d)', text.font = 2, bty='n')
