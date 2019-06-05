phloem <- read.csv("data/phloem_c13.csv")
  phloem$chamber <- as.factor(phloem$chamber)

trtkey <- read.csv("data/temp_trt.csv")

#add warming treatments
phloem2 <- merge(phloem, trtkey)
  

#add monthly 
campaignassign <- function(x) {
  x$campaign <- as.factor(x$campaign)
  x$month <- ifelse(x$campaign == 1,"October", "missing")
  x$month <- ifelse(x$campaign == 2, "December",  x$month)
  x$month <- ifelse(x$campaign == 3, "January", x$month)
  x$month <- ifelse(x$campaign == 4, "February",  x$month)
  x$month <- ifelse(x$campaign == 5, "March", x$month)
  xmonth <- as.factor(x$month)
  
  x$id <- with(x, paste(month, temp, sep="-"))
  return(x)
}


phloem3 <- campaignassign(phloem2)
phloem3$id <- factor(phloem3$id, c("October-ambient", "October-elevated", "December-ambient", "December-elevated", 
                                   "January-ambient", "January-elevated", "February-ambient", "February-elevated",
                                   "March-ambient","March-elevated"))

monthlab <- c("October", "December", "January", "February", "March")
#bar plot by month and treatment

windows()
palette(c("cornflowerblue", "red"))
par(mar=c(5,5,2,2))
boxplot(d13C ~ id, data = phloem3, col=palette(), outline=FALSE,at=c(1,2, 7,8, 10,11, 13,14, 16:17), xaxt='n',
        ylab=expression(Phloem~{delta}^13*C~~('\211')))
 axis(1, at=c(1.5, 7.5, 10.5, 13.5, 16.5), labels=monthlab)
legend("bottomleft", c("Ambient", "Elevated"), pch=22, pt.bg=palette(),  bty='n', pt.cex=1.5)
dev.copy2pdf(file="output/deltaphloem.pdf")
dev.off()
phl <- phloem3
phl$chamber2 <- as.character(phl$chamber)
phl$chamber <- ifelse((nchar(phl$chamber2) == 1), paste0('C0', phl$chamber2), paste0('C', phl$chamber2))
phl$month <- str_sub(phl$month, 1, 3)
names(phl)[2] <- 'd13Cph'
rm(phloem, phloem2, phloem3, trtkey)