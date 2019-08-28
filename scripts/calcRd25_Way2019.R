# Leaf-level measurements of Rdark at 25 C on the same WTC
Rdark <- read.csv('data/Rdark_Aspinwall.csv')
# fill in gap for March campaig: mean of Febuary and April
march <- data.frame(row.names=1:2)
march$Rdark <- c(mean(Rdark$Rdark[which(Rdark$month=='Feb' & Rdark$T_treatment=='ambient')],
                      Rdark$Rdark[which(Rdark$month=='Apr' & Rdark$T_treatment=='ambient')]),
                 mean(Rdark$Rdark[which(Rdark$month=='Feb' & Rdark$T_treatment=='warmed')],
                      Rdark$Rdark[which(Rdark$month=='Apr' & Rdark$T_treatment=='warmed')]))
march$T_treatment <- c('ambient', 'warmed')
march$month <- c('Mar','Mar')
Rdark <- rbind(Rdark, march)
# calculate Rlight at 25 C based on the empirical relationship of RKok:RDark
# figure 8a in Way et al. 2019
Rdark$Rd25 <- 0.62*Rdark$Rdark
rm(march)
