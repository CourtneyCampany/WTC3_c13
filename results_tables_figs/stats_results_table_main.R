source('scripts/iWUE_comparison_Ubierna.R')

corrResults <- list()
corrResults[[1]] <- nlme::lme(iWUEgeMD ~ iWUEph_uncorrMD, random = ~1 | fchamber,
                              data = phl, na.action = na.omit)
corrResults[[2]] <- nlme::lme(iWUEgeMD ~ iWUEph_corrMD, random = ~1 | fchamber,
                              data = phl, na.action = na.omit)
corrResults[[3]] <- nlme::lme(iWUEgeMD ~ iWUEph_corrMDmyAv, random = ~1 | fchamber,
                              data = phl, na.action = na.omit)
corrResults[[4]] <- nlme::lme(iWUEgeMD ~ iWUEleafAvg_uncorrMD, random = ~1 | fchamber,
                              data = phl, na.action = na.omit)
corrResults[[5]] <- nlme::lme(iWUEgeMD ~ iWUEleafAvg_corrMD, random = ~1 | fchamber,
                              data = phl, na.action = na.omit)
corrResults[[6]] <- nlme::lme(d13Cph ~ d13CAnet, random = ~1 | fchamber,
                              data = phl, na.action = na.omit)
corrResults[[7]] <- nlme::lme(d13Cph ~ d13CAnet, random = ~1 | fchamber,
                              data = subset(phl, month!='Jan' & month!='Dec'), na.action = na.omit)
corrResults[[8]] <- nlme::lme(d13Cph ~ d13CleafAvg, random = ~1 | fchamber,
                              data = phl, na.action = na.omit)

table1 <- data.frame(row.names = 1:length(corrResults))
for (i in 1:length(corrResults)){
  table1$one[i] <- stringi::stri_split_fixed(as.character(summary(corrResults[[i]])$call[2]), "~")[[1]][1]
  table1$two[i] <- stringi::stri_split_fixed(as.character(summary(corrResults[[i]])$call[2]), "~")[[1]][2]
  table1$intercept[i] <- round(summary(corrResults[[i]])$tTable[1], 1)
  table1$interceptSE[i] <- round(summary(corrResults[[i]])$tTable[3], 1)
  table1$interceptP[i] <- round(summary(corrResults[[i]])$tTable[9], 3)
  table1$slope[i] <- round(summary(corrResults[[i]])$tTable[2], 2)
  table1$slopeSE[i] <- round(summary(corrResults[[i]])$tTable[4], 2)
  table1$slopeP[i] <- round(summary(corrResults[[i]])$tTable[10], 3)
  table1$R2m[i] <- round(MuMIn::r.squaredGLMM(corrResults[[i]])[1], 2)
  table1$R2c[i] <- round(MuMIn::r.squaredGLMM(corrResults[[i]])[2], 2)
}
write.csv(table1, file='results_tables_figs/table1.csv', row.names=F)
