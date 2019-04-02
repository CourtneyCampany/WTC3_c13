source('scripts/canopy_gmes_more.R')
source('master_scripts/phloem_plotting.R')
source('scripts/basicFunTEG.R')
source('scripts/leafChem.R')
phl <- phloem3
phl$chamber2 <- as.character(phl$chamber)
phl$chamber <- ifelse((nchar(phl$chamber2) == 1), paste0('C0', phl$chamber2), paste0('C', phl$chamber2))
phl$month <- str_sub(phl$month, 1, 3)
names(phl)[2] <- 'd13Cph'


del13CcampAvg <- doBy::summaryBy(Corrdel13C_Avg + CO2sampleWTC + Cc + diff_Ci.Cc ~ month + chamber,
                                 FUN=mean.na, data=subset(allPaired, PAR > 3 & condAlert=='no'
                                                          & A_area > 0 & E_area > 0))
del13CcampAvgMD <- doBy::summaryBy(iWUEge_corr + Corrdel13C_Avg + CO2sampleWTC + diff_Ci.Cc ~ month + chamber, FUN=mean.na,
                                   data=subset(allPaired, midday=='yes' & condAlert=='no' & A_area > 0 & E_area > 0))
names(del13CcampAvgMD)[3:ncol(del13CcampAvgMD)] <- paste0(names(del13CcampAvgMD)[3:ncol(del13CcampAvgMD)], 'MD')
phl <- merge(phl[,c('chamber','month','d13Cph','temp')],
             merge(del13CcampAvg, del13CcampAvgMD, by=c('month', 'chamber'), all=T),
             by=c('month','chamber'), all=T)
phl <- merge(phl, leafChem, by=c('month','chamber'), all=T)

phl$iWUEph_uncorrMD <- phl$CO2sampleWTC.mean.naMD*(1-((phl$Corrdel13C_Avg.mean.naMD-phl$d13Cph-a)/(b-a)))
phl$iWUEsunLeaf_uncorrMD <- phl$CO2sampleWTC.mean.naMD*(1-((phl$Corrdel13C_Avg.mean.naMD-phl$d13CsunLeaf-a)/(b-a)))
phl$iWUEshLeaf_uncorrMD <- phl$CO2sampleWTC.mean.naMD*(1-((phl$Corrdel13C_Avg.mean.naMD-phl$d13CshLeaf-a)/(b-a)))
phl$iWUEleafAvg_uncorrMD <- phl$CO2sampleWTC.mean.naMD*(1-((phl$Corrdel13C_Avg.mean.naMD-phl$d13CleafAvg-a)/(b-a)))
