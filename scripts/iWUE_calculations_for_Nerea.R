# read the data
phl <- read.csv('march_for_nerea.csv')
# calculate Delta-obs from d13C of the phloem
phl$DELTAph <- (phl$d13chMD - phl$d13Cph) * 1000/(1000 + phl$d13Cph)
# caclulate Delta-obs incorporating the presumed effect of post-photosynthetic fractionation:
# d13Cph_corr = d13Cph + 2.5
# 2.5 permil is the difference between d13C of the phloem and d13C of photosynthesis (calculated from online d13C measurements)
phl$DELTAph_corr <- (phl$d13chMD - phl$d13Cph_corr) * 1000/(1000 + phl$d13Cph_corr)
# calcule iWUE from Delta with b = 27, Eq. 4 in the main text
phl$iWUEph_uncorrMD <- (phl$CO2chMD/1.6)*((27-phl$DELTAph)/(27-a))
# calculate iWUE from Delta incorporating gm and b = 29. Eq. 9 in the main text
phl$iWUEph_corrMD <- (phl$CO2chMD*(b-phl$DELTAph)+(ai-b)*(phl$AMD/phl$gmMD))/(1.6*(b-a))
# calculate iWUE using d13Cph corrected for presumed post-photosynthetic fractionation
phl$iWUEph_corrMDmyAv <- (phl$CO2chMD*(b-phl$DELTAph_corr)+(ai-b)*(phl$AMD/phl$gmMD))/(1.6*(b-a))
# calculate DELTAobs from d13C of the leaf
phl$DELTAleafAvg <- (phl$d13chMD - phl$d13CleafAvg) * 1000/(1000 + phl$d13CleafAvg)
# calculate iWUE from d13Cleaf 
phl$iWUEsunLeaf_corrMD <- (phl$CO2chMD*(b-phl$DELTAsunLeaf)+(ai-b)*(phl$AMD/phl$gmMD))/(1.6*(b-a))
phl$iWUEsunLeaf_uncorrMD <- (phl$CO2chMD/1.6)*((27-phl$DELTAsunLeaf)/(27-a))