library(HIEv)
setToken()
morpho <- downloadCSV(filename="WTC_TEMP_CM_HARVEST-CANOPY_20140526-20140528_L1_v1.csv")
morpho$totalLeafArea <- morpho$TotalLeafDM * morpho$SLA/1e4
treeLeaf <- doBy::summaryBy(totalLeafArea ~ chamber, FUN=sum, data=morpho)
colnames(treeLeaf)[2] <- 'leafArea'
rm(morpho)