source('scripts/canopy_gmes.R')
#function to fit the USO from Medlyn et al. 2011 GCB
fitUSO <- function(df){
  myFit <- nls(gc ~ g0+(1+g1/sqrt(D))*(Photo/(CO2S)), start=list(g0=0,g1=3), data=df)
  return(myFit)
}
chambs <- c(paste0('C0', 1:9), paste0('C', 10:12))
camps <- c('Oct','Dec','Jan','Feb','Mar','Apr')
wtcL <- list()
crap <- list()
for (i in 1:length(chambs)){
  crap[[i]] <- subset(allPaired, chamber==chambs[1])
  for(j in 1:length(camps)){
    wtcList
  }
}
funSubsetCamps <- function(df, campsV){
  empty <- list()
  for(i in 1:length(campsV)){
    empty[[i]] <- subset(df, month==campsV[i])
  }
  return(empty)
}
wtcL <- lapply(wtcList, function(x) funSubsetCamps(x, campsV=camps))
