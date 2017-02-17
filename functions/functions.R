
# global functions --------------------------------------------------------



# add measurement month ---------------------------------------------------
add_Month<- function(x){
  
  x$Month <-ifelse(x$campaign == 1, "Oct", x$campaign)
  x$Month <-ifelse(x$campaign == 2, "Dec", x$Month )
  x$Month <-ifelse(x$campaign == 3, "Jan", x$Month )
  x$Month <-ifelse(x$campaign == 4, "Feb", x$Month )
  x$Month <-ifelse(x$campaign == 5, "Mar", x$Month )
  x$Month <-ifelse(x$campaign == 6, "Apr", x$Month )
  x$Month <- as.factor(x$Month)
  return(x)
}

#add treatments------------------------------------------------------------------------------------------------------
addtrt_func <- function(x){
  x <- merge(x, treatments)
  x$temp <- as.factor(x$temp)
  x$drydown <- as.factor(ifelse(x$Month %in% c("Mar", "Apr") & x$chamber %in%c("ch01", "ch03", "ch04", "ch06", "ch08", "ch11"), 
                                "drought", "control"))
  return(x)
}
