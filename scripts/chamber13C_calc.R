
flux_files <- list.files(path = "raw data/chamber_13C_clean/", pattern="chflux", full.names = TRUE)
  ##make names of list with file names minus extension
  flux_names <- gsub("raw data/chamber_13C_clean/", "", flux_files)
  flux_names <- gsub(".csv", "", flux_files)
  flux_names <- gsub("chflux_", "", flux_files)

flux_months <- lapply(flux_files, function(x) read.csv(x, stringsAsFactors = FALSE))
  ##add names to list
   names(flux_months) <- flux_names


chamber13_func <- function(x){ #run on tdl formatted lists
  x$Date <- as.Date(x$Date, format = "%d/%m/%Y")
  x$datetime <- paste(x$Date, x$time, sep=" ")
  library(lubridate)
  x$datetime <- ymd_hms(x$datetime)

  #remove reference gases
  dat <- x2[x2$SiteOutput != 3 & x2$SiteOutput != 4, c("SiteOutput","Corrdel13C_Avg")]
  #subset even gas lines as they are the sample line that reprsent chamber13C
  is.even <- function(v) v %% 2 == 0
  dat2 <- dat[which(is.even(dat$SiteOutput)),]
  #caluclate mean corr del from gmes eq
  dat3 <- mean(dat2$Corrdel13C_Avg)
  dat3 <- as.data.frame(dat3)
  return(dat3)
}

library(plyr)
test <- flux_months[5]
test2 <- as.data.frame(test)

c13chamber <- ldply(flux_months, function(x) chamber13_func(x))
mean_c13chamber <- mean(c13chamber[,1])
