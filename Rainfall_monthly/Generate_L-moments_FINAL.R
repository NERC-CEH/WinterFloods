### giaves 2023-02-16
# EA project 35752: Hydrological analysis of the 2019-2021 flooding

# Main contributor: Gianni Vesuviano
# Read 1, 2, 3 and 6-month AMAX, calculate and write L-moments, median, and other derived statistics

# Version 0.1: 2023-02-16. Initial development of code
# Version 0.2: 2023-11-01. Refactoring for wider distribution.
# Version 1.0: 2024-07-22. Final version for wider distribution.

### NOTE: source raingauge data not given as data product in this project.


##### SETUP #####
library(lmom)

monthly_rain_filepath <- "./Data/Rainfall_long_duration" #folder containing long duration rainfall timeseries



## read in folder
RM <- list.files(monthly_rain_filepath, pattern = "Maxima_table")
months <- substr(RM, 14,24)

N <- length(RM)
M <- 158 #number of catchments of interest

##### ANALYSIS #####
LMOM <- array(0, dim = c(M, 8))
for (i in 1:N) { # for each maxima tablr
  MT <- (read.csv(RM[i]))
  LM <- t(apply(MT, 2, samlmu)) # calculate L-moments
  LM <- LM[2:(M+1), ]
  ME <- apply(MT, 2, median) #calculate per-catchment medians
  ME <- ME[2:(M+1)]
  L2 <- LM[ , 2] / ME  #L-CV with median
  T2 <- LM[ , 2] / LM[ , 1] #L-CV with mean
  LMOM[ , 1] <- gsub("[^0-9]", "", colnames(MT))[2:(M+1)] #catchment name
  LMOM[ , 2:5] <- LM #L-moments
  LMOM[ , 6] <- ME
  LMOM[ , 7] <- L2
  LMOM[ , 8] <- T2
  colnames(LMOM) <- c("Station", "l1", "l2", "t3", "t4", "rmed", "l2med", "t2")
  write.csv(LMOM, file = sprintf("./Data/rainfall_monthly/Lmoms_%s", months[i]),
            row.names = FALSE, quote = FALSE)
}
