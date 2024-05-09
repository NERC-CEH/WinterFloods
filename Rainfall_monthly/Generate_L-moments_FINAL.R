### giaves 2023-02-16
# 08458: Winter Floods 2019-21

# Main contributor: Gianni Vesuviano
# Read 1, 2, 3 and 6-month AMAX, calculate and write L-moments, median, and other derived statistics

# Version 0.1: 2023-02-16. Initial development of code
# Version 0.2: 2023-11-01. Refactoring for wider distribution.

### NOTE: source raingauge data not given as data product in this project.

monthly_rain_filepath <- "./Data/Rainfall_long_duration" 

##### SETUP 
library("lmom")

RM <- list.files(monthly_rain_filepath, pattern = "Maxima_table")
months <- substr(RM, 14,24)

N <- length(RM)
M <- 158

LMOM <- array(0, dim = c(M, 8))
for (i in 1:N) {
  MT <- (read.csv(RM[i]))
  LM <- t(apply(MT, 2, samlmu))
  LM <- LM[2:(M+1), ]
  ME <- apply(MT, 2, median)
  ME <- ME[2:(M+1)]
  L2 <- LM[ , 2] / ME
  T2 <- LM[ , 2] / LM[ , 1]
  LMOM[ , 1] <- gsub("[^0-9]", "", colnames(MT))[2:(M+1)]
  LMOM[ , 2:5] <- LM
  LMOM[ , 6] <- ME
  LMOM[ , 7] <- L2
  LMOM[ , 8] <- T2
  colnames(LMOM) <- c("Station", "l1", "l2", "t3", "t4", "rmed", "l2med", "t2")
  write.csv(LMOM, file = sprintf("./Data/rainfall_monthly/Lmoms_%s", months[i]),
            row.names = FALSE, quote = FALSE)
}
