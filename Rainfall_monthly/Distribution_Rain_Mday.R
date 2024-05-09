### giaves 2023-02-16
# 08458: Winter Floods 2019-21

# Read M-day rainfall maxima, choose best and acceptable distributions for each catchment, hence overall best

# Version 0.1: 2023-02-16. Initial development of code
# Version 0.2: 2023-11-01. Refactoring for wider distribution.

setwd("P:/08458 CWI-EA 2019-21 Flood Review")

##### SETUP #####
library(zoo)
library(readxl)
library(lmom)
library(lmomRFA)
library(readr)

##### key arguments #####
MDAY <- 30
rainfall_maxima_filepath <- paste0(
  "Data/Rainfall_long_duration/Maxima_table_",MDAY,"_day.csv")


##### Read in data #####
RainM <- readr::read_csv(rainfall_maxima_filepath)

ID <- unique(RainM$ID)

DistGrid <- array(NA, dim = c(length(ID), 5))
for (i in 1:length(ID)) {
  
  StRain <- RainM[RainM$ID == ID[i], ]
  Lmom <- lmom::samlmu(StRain[[paste0("Total",MDAY)]], nmom = 5)
  Lmom[2] <- Lmom[2] / Lmom[1]
  
  AddRow <- data.frame(Station = ID[i],
                       n = 130,
                       l_1 = Lmom[1],
                       t = Lmom[2],
                       t_3 = Lmom[3],
                       t_4 = Lmom[4],
                       t_5 = Lmom[5])
 
  DistGrid[i, 1:5] <- lmomRFA::regtst(AddRow, n = 10000)$Z
  
}

colnames(DistGrid) <- c("glo", "gev", "gno", "pe3", "gpa")

Valid_10 <- abs(DistGrid)
Valid_10[Valid_10 > 1.645] <- 0
Valid_10[Valid_10 > 0] <- 1

Valid_5 <- abs(DistGrid)
Valid_5[Valid_5 > 1.96] <- 0
Valid_5[Valid_5 > 0] <- 1

# How many distributions are valid at 10%
colSums(Valid_10)

# How many distributions are valid at 10%
colSums(Valid_5)

# Which distribution is the best per catchment
table(apply(abs(DistGrid), 1, which.min))

# What is the average Z-score per distribution
colMeans(abs(DistGrid))

DistGrid <- as.data.frame(DistGrid)
DistGrid$Station <- ID

#save to file
readr::write_csv(DistGrid, 
    paste0("./Data/Rainfall_long_duration/distribution_test", MDAY,"_day.csv"))
