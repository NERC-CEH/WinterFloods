### giaves 2023-02-16
# EA project 35752: Hydrological analysis of the 2019-2021 flooding

# Read M-day rainfall maxima, choose best and acceptable distributions for each catchment, hence overall best

# Version 0.1: 2023-02-16. Initial development of code
# Version 0.2: 2023-11-01. Refactoring for wider distribution.
# Version 1.0: 2024-07-22. Final version for wider distribution.


##### SETUP #####
library(zoo)
library(readxl)
library(lmom)
library(lmomRFA)
library(readr)

##### key arguments #####
MDAY <- 30 # modelled days per month. Change to 30, 60, 90, or 180.
rainfall_maxima_filepath <- paste0(
  "Data/Rainfall_long_duration/Maxima_table_",MDAY,"_day.csv") # location of file including value of depth, rank and RP.

distribution_output <-  paste0("./Data/Rainfall_long_duration/distribution_test", MDAY,"_day.csv") # output file showing best distribution for further analysis.

##### Read in data #####
RainM <- readr::read_csv(rainfall_maxima_filepath)

ID <- unique(RainM$ID)

DistGrid <- array(NA, dim = c(length(ID), 5))
for (i in 1:length(ID)) { # for each station 
  
  # get station name
  StRain <- RainM[RainM$ID == ID[i], ]
  # fit L-moments
  Lmom <- lmom::samlmu(StRain[[paste0("Total",MDAY)]], nmom = 5)
  Lmom[2] <- Lmom[2] / Lmom[1]
  
  AddRow <- data.frame(Station = ID[i],
                       n = 130,
                       l_1 = Lmom[1],
                       t = Lmom[2],
                       t_3 = Lmom[3],
                       t_4 = Lmom[4],
                       t_5 = Lmom[5])

  # check regional distribution test for best distribution.
  DistGrid[i, 1:5] <- lmomRFA::regtst(AddRow, n = 10000)$Z
  
}

# outputs for saving
colnames(DistGrid) <- c("glo", "gev", "gno", "pe3", "gpa")


Valid_10 <- abs(DistGrid)
Valid_10[Valid_10 > 1.645] <- 0
Valid_10[Valid_10 > 0] <- 1

Valid_5 <- abs(DistGrid)
Valid_5[Valid_5 > 1.96] <- 0
Valid_5[Valid_5 > 0] <- 1

##### RESULTS #####
# Prints the following to the console:
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


##### SAVE OUTPUTS #####
#save to file
readr::write_csv(DistGrid, distribution_output)
