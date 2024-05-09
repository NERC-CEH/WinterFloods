### giaves 2023-10-25
# 08458: Winter Floods 2019-21

# Find event return periods for depths extracted by H24_event_totals.R

# Version 0.1: 2023-10-25. Initial development of code
# Version 0.2: 2023-11-01. Refactoring for wider distribution.


#### SETUP ####
library(xml2)



#### KEY FILEPATHS ####
rainfall_events_in <- "./Data/CatAvg_rainfall_totals.csv"
rainfall_FEH_out <- "./Data/CatAvg_rainfall_RPs_from_FEH22_grid_extraction.csv"

#### READ IN DATA ####
Events <- read.csv(rainfall_events_in)

NEv <- dim(Events)[1]



# Part 1 not needed - script adapted from point rainfall RP calculation script

##########################################
#  Part 2: extract depths for specified  #
#    return periods from FEH22 rasters   #
##########################################

# Convert fixed to sliding rainfall
Events$Depth.1h <- Events$Depth.1h * 1.04 # estimate that sliding 1-hour rainfall depths should be 4% higher due to 15-minute data resolution

Extracted_rainfall_from_csv_1h <- read.csv(
	"./Data/Catchment Rainfall Event Data/Station_FEH22_depths_1h.csv")[ , 6:29]
Extracted_rainfall_from_csv_6h <- read.csv(
	"./Data/Catchment Rainfall Event Data/Station_FEH22_depths_6h.csv")[ , 6:29]
Extracted_rainfall_from_csv_1d <- read.csv(
	"./Data/Catchment Rainfall Event Data/Station_FEH22_depths_1d.csv")[ , 6:29]
Extracted_rainfall_from_csv_4d <- read.csv(
	"./Data/Catchment Rainfall Event Data/Station_FEH22_depths_4d.csv")[ , 6:29]

Metadata <- read.csv("Data/H24 Results/Station_FEH22_depths_1h.csv")[ , 1:5]



#################################
#  Part 3: find return periods  #
#  using 4-point interpolation  #
#################################

source("./I4PLA.R")
RPList <- c(1.3, 1.58, 2, 3, 6, 10, 18, 31, 56, 100, 180, 310, 560, 1000,
			1800, 3100, 5600, 10000, 18000, 31000, 56000, 100000)

RP_AMAX_1h <- rep(NA, NEv)
for (i in 1:NEv) {
  
  if (length(which(Metadata$NRFA_ID == Events$NRFA[i]) == 0) == 0) {
    next()
  }
  
  Site_extracted_rainfall <- unlist(Extracted_rainfall_from_csv_1h[
	which(Metadata$NRFA_ID == Events$NRFA[i])[1], ])
    
  if (Events$Depth.1h[i] < Site_extracted_rainfall[1]) {
    RP_AMAX_1h[i] <- 1.01
    next()
  }
  
  if (Events$Depth.1h[i] > Site_extracted_rainfall[24]) {
    RP_AMAX_1h[i] <- 1000000
    next()
  }
  
  M1h <- max(which(Site_extracted_rainfall < Events$Depth.1h[i])) - 1
  X1h <- M1h + 3
  
  if (M1h == 0) {
    M1h <- 1
    X1h <- 4
  }
  
  RP_AMAX_1h[i] <- I4PLA(Site_extracted_rainfall[M1h:X1h],
						 RPList[M1h:X1h],
						 Events$Depth.1h[i])
  
}

RP_AMAX_6h <- rep(NA, NEv)
for (i in 1:NEv) {
  
  if (length(which(Metadata$NRFA_ID == Events$NRFA[i]) == 0) == 0) {
    next()
  }
  
  Site_extracted_rainfall <- unlist(Extracted_rainfall_from_csv_6h[
		which(Metadata$NRFA_ID == Events$NRFA[i])[1], ])
  
  if (Events$Depth.6h[i] < Site_extracted_rainfall[1]) {
    RP_AMAX_6h[i] <- 1.01
    next()
  }
  
  if (Events$Depth.6h[i] > Site_extracted_rainfall[24]) {
    RP_AMAX_6h[i] <- 1000000
    next()
  }
  
  M6h <- max(which(Site_extracted_rainfall < Events$Depth.6h[i])) - 1
  X6h <- M6h + 3
  
  if (M6h == 0) {
    M6h <- 1
    X6h <- 4
  }
  
  RP_AMAX_6h[i] <- I4PLA(Site_extracted_rainfall[M6h:X6h],
						 RPList[M6h:X6h],
						 Events$Depth.6h[i])
  
}

RP_AMAX_1d <- rep(NA, NEv)
for (i in 1:NEv) {
  
  if (length(which(Metadata$NRFA_ID == Events$NRFA[i]) == 0) == 0) {
    next()
  }
  
  Site_extracted_rainfall <- unlist(Extracted_rainfall_from_csv_1d[
		which(Metadata$NRFA_ID == Events$NRFA[i])[1], ])
  
  if (Events$Depth.1d[i] < Site_extracted_rainfall[1]) {
    RP_AMAX_1d[i] <- 1.01
    next()
  }
  
  if (Events$Depth.1d[i] > Site_extracted_rainfall[24]) {
    RP_AMAX_1d[i] <- 1000000
    next()
  }
  
  M1d <- max(which(Site_extracted_rainfall < Events$Depth.1d[i])) - 1
  X1d <- M1d + 3
  
  if (M1d == 0) {
    M1d <- 1
    X1d <- 4
  }
  
  RP_AMAX_1d[i] <- I4PLA(Site_extracted_rainfall[M1d:X1d],
							RPList[M1d:X1d],
							Events$Depth.1d[i])
  
}

RP_AMAX_4d <- rep(NA, NEv)
for (i in 1:NEv) {
  
  if (length(which(Metadata$NRFA_ID == Events$NRFA[i]) == 0) == 0) {
    next()
  }
  
  Site_extracted_rainfall <- unlist(Extracted_rainfall_from_csv_4d[
		which(Metadata$NRFA_ID == Events$NRFA[i])[1], ])
  
  if (Events$Depth.4d[i] < Site_extracted_rainfall[1]) {
    RP_AMAX_4d[i] <- 1.01
    next()
  }
  
  if (Events$Depth.4d[i] > Site_extracted_rainfall[24]) {
    RP_AMAX_4d[i] <- 1000000
    next()
  }
  
  M4d <- max(which(Site_extracted_rainfall < Events$Depth.4d[i])) - 1
  X4d <- M4d + 3
  
  if (M4d == 0) {
    M4d <- 1
    X4d <- 4
  }
  
  RP_AMAX_4d[i] <- I4PLA(Site_extracted_rainfall[M4d:X4d],
						RPList[M4d:X4d],
						Events$Depth.4d[i])
  
}

RPs_AMAX <- data.frame(RP_AMAX_1h, RP_AMAX_6h, RP_AMAX_1d, RP_AMAX_4d)
RPs_POT <- 1 / -log( 1 - 1/RPs_AMAX)
colnames(RPs_POT) <- gsub("AMAX", "POT", colnames(RPs_AMAX))
OUT <- as.data.frame(c(Events, round(RPs_AMAX, 1), round(RPs_POT, 1)))

write.csv(OUT, file = rainfall_FEH_out, row.names = FALSE)
