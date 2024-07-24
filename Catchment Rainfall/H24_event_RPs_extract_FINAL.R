### giaves 2023-10-25
# EA project 35752: Hydrological analysis of the 2019-2021 flooding

# Find event return periods for depths extracted by H24_event_totals_FINAL.R
# Interpolates between FEH22 grids of return levels to get 1 in x AEP for four
# accumulation periods 1h, 6h, 1d, 4d.
# Saves all the results to a single .csv file for further analysis.

# Version 0.1: 2023-10-25. Initial development of code
# Version 0.2: 2023-11-01. Refactoring for wider distribution.
# Version 1.0: 2024-07-22. Final version for wider distribution


#### SETUP ####
library(xml2)



#### KEY FILEPATHS ####
rainfall_events_in <- "./Data/CatAvg_rainfall_totals.csv"
rainfall_FEH_out <- "./Data/CatAvg_rainfall_RPs_from_FEH22_grid_extraction.csv"
feh22_events_folder <- "./Data/Catchment Rainfall Event Data/" # contains 'Station_FEH22_depths_**.csv' files
metadata_filename <- "Data/H24 Results/Station_FEH22_depths_1h.csv" # This file can be any of the 'FEH22_depths' files, the metadata is in the first 5 columns.

#### READ IN DATA ####
Events <- read.csv(rainfall_events_in)

NEv <- dim(Events)[1] # number of events to analyse..



#### Extract depths for specified return periods from FEH22 rasters



# Convert fixed to sliding rainfall
Events$Depth.1h <- Events$Depth.1h * 1.04 # estimate that sliding 1-hour rainfall depths should be 4% higher due to 15-minute data resolution

# Reads in rainfall estimates, excludes metadata columns.
Extracted_rainfall_from_csv_1h <- read.csv(
	paste0(feh22_events_folder, "Station_FEH22_depths_1h.csv"))[ , 6:29]
Extracted_rainfall_from_csv_6h <- read.csv(
  paste0(feh22_events_folder, "Station_FEH22_depths_6h.csv"))[ , 6:29]
Extracted_rainfall_from_csv_1d <- read.csv(
  paste0(feh22_events_folder, "Station_FEH22_depths_1d.csv"))[ , 6:29]
Extracted_rainfall_from_csv_4d <- read.csv(
  paste0(feh22_events_folder, "Station_FEH22_depths_4d.csv"))[ , 6:29]

# Reads in rainfall estimates, but only retains metadata.
Metadata <- read.csv(metadata_filename)[ , 1:5]



##### ANALYSIS #####
## find return periods using 4-point interpolation 

source("./I4PLA.R") # function for 4-point interpolation

# These are the return periods calculated in the FEH22 grids, do not change.
RPList <- c(1.3, 1.58, 2, 3, 6, 10, 18, 31, 56, 100, 180, 310, 560, 1000,
			1800, 3100, 5600, 10000, 18000, 31000, 56000, 100000)



### 1-HOUR ANALYSIS ###
# Each of the four accumulation periods works in the same way

RP_AMAX_1h <- rep(NA, NEv) #preallocate
for (i in 1:NEv) {
  
  if (length(which(Metadata$NRFA_ID == Events$NRFA[i]) == 0) == 0) {
    next() # If station not in Events list, skip
  }
  
  Site_extracted_rainfall <- unlist(
    Extracted_rainfall_from_csv_1h[which(Metadata$NRFA_ID == Events$NRFA[i])[1], ]
    )
    
  if (Events$Depth.1h[i] < Site_extracted_rainfall[1]) {
    RP_AMAX_1h[i] <- 1.01
    next() #If rainfall is less than 1.3yr return level, set to 1.01 years
  }
  
  if (Events$Depth.1h[i] > Site_extracted_rainfall[24]) {
    RP_AMAX_1h[i] <- 1000000
    next() #If rainfall is greater than 10000yr return level, cap at 100000 years.
  }
  
  # which levels are closest to the observed rainfall
  M1h <- max(which(Site_extracted_rainfall < Events$Depth.1h[i])) - 1 
  X1h <- M1h + 3
  
  if (M1h == 0) { # accounts for lowest return levels
    M1h <- 1
    X1h <- 4
  }
  
  # Given 4 return levels, calculate an interpolated rainfall return period from
  # nearby locations and return levels.
  RP_AMAX_1h[i] <- I4PLA(Site_extracted_rainfall[M1h:X1h],
						 RPList[M1h:X1h],
						 Events$Depth.1h[i])
  
}




### 6-HOUR ANALYSIS ###
RP_AMAX_6h <- rep(NA, NEv)
for (i in 1:NEv) {
  
  if (length(which(Metadata$NRFA_ID == Events$NRFA[i]) == 0) == 0) {
    next() # If station not in Events list, skip
  }
  
  Site_extracted_rainfall <- unlist(Extracted_rainfall_from_csv_6h[
		which(Metadata$NRFA_ID == Events$NRFA[i])[1], ])
  
  if (Events$Depth.6h[i] < Site_extracted_rainfall[1]) {
    RP_AMAX_6h[i] <- 1.01
    next() #If rainfall is less than 1.3yr return level, set to 1.01 years
  }
  
  if (Events$Depth.6h[i] > Site_extracted_rainfall[24]) {
    RP_AMAX_6h[i] <- 1000000
    next() #If rainfall is greater than 10000yr return level, cap at 100000 years.
  }
  
  # which levels are closest to the observed rainfall
  M6h <- max(which(Site_extracted_rainfall < Events$Depth.6h[i])) - 1
  X6h <- M6h + 3
  
  if (M6h == 0) { # accounts for lowest return levels
    M6h <- 1
    X6h <- 4
  }
  
  # Given 4 return levels, calculate an interpolated rainfall return period from
  # nearby locations and return levels.
  RP_AMAX_6h[i] <- I4PLA(Site_extracted_rainfall[M6h:X6h],
						 RPList[M6h:X6h],
						 Events$Depth.6h[i])
  
}


### 1-DAY ANALYSIS ###
RP_AMAX_1d <- rep(NA, NEv)
for (i in 1:NEv) {
  
  if (length(which(Metadata$NRFA_ID == Events$NRFA[i]) == 0) == 0) {
    next() # If station not in Events list, skip
  }
  
  Site_extracted_rainfall <- unlist(Extracted_rainfall_from_csv_1d[
		which(Metadata$NRFA_ID == Events$NRFA[i])[1], ])
  
  if (Events$Depth.1d[i] < Site_extracted_rainfall[1]) {
    RP_AMAX_1d[i] <- 1.01
    next() #If rainfall is less than 1.3yr return level, set to 1.01 years
  }
  
  if (Events$Depth.1d[i] > Site_extracted_rainfall[24]) {
    RP_AMAX_1d[i] <- 1000000
    next() #If rainfall is greater than 10000yr return level, cap at 100000 years.
  }
  
  # which levels are closest to the observed rainfall
  M1d <- max(which(Site_extracted_rainfall < Events$Depth.1d[i])) - 1
  X1d <- M1d + 3
  
  if (M1d == 0) { # accounts for lowest return levels
    M1d <- 1
    X1d <- 4
  }
  
  # Given 4 return levels, calculate an interpolated rainfall return period from
  # nearby locations and return levels.
  RP_AMAX_1d[i] <- I4PLA(Site_extracted_rainfall[M1d:X1d],
							RPList[M1d:X1d],
							Events$Depth.1d[i])
  
}




### 4-DAY ANALYSIS ###
RP_AMAX_4d <- rep(NA, NEv)
for (i in 1:NEv) {
  
  if (length(which(Metadata$NRFA_ID == Events$NRFA[i]) == 0) == 0) {
    next() # If station not in Events list, skip
  }
  
  Site_extracted_rainfall <- unlist(Extracted_rainfall_from_csv_4d[
		which(Metadata$NRFA_ID == Events$NRFA[i])[1], ])
  
  if (Events$Depth.4d[i] < Site_extracted_rainfall[1]) {
    RP_AMAX_4d[i] <- 1.01
    next() #If rainfall is less than 1.3yr return level, set to 1.01 years
  }
  
  if (Events$Depth.4d[i] > Site_extracted_rainfall[24]) {
    RP_AMAX_4d[i] <- 1000000
    next()  #If rainfall is greater than 10000yr return level, cap at 100000 years.
  }
  
  # which levels are closest to the observed rainfall
  M4d <- max(which(Site_extracted_rainfall < Events$Depth.4d[i])) - 1
  X4d <- M4d + 3
  
  if (M4d == 0) {  # accounts for lowest return levels
    M4d <- 1
    X4d <- 4
  }
  
  # Given 4 return levels, calculate an interpolated rainfall return period from
  # nearby locations and return levels.
  RP_AMAX_4d[i] <- I4PLA(Site_extracted_rainfall[M4d:X4d],
						RPList[M4d:X4d],
						Events$Depth.4d[i])
  
}



##### FINAL OUTPUTS #####
RPs_AMAX <- data.frame(RP_AMAX_1h, RP_AMAX_6h, RP_AMAX_1d, RP_AMAX_4d)
RPs_POT <- 1 / -log( 1 - 1/RPs_AMAX) # Langbein's correction from AMAX to POT 1 in x AEPs
colnames(RPs_POT) <- gsub("AMAX", "POT", colnames(RPs_AMAX))
OUT <- as.data.frame(c(Events, round(RPs_AMAX, 1), round(RPs_POT, 1))) # 1 in x AEP rounded to 1d.p.


##### SAVE OUTPUTS #####
write.csv(OUT, file = rainfall_FEH_out, row.names = FALSE)
