### giaves 2023-10-25
# EA project 35752: Hydrological analysis of the 2019-2021 flooding

# Find event return periods for depths extracted by H24_event_totals.R
# This code extracts Catchment Average rainfall return levels from the Peak Flow or
# FEH web service .xml files.

# Version 0.1: 2023-10-25. Initial development of code
# Version 0.2: 2023-11-01. Refactoring for wider distribution.
# Version 1.0: 2024-07-22. Final version for wider distribution.


##### SETUP #####
library(xml2)

##### KEY FILEPATHS #####
rainfall_events_in <- "CatAvg_rainfall_totals.csv" #filepath to .csv file containing catchment average rainfall event totals.
feh_in <- "" #folder containing .xml files containing FEH22 parameters and return levels
rainfall_FEH_out <- "CatAvg_rainfall_RPs_from_XMLs.csv" #file to save return periods to
partial_completion_backup <- "" # file to save working progress in case of crash.



##### READ IN DATA #####
Events <- read.csv(rainfall_events_in)

NEv <- dim(Events)[1]

FEHfiles <- list.files(path = feh_in, pattern = "xml", recursive = TRUE, full = TRUE)


##### ANALYSIS #####
# Extract depths for specified return periods from FEH22 rasters


# Convert fixed to sliding rainfall
Events$Depth.1h <- Events$Depth.1h * 1.04 
# estimate that sliding 1-hour rainfall depths should be 4% higher due to 15-minute data resolution


### convert rainfall depths into a tidy dataframe
if (file.exists(partial_completion_backup)) {
  # If process crashes during computation, this will carry on from where it got to.
  load(partial_completion_backup)
  
} else {
  
  Extracted_rainfall_from_csv <- array(0, dim = c(4, 22, NEv)) #preallocate
  
  for (i in 1:NEv) {
    
    St <- formatC(Events$NRFA[i], digits = 5, flag = 0) # extracts NRFA station number
    
    File <- grep(St, FEHfiles) # finds corresponding .xml file
    
    if (length(File) == 0) next() #skip station if no .xml file found
    
    FEHfile <- as_list(read_xml(FEHfiles[File]))
    
    # Reads 
    Depths <- unlist(FEHfile$FEHDescriptors$CatchmentAverageDDF2022Values)
    
    # Gets 1h, 4h, 1d, 4d rainfall depths from xml files
    Extracted_rainfall_from_csv[1, 1:22, i]  <- as.numeric(strsplit(Depths[6], ",")$Depths)
    Extracted_rainfall_from_csv[2, 1:22, i]  <- as.numeric(strsplit(Depths[9], ",")$Depths)
    Extracted_rainfall_from_csv[3, 1:22, i]  <- as.numeric(strsplit(Depths[12], ",")$Depths)
    Extracted_rainfall_from_csv[4, 1:22, i]  <- as.numeric(strsplit(Depths[14], ",")$Depths)
    
  }

  save(Extracted_rainfall_from_csv, file = partial_completion_backup)
  
}



#  Find return periods using 4-point interpolation

source("Code/Pluvial/I4PLA.R") # function for 4-point interpolation

# These are the return periods calculated in the FEH22 grids, do not change.
RPList <- c(1.3, 1.58, 2, 3, 6, 10, 18, 31, 56, 100, 180, 310, 560, 1000,
			1800, 3100, 5600, 10000, 18000, 31000, 56000, 100000)

### 1-HOUR ANALYSIS ###
# Each of the four accumulation periods works in the same way
RP_AMAX_1h <- rep(NA, NEv) #preallocate
for (i in 1:NEv) {
  
  if (sum(Extracted_rainfall_from_csv[1, , i]) == 0) {
    RP_AMAX_1h[i] <- NA #If no rainfall in event, set 1 in x AEP to NA
    next()
  }
  
  if (Events$Depth.1h[i] < Extracted_rainfall_from_csv[1, 1, i]) {
    RP_AMAX_1h[i] <- 1.01 
    next() #If rainfall is less than 1.3yr return level, set to 1.01 years
  }
  
  if (Events$Depth.1h[i] > Extracted_rainfall_from_csv[1, 22, i]) {
    RP_AMAX_1h[i] <- 1000000
    next() #If rainfall is greater than 10000yr return level, cap at 100000 years.
  }
  
  # which levels are closest to the observed rainfall
  M1h <- max(which(Extracted_rainfall_from_csv[1, , i] < Events$Depth.1h[i])) - 1
  X1h <- M1h + 3
  
  if (M1h == 0) { # accounts for lowest return levels
    M1h <- 1
    X1h <- 4
  }
  
  # Given 4 return levels, calculate an interpolated rainfall return period from
  # nearby locations and return levels.
  RP_AMAX_1h[i] <- I4PLA(Extracted_rainfall_from_csv[1, M1h:X1h , i],
						RPList[M1h:X1h], Events$Depth.1h[i])
  
}

### 6-HOUR ANALYSIS ###
RP_AMAX_6h <- rep(NA, NEv)
for (i in 1:NEv) {
  
  if (sum(Extracted_rainfall_from_csv[2, , i]) == 0) {
    RP_AMAX_6h[i] <- NA #If no rainfall in event, set 1 in x AEP to NA
    next()
  }
  
  if (Events$Depth.6h[i] < Extracted_rainfall_from_csv[2, 1, i]) {
    RP_AMAX_6h[i] <- 1.01
    next() #If rainfall is less than 1.3yr return level, set to 1.01 years
  }
  
  if (Events$Depth.6h[i] > Extracted_rainfall_from_csv[2, 22, i]) {
    RP_AMAX_6h[i] <- 1000000
    next() #If rainfall is greater than 10000yr return level, cap at 100000 years.
  }
  
  # which levels are closest to the observed rainfall
  M6h <- max(which(Extracted_rainfall_from_csv[2, , i] < Events$Depth.6h[i])) - 1
  X6h <- M6h + 3
  
  if (M6h == 0) { # accounts for lowest return levels
    M6h <- 1
    X6h <- 4
  }
  
  # Given 4 return levels, calculate an interpolated rainfall return period from
  # nearby locations and return levels.
  RP_AMAX_6h[i] <- I4PLA(Extracted_rainfall_from_csv[2, M6h:X6h , i],
						RPList[M6h:X6h], Events$Depth.6h[i])
  
}

### 1-DAY ANALYSIS ###
RP_AMAX_1d <- rep(NA, NEv)
for (i in 1:NEv) {
  
  if (sum(Extracted_rainfall_from_csv[3, , i]) == 0) {
    RP_AMAX_1d[i] <- NA #If no rainfall in event, set 1 in x AEP to NA
    next()
  }
  
  if (Events$Depth.1d[i] < Extracted_rainfall_from_csv[3, 1, i]) {
    RP_AMAX_1d[i] <- 1.01
    next() #If rainfall is less than 1.3yr return level, set to 1.01 years
  }
  
  if (Events$Depth.1d[i] > Extracted_rainfall_from_csv[3, 22, i]) {
    RP_AMAX_1d[i] <- 1000000
    next() #If rainfall is greater than 10000yr return level, cap at 100000 years.
  }
  
  # which levels are closest to the observed rainfall
  M1d <- max(which(Extracted_rainfall_from_csv[3, , i] < Events$Depth.1d[i])) - 1
  X1d <- M1d + 3
  
  if (M1d == 0) { # accounts for lowest return levels
    M1d <- 1
    X1d <- 4
  }
  
  # Given 4 return levels, calculate an interpolated rainfall return period from
  # nearby locations and return levels.
  RP_AMAX_1d[i] <- I4PLA(Extracted_rainfall_from_csv[3, M1d:X1d , i],
						RPList[M1d:X1d], Events$Depth.1d[i])
  
}

### 4-DAY ANALYSIS ###
RP_AMAX_4d <- rep(NA, NEv)
for (i in 1:NEv) {
  
  if (sum(Extracted_rainfall_from_csv[4, , i]) == 0) {
    RP_AMAX_4d[i] <- NA  #If no rainfall in event, set 1 in x AEP to NA
    next()
  }
  
  if (Events$Depth.4d[i] < Extracted_rainfall_from_csv[4, 1, i]) {
    RP_AMAX_4d[i] <- 1.01
    next() #If rainfall is less than 1.3yr return level, set to 1.01 years
  }
  
  if (Events$Depth.4d[i] > Extracted_rainfall_from_csv[4, 22, i]) {
    RP_AMAX_4d[i] <- 1000000
    next() #If rainfall is greater than 10000yr return level, cap at 100000 years.
  }
  
  # which levels are closest to the observed rainfall
  M4d <- max(which(Extracted_rainfall_from_csv[4, , i] < Events$Depth.4d[i])) - 1
  X4d <- M4d + 3
  
  if (M4d == 0) { # accounts for lowest return levels
    M4d <- 1
    X4d <- 4
  }
  
  # Given 4 return levels, calculate an interpolated rainfall return period from
  # nearby locations and return levels.
  RP_AMAX_4d[i] <- I4PLA(Extracted_rainfall_from_csv[4, M4d:X4d , i],
						RPList[M4d:X4d], Events$Depth.4d[i])
  
}

##### FINAL OUTPUTS #####
RPs_AMAX <- data.frame(RP_AMAX_1h, RP_AMAX_6h, RP_AMAX_1d, RP_AMAX_4d)
RPs_POT <- 1 / -log( 1 - 1/RPs_AMAX) # Langbein's correction from AMAX to POT 1 in x AEPs
colnames(RPs_POT) <- gsub("AMAX", "POT", colnames(RPs_AMAX))
OUT <- as.data.frame(c(Events, round(RPs_AMAX, 1), round(RPs_POT, 1))) # 1 in x AEP rounded to 1d.p.

##### SAVE OUTPUTS #####
write.csv(OUT, file = rainfall_FEH_out, row.names = FALSE)
