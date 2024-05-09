### giaves 2023-10-25
# 08458: Winter Floods 2019-21

# Find event return periods for depths extracted by H24_event_totals.R

# Version 0.1: 2023-10-25. Initial development of code
# Version 0.2: 2023-11-01. Refactoring for wider distribution.


#### SETUP ####
library("xml2")

#### KEY FILEPATHS ####
rainfall_events_in <- "Data/Catchment Rainfall Event Data/CatAvg_rainfall_totals.csv"
feh_in <- "P:/Flood Estimation Handbook/FEH Data/WINFAP-FEH_v12.0.2"
rainfall_FEH_out <- "Data/Catchment Rainfall Event Data/CatAvg_rainfall_RPs_from_XMLs.csv"


#### READ IN DATA ####
Events <- read.csv(rainfall_events_in)

NEv <- dim(Events)[1]

FEHfiles <- list.files(path = feh_in, pattern = "xml", recursive = TRUE, full = TRUE)

# Part 1 not needed - script adapted from point rainfall RP calculation script

##########################################
#  Part 2: extract depths for specified  #
#    return periods from FEH22 rasters   #
##########################################

# Convert fixed to sliding rainfall
Events$Depth.1h <- Events$Depth.1h * 1.04 # estimate that sliding 1-hour rainfall depths should be 4% higher due to 15-minute data resolution



if (file.exists(partial_completion_backup)) {
  
  load(partial_completion_backup)
  
} else {
  
  Extracted_rainfall_from_csv <- array(0, dim = c(4, 22, NEv))
  
  for (i in 1:NEv) {
    
    St <- formatC(Events$NRFA[i], digits = 5, flag = 0)
    
    File <- grep(St, FEHfiles)
    
    if (length(File) == 0) next()
    
    FEHfile <- as_list(read_xml(FEHfiles[File]))
    
    Depths <- unlist(FEHfile$FEHDescriptors$CatchmentAverageDDF2022Values)
    
    Extracted_rainfall_from_csv[1, 1:22, i]  <- as.numeric(strsplit(Depths[6], ",")$Depths)
    Extracted_rainfall_from_csv[2, 1:22, i]  <- as.numeric(strsplit(Depths[9], ",")$Depths)
    Extracted_rainfall_from_csv[3, 1:22, i]  <- as.numeric(strsplit(Depths[12], ",")$Depths)
    Extracted_rainfall_from_csv[4, 1:22, i]  <- as.numeric(strsplit(Depths[14], ",")$Depths)
    
  }

  save(Extracted_rainfall_from_csv, file = partial_completion_backup)
  
}

#################################
#  Part 3: find return periods  #
#  using 4-point interpolation  #
#################################

source("Code/Pluvial/I4PLA.R")
RPList <- c(1.3, 1.58, 2, 3, 6, 10, 18, 31, 56, 100, 180, 310, 560, 1000,
			1800, 3100, 5600, 10000, 18000, 31000, 56000, 100000)

RP_AMAX_1h <- rep(NA, NEv)
for (i in 1:NEv) {
  
  if (sum(Extracted_rainfall_from_csv[1, , i]) == 0) {
    RP_AMAX_1h[i] <- NA
    next()
  }
  
  if (Events$Depth.1h[i] < Extracted_rainfall_from_csv[1, 1, i]) {
    RP_AMAX_1h[i] <- 1.01
    next()
  }
  
  if (Events$Depth.1h[i] > Extracted_rainfall_from_csv[1, 22, i]) {
    RP_AMAX_1h[i] <- 1000000
    next()
  }
  
  M1h <- max(which(Extracted_rainfall_from_csv[1, , i] < Events$Depth.1h[i])) - 1
  X1h <- M1h + 3
  
  if (M1h == 0) {
    M1h <- 1
    X1h <- 4
  }
  
  RP_AMAX_1h[i] <- I4PLA(Extracted_rainfall_from_csv[1, M1h:X1h , i],
						RPList[M1h:X1h], Events$Depth.1h[i])
  
}

RP_AMAX_6h <- rep(NA, NEv)
for (i in 1:NEv) {
  
  if (sum(Extracted_rainfall_from_csv[2, , i]) == 0) {
    RP_AMAX_6h[i] <- NA
    next()
  }
  
  if (Events$Depth.6h[i] < Extracted_rainfall_from_csv[2, 1, i]) {
    RP_AMAX_6h[i] <- 1.01
    next()
  }
  
  if (Events$Depth.6h[i] > Extracted_rainfall_from_csv[2, 22, i]) {
    RP_AMAX_6h[i] <- 1000000
    next()
  }
  
  M6h <- max(which(Extracted_rainfall_from_csv[2, , i] < Events$Depth.6h[i])) - 1
  X6h <- M6h + 3
  
  if (M6h == 0) {
    M6h <- 1
    X6h <- 4
  }
  
  RP_AMAX_6h[i] <- I4PLA(Extracted_rainfall_from_csv[2, M6h:X6h , i],
						RPList[M6h:X6h], Events$Depth.6h[i])
  
}

RP_AMAX_1d <- rep(NA, NEv)
for (i in 1:NEv) {
  
  if (sum(Extracted_rainfall_from_csv[3, , i]) == 0) {
    RP_AMAX_1d[i] <- NA
    next()
  }
  
  if (Events$Depth.1d[i] < Extracted_rainfall_from_csv[3, 1, i]) {
    RP_AMAX_1d[i] <- 1.01
    next()
  }
  
  if (Events$Depth.1d[i] > Extracted_rainfall_from_csv[3, 22, i]) {
    RP_AMAX_1d[i] <- 1000000
    next()
  }
  
  M1d <- max(which(Extracted_rainfall_from_csv[3, , i] < Events$Depth.1d[i])) - 1
  X1d <- M1d + 3
  
  if (M1d == 0) {
    M1d <- 1
    X1d <- 4
  }
  
  RP_AMAX_1d[i] <- I4PLA(Extracted_rainfall_from_csv[3, M1d:X1d , i],
						RPList[M1d:X1d], Events$Depth.1d[i])
  
}

RP_AMAX_4d <- rep(NA, NEv)
for (i in 1:NEv) {
  
  if (sum(Extracted_rainfall_from_csv[4, , i]) == 0) {
    RP_AMAX_4d[i] <- NA
    next()
  }
  
  if (Events$Depth.4d[i] < Extracted_rainfall_from_csv[4, 1, i]) {
    RP_AMAX_4d[i] <- 1.01
    next()
  }
  
  if (Events$Depth.4d[i] > Extracted_rainfall_from_csv[4, 22, i]) {
    RP_AMAX_4d[i] <- 1000000
    next()
  }
  
  M4d <- max(which(Extracted_rainfall_from_csv[4, , i] < Events$Depth.4d[i])) - 1
  X4d <- M4d + 3
  
  if (M4d == 0) {
    M4d <- 1
    X4d <- 4
  }
  
  RP_AMAX_4d[i] <- I4PLA(Extracted_rainfall_from_csv[4, M4d:X4d , i],
						RPList[M4d:X4d], Events$Depth.4d[i])
  
}

RPs_AMAX <- data.frame(RP_AMAX_1h, RP_AMAX_6h, RP_AMAX_1d, RP_AMAX_4d)
RPs_POT <- 1 / -log( 1 - 1/RPs_AMAX)
colnames(RPs_POT) <- gsub("AMAX", "POT", colnames(RPs_AMAX))
OUT <- as.data.frame(c(Events, round(RPs_AMAX, 1), round(RPs_POT, 1)))

write.csv(OUT, file = rainfall_FEH_out, row.names = FALSE)
