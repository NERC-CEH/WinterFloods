#### giaves. 2023-10-25
# EA project 35752: Hydrological analysis of the 2019-2021 flooding

# Main contributor: Gianni Vesuviano
# Info: Find event return periods for depths extracted by Pluvial_event_totals_FINAL.R

# Version 0.1: 2023-10-25. Initial development of code
# Version 0.2: 2023-11-01. Refactoring for wider distribution.
# Version 1.0: 2024-07-22. Final version for wider distribution.

### NOTE: source rainfall data not given as data product in this project.


#### SETUP ####
library(raster)
library(gtools)

source("Code/Pluvial/I4PLA.R")

#### KEY FILEPATHS ####
rainfall_events_in <- "Data/Pluvial/Point_rainfall_totals.csv" # Per event raingauge depth totals
gridded_feh_in <- "" #### INTERNAL FILE FOR FEH22 GRIDS. Contact UKCEH for licensing this data product.
rainfall_raw_in <- "Data/Pluvial" # folder containing raw pluvial, at-site data

rainfall_FEH_out <- "Data/Pluvial/Point_rainfall_RPs.csv" # filename for outputs of raingauge at site 1 in x AEP


partial_completion_backup <- "Code/Pluvial/Extracted_rainfall_from_grids.Rda" # filename for backup R file in case of process crash.


#### READ IN DATA ####
Events <- read.csv(rainfall_events_in)

# Raw rainfall files come in two formats .all, and *exp.log, which are roughly the same.
RG_files <- list.files(path = rainfall_raw_in, pattern = "\\.all",
                       full = TRUE, recursive = TRUE)
RG_explog <- list.files(path = rainfall_raw_in, pattern = "exp.log",
                        full = TRUE, recursive = TRUE)

RG_series <- setdiff(RG_files, RG_explog)

NEv <- dim(Events)[1]



##### ANALYSIS #####

#  Part 1: get coordinates for all gauges 

for (i in 1:NEv) { # for each event
  
  #Find the right rainfall file
  RG_File <- RG_series[grep(Events$ID[i], RG_series)]
  
  # Skip if it doesn't exist
  if (length(RG_File) == 0) {
    print(sprintf("%s empty", i))
    next()
  }
  
  # Cases where two files are available (one 15-minute, one 60-minute) - only select 15-minute
  if (length(RG_File) > 1) {
    RG_File <- RG_File[grep("15", RG_File)]
  }
  
  # Load coordinates 
  Coords <- read.csv(RG_File, skip = 11, nrows = 2, header = FALSE, sep = "\t")[ , 2]
  
  Events$X[i] <- Coords[1]
  Events$Y[i] <- Coords[2]
  
}


#  Part 2: extract depths for specified return periods from FEH22 rasters


# Convert fixed to sliding rainfall
I15 <- which(Events$Interval == 15)
I60 <- which(Events$Interval == 60)
Events$Depth.1h[I15] <- Events$Depth.1h[I15] * 1.04 # estimate that sliding 1-hour rainfall depths should be 4% higher due to 15-minute data resolution
Events$Depth.1h[I60] <- Events$Depth.1h[I60] * 1.16 # estimate that sliding 1-hour rainfall depths should be 16% higher due to 1-hour data resolution
Events$Depth.6h[I60] <- Events$Depth.6h[I60] * 1.02 # estimate that sliding 6-hour rainfall depths should be 2% higher due to 1-hour data resolution


# read in gridded FEH22 data
Rast_1h <- gtools::mixedsort(
  list.files(gridded_feh_in, pattern = "GB_1_", full = TRUE))
Rast_6h <- gtools::mixedsort(
  list.files(gridded_feh_in, pattern = "GB_6_", full = TRUE))
Rast_1d <- gtools::mixedsort(
  list.files(gridded_feh_in, pattern = "GB_24_", full = TRUE))
Rast_4d <- gtools::mixedsort(
  list.files(gridded_feh_in, pattern = "GB_96_", full = TRUE))

Coords<- data.frame(Events[ , c("X", "Y")])


# If process of reading rasters crashes, this will load it from the last save point.
if (file.exists(partial_completion_backup)) {
  
  load(partial_completion_backup)
  
} else {
  
  Extracted_rainfall_from_grids <- array(0, dim = c(4, 24, NEv))
  
  RPList <- c(1.3, 1.58, 2, 3, 6, 10, 18, 31, 56, 100, 180, 310, 560, 1000,
              1800, 3100, 5600, 10000, 18000, 31000, 56000, 100000, 250000,
              500000)
  
  for (i in 1:24) { # for each return level grid 
    
    print(i)
    
    R1H <- raster(Rast_1h[i])
    R6H <- raster(Rast_6h[i])
    R1D <- raster(Rast_1d[i])
    R4D <- raster(Rast_4d[i])
    
    Extracted_rainfall_from_grids[1, i, 1:NEv] <- extract(
      R1H, Coords, method = "bilinear")
    Extracted_rainfall_from_grids[2, i, 1:NEv] <- extract(
      R6H, Coords, method = "bilinear")
    Extracted_rainfall_from_grids[3, i, 1:NEv] <- extract(
      R1D, Coords, method = "bilinear")
    Extracted_rainfall_from_grids[4, i, 1:NEv] <- extract(
      R4D, Coords, method = "bilinear")
    
  }
  
  # convert to mm
  Extracted_rainfall_from_grids <- Extracted_rainfall_from_grids / 10
  
  save(Extracted_rainfall_from_grids, file = partial_completion_backup)
  
}

#  Part 3: find return periods using 4-point interpolation

RPList <- c(1.3, 1.58, 2, 3, 6, 10, 18, 31, 56, 100, 180, 310, 560, 1000,
            1800, 3100, 5600, 10000, 18000, 31000, 56000, 100000, 250000,
            500000)


### 1-hour analysis
RP_AMAX_1h <- rep(1.01, NEv)
for (i in 1:NEv) { # for each event
  
  # if event depth is less than 1.3yr return level, skip 
  if (Events$Depth.1h[i] < Extracted_rainfall_from_grids[1, 1, i]) next()
  
  #find the start time and period of interest
  M1h <- max(which(
    Extracted_rainfall_from_grids[1, , i] < Events$Depth.1h[i])) - 1
  X1h <- M1h + 3
  
  if (M1h == 0) { # account for first timestep
    M1h <- 1
    X1h <- 4
  }
  
  #calculate 4-point interpolated return period
  RP_AMAX_1h[i] <- I4PLA(Extracted_rainfall_from_grids[1, M1h:X1h , i],
                         RPList[M1h:X1h], Events$Depth.1h[i])
  
}

### 6-hour analysis
RP_AMAX_6h <- rep(1.01, NEv)
for (i in 1:NEv) { # for each event
  
  # if event depth is less than 1.3yr return level, skip 
  if (Events$Depth.6h[i] < Extracted_rainfall_from_grids[2, 1, i]) next()
  
  #find the start time and period of interest
  M6h <- max(which(
    Extracted_rainfall_from_grids[2, , i] < Events$Depth.6h[i])) - 1
  X6h <- M6h + 3
  
  # account for first timestep
  if (M6h == 0) {
    M6h <- 1
    X6h <- 4
  }
  
  #calculate 4-point interpolated return period
  RP_AMAX_6h[i] <- I4PLA(Extracted_rainfall_from_grids[2, M6h:X6h , i],
                         RPList[M6h:X6h], Events$Depth.6h[i])
  
}


### 1-day analysis
RP_AMAX_1d <- rep(1.01, NEv)
for (i in 1:NEv) { # for each event
  
  # if event depth is less than 1.3yr return level, skip 
  if (Events$Depth.1d[i] < Extracted_rainfall_from_grids[3, 1, i]) next()
  
  #find the start time and period of interest
  M1d <- max(which(
    Extracted_rainfall_from_grids[3, , i] < Events$Depth.1d[i])) - 1
  X1d <- M1d + 3
  
  # account for first timestep
  if (M1d == 0) {
    M1d <- 1
    X1d <- 4
  }
  
  #calculate 4-point interpolated return period
  RP_AMAX_1d[i] <- I4PLA(Extracted_rainfall_from_grids[3, M1d:X1d , i],
                         RPList[M1d:X1d], Events$Depth.1d[i])
  
}

### 4-day analysis
RP_AMAX_4d <- rep(1.01, NEv)
for (i in 1:NEv) {  # for each event
  
  # if event depth is less than 1.3yr return level, skip 
  if (Events$Depth.4d[i] < Extracted_rainfall_from_grids[4, 1, i]) next()
  
  #find the start time and period of interest
  M4d <- max(which(
    Extracted_rainfall_from_grids[4, , i] < Events$Depth.4d[i])) - 1
  X4d <- M4d + 3
  
  # account for first timestep
  if (M4d == 0) {
    M4d <- 1
    X4d <- 4
  }
  
  #calculate 4-point interpolated return period
  RP_AMAX_4d[i] <- I4PLA(Extracted_rainfall_from_grids[4, M4d:X4d , i],
                         RPList[M4d:X4d], Events$Depth.4d[i])
  
}

##### SAVE OUTPUTS #####

RPs_AMAX <- data.frame(RP_AMAX_1h, RP_AMAX_6h, RP_AMAX_1d, RP_AMAX_4d)
RPs_POT <- 1 / -log( 1 - 1/RPs_AMAX) # langbein approximation
colnames(RPs_POT) <- gsub("AMAX", "POT", colnames(RPs_AMAX))
OUT <- as.data.frame(c(Events, RPs_AMAX, RPs_POT))

write.csv(OUT, file = rainfall_FEH_out, row.names = FALSE)
