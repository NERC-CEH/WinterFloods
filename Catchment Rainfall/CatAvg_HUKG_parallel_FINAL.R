### giaves 2023-09-01
# EA project 35752: Hydrological analysis of the 2019-2021 flooding

# Script for use on JASMIN (jasmin.ac.uk) to get catchment averages from HadUK daily rainfall 1km dataset
# HadUK data available from Met Office.
# Reads in gridded netcdf data, extracts timeseries for all gridcells within a given catchment polygon,
# takes a weighted mean over the catchment (weighted by fraction of gridcell within catchment),
# returns a single timeseries per catchment, one file per station.

# Version 0.1: 2023-09-01. Initial development of code
# Version 0.2: 2023-11-01. Refactoring for wider distribution.
# Version 1.0: 2024-07-22. Final version for wider distribution.

##### SETUP #####
library(ncdf4)
library(raster)
library(sf)
library(gtools)

# Name of catchment passed as argument from slurm script
# If ran as a looped script within a slurm pipeline, this identified the station.
# otherwise, edit ca to be the row of NRFA (supplied by NRFA_catchments_filename)
# to be analysed.
ca <- as.numeric(commandArgs(trailingOnly = TRUE))[1]



##### KEY ARGUMENTS #####
data_dir <- "" # fill with the folder with nrfa catchments
event_info_filename <- "" #fill with the events metadata filepath
haduk_foldername <- "" #fill with folder containing daily rainfall grids from HadUK
NRFA_catchments_filename <- paste0(data_dir,"") #fill with .shp file with catchment polygons
output_folder_filename <- "" # fill with folder to save daily rainfall timeseries data into.




##### DATA #####
# read in data
NRFAshp <- sf::st_read(NRFA_catchments_filename)
WinterFloods <- readr::read_csv(event_info_filename) # Needs 'Station' character field


NRFA <- NRFAshp[which(NRFAshp@data$STATION %in% WinterFloods$Station), ]
NRFA <- NRFA[order(NRFA@data$SHAPE_AREA), ]
NRFA <- NRFA[ca, ]


# read in HADUK grid - need permission
# Save the files for 1km daily rainfall HadUK grid in a single folder with files
# starting with "rainfall_hadukgrid_uk_1km_day_"
HADUKGRID <- gtools::mixedsort(
  list.files(path = haduk_foldername,
             pattern = "rainfall_hadukgrid_uk_1km_day_",
             full.names = TRUE))
HADUKGRID <- HADUKGRID[1525:1572] # selects the correct period of record

SNUM <- NRFA[1, ]@data$STATION
# strip leading zeroes
SNAME <- gsub(" ", "0", formatC(SNUM, format = "d", width = 6))
# file to save rain average data to
FNAME <- paste0(output_folder_filename, SNAME, "-rain-data.csv")

print(paste(SNUM, SNAME, FNAME))




##### TIME SERIES EXTRACTION #####
# setup .csv files to write into
if (file.exists(FNAME)) {
  A <- readr::read_csv(FNAME)
  st <- length(grep("-01 00:00:00", A$V1)) + 1
} else {
  st <- 1
}
rm(A)

# there are 48 specific files which we can use to extract daily rainfall
# to prevent it redoing the loop over an over on crashing, this skips straight to the first 
# incomplete file
if (st > 48) stop()
for (f in st:48) {
  # read in the netCDF file and extract the rainfall variable
  Rain <- ncdf4::nc_open(HADUKGRID[f])
  Precp <- ncdf4::ncvar_get(Rain, varid = "rainfall")
  
  # converts hours to fractions of days, and fixes to UTC.
  Time <- ncdf4::ncvar_get(Rain, varid = "time")
  Time <- as.Date(Time/24, origin = "1800-01-01", tz= "UTC") 
  
  # netcdf files closed to save system memory and prevent accidental edits.
  ncdf4::nc_close(Rain)
  
  # set up the daily catchment average vector
  CatAvgRain <- rep(NA, dim(Precp)[3])
  
  
  for (g in 1:dim(Precp)[3]) {  # for each day
    Timestep <- as.character(Time[g], format = "%Y-%m-%d %H:%M:%S") #extract the timestep from the rainfall
    
    PrecpFlip <- apply(t(Precp[ , ,g]), 2, rev) #flip axes to correct orientation
    
    # convert grid to raster, then use that to get a weighted mean
    OneRain <- raster::raster(
      PrecpFlip, xmn = -200000, ymn = -200000, xmx = 700000, ymx = 1250000) 
    extractedRasterRain <- raster::extract(
      OneRain, NRFA[1, ], weight = TRUE, normalizeWeights = FALSE)[[1]]
    
    CatAvgRain[g] <- weighted.mean(
      extractedRasterRain[ , 1], extractedRasterRain[ , 2]) #save to output table
  }
    
  OUT <- data.frame(Time, round(CatAvgRain, 4)) # round catchment rainfall to 4 decimal places.
  # appends to table rather than writing a whole new file
  write.table(OUT, file = FNAME, row.names = FALSE, col.names = FALSE,
              quote = FALSE, sep = ",", append = TRUE)
}


# end R session, only needed for HPC computing using slurm/LSF etc.
quit(save = "no", runLast = FALSE)