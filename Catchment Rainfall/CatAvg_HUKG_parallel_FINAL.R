### giaves 2023-09-01
# 08458: Winter Floods 2019-21

# Script for use on JASMIN to get catchment averages from HadUK daily rainfall 1km dataset

# Version 0.1: 2023-09-01. Initial development of code
# Version 0.2: 2023-11-01. Refactoring for wider distribution.

##### SETUP #####
library(ncdf4)
library(raster)
library(sf)

# Name of catchment passed as argument from slurm script
ca <- as.numeric(commandArgs(trailingOnly = TRUE))[1]

data_dir <- "" # fill with the folder with nrfa catchments
event_info_filename <- paste(data_dir, "WinterFloodsNRFA.csv", sep = "/")


NRFAshp <- sf::st_read(paste0(data_dir,"./nrfa_all_catchments.shp"))
WinterFloods <- readr::read_csv(event_info_filename)


NRFA <- NRFAshp[which(NRFAshp@data$STATION %in% WinterFloods$Station), ]
NRFA <- NRFA[order(NRFA@data$SHAPE_AREA), ]
NRFA <- NRFA[ca, ]

# read in HADUK grid - need permission
HADUKGRID <- gtools::mixedsort(
  list.files(path = "/badc/ukmo-hadobs/data/insitu/MOHC/HadOBS/HadUK-Grid/v1.1.0.0/1km/rainfall/day/latest",
             pattern = "rainfall_hadukgrid_uk_1km_day_",
             full.names = TRUE))
HADUKGRID <- HADUKGRID[1525:1572]

SNUM <- NRFA[1, ]@data$STATION
# strip leading zeroes
SNAME <- gsub(" ", "0", formatC(SNUM, format = "d", width = 6))
# file to save rain average data to
FNAME <- paste0("catavgHUKG/", SNAME, "-rain-data.csv")

print(paste(SNUM, SNAME, FNAME))

# setup files to write into
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
  # read in the netCDF file
  Rain <- ncdf4::nc_open(HADUKGRID[f])
  
  Precp <- ncdf4::ncvar_get(Rain, varid = "rainfall")
  
  Time <- ncdf4::ncvar_get(Rain, varid = "time")
  Time <- as.Date(Time/24, origin = "1800-01-01", tz= "UTC")
  
  ncdf4::nc_close(Rain)
  
  # set up the daily catchment average vector
  CatAvgRain <- rep(NA, dim(Precp)[3])
  
  
  for (g in 1:dim(Precp)[3]) {  # for each day
    Timestep <- as.character(Time[g], format = "%Y-%m-%d %H:%M:%S")
    PrecpFlip <- apply(t(Precp[ , ,g]), 2, rev)
    OneRain <- raster::raster(PrecpFlip, xmn = -200000, ymn = -200000, xmx = 700000, ymx = 1250000)
    extractedRasterRain <- raster::extract(OneRain, NRFA[1, ], weight = TRUE, normalizeWeights = FALSE)[[1]]
    CatAvgRain[g] <- weighted.mean(extractedRasterRain[ , 1], extractedRasterRain[ , 2])
  }
    
  OUT <- data.frame(Time, round(CatAvgRain, 4))
  # appends to table rathere than writing a whole new file
  write.table(OUT, file = FNAME, row.names = FALSE, col.names = FALSE,
              quote = FALSE, sep = ",", append = TRUE)
}


# end R session
quit(save = "no", runLast = FALSE)