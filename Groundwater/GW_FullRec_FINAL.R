#### giaves 2023-09-13
# EA project 35752: Hydrological analysis of the 2019-2021 flooding

# Main contributor: Gianni Vesuviano
# Info: Load each groundwater record, extract AMAX from the water year for groundwater data.

# Version 0.1: 2023-09-13. Initial development of code
# Version 0.2: 2023-11-01. Refactoring for wider distribution.
# Version 1.0: 2024-07-22. Final version for wider distribution.

### NOTE: source groundwater data not given as data product in this project.


#### SETUP ####
library(zoo)
library(tidyverse)
library(lubridate)


Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#### KEY FILEPATHS ####
gw_data_folder <- "Data/Groundwater" # folder of raw groundwater data
gw_fullrec_folder <- "./Data/Groundwater/FullRecord/" # folder of full records of groudnwater data

GWlist <- list.files(path = gw_data_folder, full = TRUE)
GWlistminus <- list.files(path = gw_data_folder, pattern = "exp", full = TRUE)
GWlist <- GWlist[!(GWlist %in% GWlistminus)]

# Pre-allocation
AMACES <- data.frame(Station = NA,
                     WY = NA,
                     Day = NA,
                     Absolute..mAOD. = NA,
                     Type = NA,
                     Wint = NA,
                     WCompl = NA)

for (i in 1:length(GWlist)) { # for each groundwater data file
  
  # get filename
  GWnames <- strsplit(GWlist[i], "/")[[1]][5]
  
  GW <- read.csv(GWlist[i], fill = TRUE, skip = 15)
  
  # get station name
  GWname <- strsplit(GWnames, "\\.")[[1]][1]
  print(GWname)
  
  # check time data
  GW$Time.stamp <- lubridate::ymd_hms(GW$Time.stamp, tz="UTC")
  GW$Absolute..mAOD. <- as.numeric(GW$Absolute..mAOD.)
  NAval <- which(is.na(GW$Absolute..mAOD.))
  if (length(NAval) > 0) GW <- GW[-NAval, ]

  GW$Time.stamp <- GW$Time.stamp - hours(9) # hydrological day starts at 0900
  GW$Day <- date(GW$Time.stamp)
  
  GW2 <- aggregate(GW$Absolute..mAOD., by=list(GW$Day), max)
  colnames(GW2) <- c("Day", "Absolute..mAOD.")
  
  # check AMAX during hydrological year (starts in October)
  GW$WY <- year(GW$Time.stamp)
  GW$WY[which(month(GW$Time.stamp) < 10)] <- GW$WY[which(month(GW$Time.stamp) < 10)] - 1
  GW$Wint <- as.numeric(month(GW$Time.stamp))
  GW$Wint[GW$Wint > 3 & GW$Wint < 10] <- 0
  GW$Wint[GW$Wint > 0] <- 1
  GW$Time.stamp <- GW$Time.stamp + hours(9)
  
  GW <- GW[ , c("Day", "Absolute..mAOD.", "WY", "Wint")]
  
  GW <- merge(GW2, GW)
  GW <- unique(GW)
  
  colnames(GW) <- c("Day", "Level", "WY", "Wint")
  
  ##### SAVE OUTPUTS #####
  # writes full record of groundwater level data
  readr::write_csv(GW, paste0(gw_fullrec_folder,GWname, ".csv"))
}