#### giaves 2023-09-13
# EA project 35752: Hydrological analysis of the 2019-2021 flooding

# Main contributor: Gianni Vesuviano
# Info: Load each groundwater level record, extract AMAX based on the UK hydrological water year

# Version 0.1: 2023-09-13. Initial development of code
# Version 0.2: 2023-11-01. Refactoring for wider distribution.
# Version 1.0: 2024-07-22. Final version for wider distribution.

### NOTE: Source groundwater data not supplied as data product


#### SETUP ####
library(zoo)
library(lubridate)

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#### KEY FILEPATHS ####
gw_data_folder <- "Data/Groundwater" # folder containing raw groundwater depth data
gw_amax_outfile <- "Data/Groundwater/GW_CL_AMAX.csv" # output of AMAX of groundwater data

#### READ IN DATA ####
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


for (i in 1:length(GWlist)) { # for each groundwater station
  
  # get filenames
  GWnames <- strsplit(GWlist[i], "/")[[1]][5]
  # read in data
  GW <- read.csv(GWlist[i], fill = TRUE, skip = 15)
  # get station names
  GWname <- strsplit(GWnames, "\\.")[[1]][1]
  
  # get time data
  GW$Time.stamp <- lubridate::ymd_hms(GW$Time.stamp, tz="UTC")
  # make sure depth data is correctly formatted
  GW$Absolute..mAOD. <- as.numeric(GW$Absolute..mAOD.)
  NAval <- which(is.na(GW$Absolute..mAOD.))
  if (length(NAval) > 0) GW <- GW[-NAval, ] #remove NA values

  GW$Time.stamp <- GW$Time.stamp - hours(9) # need to shift time to match start of hydrological day
  GW$Day <- date(GW$Time.stamp)
  
  # find day maxima
  GW2 <- aggregate(GW$Absolute..mAOD., by=list(GW$Day), max)
  colnames(GW2) <- c("Day", "Absolute..mAOD.")
  
  # work out hydrological years and AMAMX
  GW$WY <- year(GW$Time.stamp)
  GW$WY[which(month(GW$Time.stamp) < 10)] <- GW$WY[which(month(GW$Time.stamp) < 10)] - 1
  GW$Wint <- as.numeric(month(GW$Time.stamp))
  GW$Wint[GW$Wint > 3 & GW$Wint < 10] <- 0
  GW$Wint[GW$Wint > 0] <- 1
  GW$Time.stamp <- GW$Time.stamp + hours(9)
  
  GW <- GW[ , c("Day", "Absolute..mAOD.", "WY", "Wint")]
  
  GW <- merge(GW2, GW)
  GW <- unique(GW)
  
  # Remove incomplete years from 15-minute records only
  WintCompl <- aggregate(GW$Wint, by=list(GW$WY),sum)
  colnames(WintCompl) <- c("WY", "WCompl")
  
  # combine daily and sub-daily data, only using complete sub-daily years
  if (length(strsplit(GWnames, "\\-level")[[1]]) > 1){
    if (strsplit(GWnames, "\\-level")[[1]][2] == "-15min-Qualified.csv"){
      GW <- merge(GW, WintCompl)
    }
  }
  
  # cap values at zero
  GW$Absolute..mAOD.[GW$Absolute..mAOD. < 0] <- 0
  GW$Absolute..mAOD.[is.na(GW$Absolute..mAOD.)] <- 0
  
  GW1 <- GW[ , c("WY", "Wint", "Absolute..mAOD.")]
  
  # compute AMAX
  AMAX <- aggregate(GW1$Absolute..mAOD., by = list(GW1$WY), max)
  AMAX <- unique(AMAX)
  colnames(AMAX) <- c("WY", "Absolute..mAOD.")
  AMAX$Station <- GWname
  
  AMAX <- merge(AMAX, GW)
  AMAX <- merge(AMAX, WintCompl)
  
  # account for leap years
  LeapYear <- AMAX$WY %% 4
  LeapYear[LeapYear < 3] <- 0
  
  # check for completion of data in terms of fraction of year
  AMAX$WCompl <- AMAX$WCompl / (182 + LeapYear/3)
  
  # sort by date
  AMAX <- AMAX[order(AMAX$Day), ]
  rpt <- c(1, diff(AMAX$WY))
  AMAX <- AMAX[which(rpt == 1), ]
  
  AMAX$Type <- "Level"
  
  AMAX <- AMAX[ , c(3, 1, 4, 2, 7, 5, 6)]
  
  
  AMACES <- rbind(AMACES, AMAX)
  
}

#### SAVE TO FILE ####
readr::write_csv(na.omit(AMACES), file = gw_amax_outfile)
