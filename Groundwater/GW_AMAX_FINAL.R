#### giaves 2023-09-13
# 08458: Winter Floods 2019-21

# Main contributor: GV
# Info: Load each groundwater record, extract WY AMAX

# Version 0.1: 2023-09-13. Initial development of code
# Version 0.2: 2023-11-01. Refactoring for wider distribution.

### NOTE: Source groundwater data not supplied as data product


#### SETUP ####
library(zoo)
library(lubridate)

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#### KEY FILEPATHS ####
gw_data_folder <- "Data/Groundwater"
gw_amax_outfile <- "Data/Groundwater/GW_CL_AMAX.csv"

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


for (i in 1:length(GWlist)) {
  
  GWnames <- strsplit(GWlist[i], "/")[[1]][5]
  GW <- read.csv(GWlist[i], fill = TRUE, skip = 15)
  GWname <- strsplit(GWnames, "\\.")[[1]][1]
  
  GW$Time.stamp <- lubridate::ymd_hms(GW$Time.stamp, tz="UTC")
  GW$Absolute..mAOD. <- as.numeric(GW$Absolute..mAOD.)
  NAval <- which(is.na(GW$Absolute..mAOD.))
  if (length(NAval) > 0) GW <- GW[-NAval, ]

  GW$Time.stamp <- GW$Time.stamp - hours(9)
  GW$Day <- date(GW$Time.stamp)
  
  GW2 <- aggregate(GW$Absolute..mAOD., by=list(GW$Day), max)
  colnames(GW2) <- c("Day", "Absolute..mAOD.")
  
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
  
  if (length(strsplit(GWnames, "\\-level")[[1]]) > 1){
    if (strsplit(GWnames, "\\-level")[[1]][2] == "-15min-Qualified.csv"){
      GW <- merge(GW, WintCompl)
    }
  }
  
  GW$Absolute..mAOD.[GW$Absolute..mAOD. < 0] <- 0
  GW$Absolute..mAOD.[is.na(GW$Absolute..mAOD.)] <- 0
  
  GW1 <- GW[ , c("WY", "Wint", "Absolute..mAOD.")]
  
  AMAX <- aggregate(GW1$Absolute..mAOD., by = list(GW1$WY), max)
  AMAX <- unique(AMAX)
  colnames(AMAX) <- c("WY", "Absolute..mAOD.")
  AMAX$Station <- GWname
  
  AMAX <- merge(AMAX, GW)
  AMAX <- merge(AMAX, WintCompl)
  
  LeapYear <- AMAX$WY %% 4
  LeapYear[LeapYear < 3] <- 0
  
  AMAX$WCompl <- AMAX$WCompl / (182 + LeapYear/3)
  
  AMAX <- AMAX[order(AMAX$Day), ]
  rpt <- c(1, diff(AMAX$WY))
  AMAX <- AMAX[which(rpt == 1), ]
  
  AMAX$Type <- "Level"
  
  AMAX <- AMAX[ , c(3, 1, 4, 2, 7, 5, 6)]
  
  
  AMACES <- rbind(AMACES, AMAX)
  
}

#### SAVE TO FILE ####
readr::write_csv(na.omit(AMACES), file = gw_amax_outfile)
