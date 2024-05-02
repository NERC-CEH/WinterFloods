### giaves 2023-09-13
# Load each groundwater record, extract WY AMIN

#### SETUP ####
rm(list = ls())
setwd("P:/08458 CWI-EA 2019-21 Flood Review")
library(zoo)
library(lubridate)
Sys.setenv(tz = "utc")

Mode <- function(x) {
  ux <- unique(x)
  ux[which.min(tabulate(match(x, ux)))]
}

#### KEY FILEPATHS ####
gw_data_folder <- "Data/Groundwater/CumbriaLancashire/CumbriaLancashire"
gw_amin_outfile <- "Data/Groundwater/GW_CL_AMIN.csv"

#### READ IN DATA ####
GWlist <- list.files(path = gw_data_folder, full = TRUE)
GWlistminus <- list.files(path = gw_data_folder, pattern = "exp", full = TRUE)
GWlist <- GWlist[!(GWlist %in% GWlistminus)]

# Pre-allocation
AMINS <- data.frame(Station = NA,
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
  
  GW2 <- aggregate(GW$Absolute..mAOD., by=list(GW$Day), min)
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
  WintCompl <- aggregate(GW$Wint, by=list(GW$WY), sum)
  colnames(WintCompl) <- c("WY", "WCompl")
  
  if (length(strsplit(GWnames, "\\-level")[[1]]) > 1){
    if (strsplit(GWnames, "\\-level")[[1]][2] == "-15min-Qualified.csv"){
      GW <- merge(GW, WintCompl)
    }
  }
  
  GW$Absolute..mAOD.[GW$Absolute..mAOD. < 0] <- 0
  GW$Absolute..mAOD.[is.na(GW$Absolute..mAOD.)] <- 0
  
  GW1 <- GW[ , c("WY", "Wint", "Absolute..mAOD.")]
  
  AMIN <- aggregate(GW1$Absolute..mAOD., by = list(GW1$WY), min)
  AMIN <- unique(AMIN)
  colnames(AMIN) <- c("WY", "Absolute..mAOD.")
  AMIN$Station <- GWname
  
  AMIN <- merge(AMIN, GW)
  AMIN <- merge(AMIN, WintCompl)
  
  LeapYear <- AMIN$WY %% 4
  LeapYear[LeapYear < 3] <- 0
  
  AMIN$WCompl <- AMIN$WCompl / (182 + LeapYear/3)
  
  AMIN <- AMIN[order(AMIN$Day), ]
  rpt <- c(1, diff(AMIN$WY))
  AMIN <- AMIN[which(rpt == 1), ]
  
  AMIN$Type <- "Level"
  
  AMIN <- AMIN[ , c(3, 1, 4, 2, 7, 5, 6)]
  
  
  AMINS <- rbind(AMINS, AMIN)
  
}

#### SAVE TO FILE ####
readr::write_csv(na.omit(AMINS), file = gw_amin_outfile)
