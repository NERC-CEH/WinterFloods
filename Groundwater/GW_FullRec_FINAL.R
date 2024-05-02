### giaves 2023-09-13
# Load each groundwater record, extract WY AMAX

#### SETUP ####
rm(list = ls())
setwd("P:/08458 CWI-EA 2019-21 Flood Review")
library(zoo)
library(tidyverse)
library(lubridate)
Sys.setenv(tz = "utc")

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#### KEY FILEPATHS ####
gw_data_folder <- "Data/Groundwater/CumbriaLancashire/CumbriaLancashire"
gw_fullrec_folder <- "./Data/Groundwater/FullRecord/"

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
  print(GWname)
  
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
  
  colnames(GW) <- c("Day", "Level", "WY", "Wint")
  
  readr::write_csv(GW, paste0(gw_fullrec_folder,GWname, ".csv"))
}