### giaves 2023-09-11
# EA project 35752: Hydrological analysis of the 2019-2021 flooding

# Main contributor: Gianni Vesuviano
# Info: Combine 15-minute records from Q 15 and Q 15 IR
# Extract maximum volumes per water year
# 1, 2, 4, 6, 8-week maximum volumes with START DATE

# Version 0.1: 2023-09-11. Initial development of code
# Version 0.2: 2023-11-01. Refactoring for wider distribution.
# Version 1.0: 2024-07-22. Final version for wider distribution.

#### NOTE: source flow and level data not supplied as a data product in this project.

##### SETUP #####
library(zoo)
library(tidyverse)
library(lubridate)
library(readr)
library(lfstat)

##### Key Arguments #####

Q15_folder <- "./Data/Flow/Q_combined" # location of 15-minute flow data

output_vol_folder <- "./Data/Volumes/" # folder to save volume timeseries


##### READ IN DATA #####
Q15list <- list.files(path = Q15_folder, full = TRUE)
Q15names <- list.files(path = Q15_folder, full = FALSE)

NWEEK <- 1 # key argument for number of weeks to measure flow volume (1,2,4,6,8)

#preallocate
MaxNWk <- data.frame(
  Station = NA,
  WY = NA,
  DateTime = NA,
  Week = NA,
  Comp = NA
)

##### ANALYSIS #####

for(i in Q15list){ # for each 15-minute flow file
  # read in data
  Q15 <- readr::read_csv(Q15list[i],
                         col_names=c("DateTime", "Flow"),
                         col_select=1:2)
  
  #fix time, and stick to 15-min timesteps
  Q15$DateTime <- lubridate::ymd_hms(Q15$DateTime, tz="UTC")
  Q15 <- Q15[lubridate::minute(Q15$DateTime) %in% c(0,15,30,45)]
  
  # get station name
  Q15name <- strsplit(Q15names[i], "\\.")[[1]][1]
  Q15$Flow <- as.numeric(Q15$Flow)
  Q15 <- na.omit(Q15)
  
  if (dim(Q15)[1] == 0) next() # if no data, skip
  
  DTSeries <- data.frame(
    DateTime = seq(from = min(Q15$DateTime),
                   to = max(Q15$DateTime),
                   by = "15 min"))
  
  # Account for missing values, by adding in missing timesteps.
  Q15 <- full_join(Q15, DTSeries, by="DateTime")
  Q15 <- Q15[order(Q15$DateTime), ]
  Q15$Station <- Q15name
  
  # Get water year and half-year (winter/summer)
  Q15$WY <- lfstat::water_year(Q15$DateTime)
  Q15$WY <- levels(Q15$WY)[Q15$WY]
  Q15$Wint <- 1*(lubridate::month(Q15$DateTime)>3 | 
                   lubridate::month(Q15$DateTime)<10)
  
  # limit flow to zero
  Q15$Flow[Q15$Flow < 0] <- 0
  Q15$Flow[is.na(Q15$Flow)] <- 0
  
  Q15$Data <- 1*(Q15$Flow != 0) #missing data flag
  
  
  # Calculate accumulated flow, based on 15-minute data
  Q15$Week <- zoo::rollsum(Q15$Flow, 96*NWEEK*7, fill = 0, align= "left") * 0.9
  # Calculate completeness of data
  Q15$Comp <- zoo::rollsum(Q15$Data, 96*NWEEK*7, fill = 0, align= "left") / 
    (96 * 7 * NWEEK)
  
  # group data and get weekly maxima
  VM <- Q15 %>%
    dplyr::group_by(WY) %>%
    dplyr::slice_max(Week, n=1, with_ties=FALSE) %>%
    dplyr::select(Station, WY, DateTime, Week, Comp)
  MaxNWk <- rbind(MaxNWk, VM)
  
}
# Switch to datetime for output
MaxNWk$DateTime <- lubridate::ymd_hms(MaxNWk$DateTime, tz="UTC")

##### SAVE OUTPUTS #####
readr::write_csv(na.omit(MaxNWk), 
      file = paste0(output_vol_folder, N, "_week_max_Q_combined.csv"),
      row.names = FALSE)
