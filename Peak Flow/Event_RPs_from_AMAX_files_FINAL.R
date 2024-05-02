# giaves 2023-08-21
# 08458: Winter Floods 2019-21

# Load compute return periods from annual maxima at NRFA stations.
# Needs Enhanced Single Site analysis outputs from WINFAP (not included)

# Version 0.1: 2023-08-21. Initial development of code
# Version 0.2: 2023-11-01. Refactoring for wider distribution.


setwd("P:/08458 CWI-EA 2019-21 Flood Review")

##### SETUP #####
library(readr)

#### Key arguments #####
NRFA_folderpath <- "./WINFAP-FEH_v12"
ESS_data_filepath <- "Data/WINFAP-ESS/ESS-on_NRFA-11.csv"
list_of_stations <- "Code/WINFAP-ESS/Stations_not_on_batch_AMAX.csv"

PF <- list.files(path = NRFA_folderpath, pattern = ".am",
                 full = TRUE, recursive = TRUE)


ESS <- readr::read_csv(ESS_data_filepath)
Stations <- unlist(read.csv(list_of_stations))

##### Analysis #####
PT <- 0
AMS <- c(0,0,0,0)
for (i in 1:length(Stations)) { # for each station
  PT <- which(grepl(Stations[i], PF, fixed = TRUE)) # find the .am file
  if (length(PT) == 1) {
    AM <- readLines(PF[PT])
    AM <- read.csv(PF[PT], skip = which(AM=="[AM Values]"), header = FALSE)
      # find the line which has the first AMAX
    AM$V1 <- as.Date(AM$V1, format = "%d %b %Y")
    AM <- AM[AM$V1 >= as.Date("2019-01-01"), ] # get dates from WF study period
    AM <- cbind(Stations[i], AM)
    AMS <- rbind(AMS, AM)
  }
}

AMS <- na.omit(AMS)
AMS <- AMS[-1, ]
colnames(AMS) <- c("STATION", "DATE", "FLOW", "STAGE")

# Compute return period using ESS GLO parameters
AMS$RETURN_PERIOD_AM <- 0
for (i in 1:dim(AMS)[1]) {
  Site <- ESS[which(ESS$Station == AMS$STATION[i]), ]
  AMS$RETURN_PERIOD_AM[i] <- 1 + (1 - Site$GLOkappa/Site$GLObeta * (AMS$FLOW[i]/Site$QMED - 1))^(-1/Site$GLOkappa)
}

# conversion to POT return periods
AMS$RETURN_PERIOD_AM <- 1 / -log((AMS$RETURN_PERIOD_AM - 1) / AMS$RETURN_PERIOD_AM)

##### Save to file #####
readr::write_csv(AMS, "Data/WINFAP-ESS/AM_return_periods_from_files.csv")
