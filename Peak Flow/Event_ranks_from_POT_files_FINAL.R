# giaves 2023-08-21
# EA project 35752: Hydrological analysis of the 2019-2021 flooding

# Load compute ranks from annual maxima at NRFA stations from .pt files.
# Needs Enhanced Single Site analysis outputs from WINFAP (not included)

# Version 0.1: 2023-08-21. Initial development of code
# Version 0.2: 2023-11-01. Refactoring for wider distribution.
# Version 1.0: 2024-07-22. Final version for wider distribution.


##### SETUP #####
library(readr)

#### Key arguments #####
NRFA_folderpath <- "./WINFAP-FEH_v11"  #folder for V11 of NRFA Peak Flow data
ESS_data_filepath <- "Data/Flow/ESS-on_NRFA-11.csv" # ESS results from NRFA V11
list_of_stations <- "Data/Flow/Stations_not_on_batch_AMAX.csv" # .csv of extra stations not in NRFA

POT_ranks_outfile <- "Data/Flow/POT_ranks_from_files.csv" # file for POT ranks output

##### READ IN DATA #####

PF <- list.files(path = NRFA_folderpath, pattern = ".pt", full = TRUE, recursive = TRUE)

ESS <- readr::read.csv(ESS_data_filepath)

Stations <- unlist(read.csv(list_of_stations, header = FALSE))

PT <- 0
POTS <- c(NA,NA,NA,NA)

##### Analysis #####
for (i in 1:length(Stations)) {
  PT <- which(grepl(Stations[i], PF, fixed = TRUE))
  if (length(PT) == 1) {
    POT <- readLines(PF[PT]) # find first POT value
    POT <- read.csv(PF[PT], skip = which(POT=="[POT Values]"), header = FALSE)
    POT <- na.omit(POT)
    POT$V1 <- as.Date(POT$V1, format = "%d %b %Y")
    POT <- cbind(Stations[i], POT)
    POT <- POT[rev(order(POT$V2)), ] # compute rankings
    POT$RANK <- 1:dim(POT)[1]
    POT <- POT[order(POT$V1), ]
    POT$MIN_DATE <- min(POT$V1)
    POT$MAX_DATE <- max(POT$V1)
  }
  POTS <- rbind(POTS, POT) # add to dataframe
}

POTS <- POTS[POTS$V1 >= as.Date("2019-01-01"), ] # filter to WF19-21 study period
POTS <- na.omit(POTS)


##### Save to file #####
colnames(POTS) <- c("STATION", "DATE", "FLOW", "STAGE", "RANK", "MIN_DATE", "MAX_DATE")
write.csv(POTS, file = POT_ranks_outfile, row.names = FALSE)
