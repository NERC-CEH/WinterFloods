### giaves 2023-10-09
# 08458: Winter Floods 2019-21

# Main contributor: Gianni Vesuviano
# Read 30-, 60-, 90- and 180-day rainfall maxima, and add ranks and return periods

# Version 0.1: 2023-10-09. Initial development of code
# Version 0.2: 2023-11-01. Refactoring for wider distribution.


##### SETUP #####
library("zoo")
library("readxl")
library("lmom")
library("lmomRFA")

##### Key Arguments #####
long_rain_folder <- "./Data/Rainfall_long_AMAX"

##### read in data #####
CSVL <- list.files(path = long_rain_folder,
                   pattern = "Maxima_table_", full = TRUE)

ID <- unique(read.csv(CSVL[1])$ID)

#### for each table of M-day annual maxima
for (C in 1:length(CSVL)) {
  
  Rain <- read.csv(CSVL[C])
  Dur <- gsub("[^0-9]", "", CSVL[C])
  
  RainWF <- RainPlus <- data.frame(NRFA_ID = integer(0), Gauge = character(0),
                         River = character(0), ID = character(0),
                         WaterYear = integer(0), Date = character(0),
                         Total = numeric(0), Season = numeric(0),
                         Rank = integer(0), RP = numeric(0))
  
  for (i in 1:length(ID)) { # for each station
    # Get data for 1 station
    RainTemp <- Rain[which(Rain$ID == ID[i]), ]
    RainTemp <- RainTemp[rev(order(RainTemp$Total)), ]
    RainTemp$Rank <- 1:nrow(RainTemp)
    # fit GEV distribution using L-moments
    CDF <- lmom::cdfgev(RainTemp$Total, lmom::pelgev(lmom::samlmu(RainTemp$Total)))
    RainTemp$RP <- 1 / (1 - CDF)
    RainTemp <- RainTemp[order(RainTemp$WaterYear), ]
    RainTemp$WaterYear <- 1891:2020
    
    colnames(RainTemp) <- colnames(RainPlus)
    
    RainEnd <- RainTemp[which(RainTemp$WaterYear >= 2019), ]
    
    RainPlus <- rbind(RainPlus, RainTemp)
    RainWF <- rbind(RainWF, RainEnd)
    
  }
  
  colnames(RainPlus)[7:8] <- c(sprintf("Total%s", Dur), sprintf("T%sSeason", Dur))
  
  
  ##### save to file
  readr::write_csv(RainPlus,
                   sprintf("Data/Rainfall_long_duration/Maxima_rank_RP_table_%s_day.csv", Dur))
  readr::write_csv(RainWF, 
      sprintf("Data/Rainfall_long_duration/Maxima_rank_RP_table_%s_day_WF_years_only.csv", Dur))
  
}
