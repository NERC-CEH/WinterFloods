#### giaves 2023-10-09
# 08458: Winter Floods 2019-21

# Main contributor: GV
# Info: Read 30-, 60-, 90- and 180-day rainfall maxima, and add ranks and return periods

# Version 0.1: 2023-10-09. Initial development of code
# Version 0.2: 2023-11-01. Refactoring for wider distribution.

### NOTE: source rain gauge data not given as data product in this project.

setwd("P:/08458 CWI-EA 2019-21 Flood Review")

#### SETUP
library(zoo)
library(readr)
library(lmom)
library(lmomRFA)

CSVL <- list.files(path = "Data/Rainfall_long_duration", pattern = "Maxima_table_", full = TRUE)
AMAX1 <- list.files(path = "Data/Rainfall_long_duration", pattern = "WF_AMAX1_table_", full = TRUE)

ID <- unique(readr::read_csv(CSVL[1])$ID)

for (C in 1:length(CSVL)) {
  
  # for each M-day maxima table (one for each duration)
  Rain <- readr::read_csv(CSVL[C])
  Dur <- gsub("[^0-9]", "", CSVL[C])
  
  # for each WF AMAX table (should line up with CSVL)
  A1 <- readr::read_csv(AMAX1[C])
  
  RainWF <- data.frame(NRFA_ID = integer(0),
                       Gauge = character(0),
                       River = character(0),
                       ID = character(0),
                       Total = numeric(0),
                       W = integer(0),
                       S = integer(0),
                       Rank = integer(0),
                       RP = numeric(0))
  
  for (i in 1:length(ID)) {
    # for each station
    AD <- A1[which(A1$ID == ID[i]), ]
    
    # rank the events
    RainTemp <- Rain[which(Rain$ID == ID[i]), ]
    RainTemp <- RainTemp[rev(order(RainTemp$Total)), ]
    RainTemp$Rank <- 1:nrow(RainTemp)
    
    # fit the GEV distribution to get return periods
    CDF <- lmom::cdfgev(AD[5], lmom::pelgev(lmom::samlmu(RainTemp$Total)))
    RP <- 1 / (1 - CDF)
    
    # add ranks to data.frame
    Rank <- length(which(unlist(AD[5]) < unlist(RainTemp[ , 7]))) + 1
    RainEnd <- data.frame(AD, Rank, RP)
    RainWF <- rbind(RainWF, RainEnd)
    
  }
  
  readr::write_csv(RainWF, 
      sprintf("Data/Rainfall_long_duration/Maxima_rank_RP_table_%s_day_WF_years_only.csv", Dur))
  
}
