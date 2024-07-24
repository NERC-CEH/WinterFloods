##### Adam Griffin, 2023-09-01
# EA project 35752: Hydrological analysis of the 2019-2021 flooding
# Compute ranks of stage measurements for key events based on water year
#
# Version 0.1: 2023-09-01. Initial development of code
# Version 0.2: 2023-11-01. Refactoring for wider distribution.
# Version 1.0: 2024-07-22. Final version for wider distribution


##### SETUP #####
library(tidyverse) # contains dplyr, readr, tidyr, magrittr
library(readr)
library(readxl)
library(lfstat)

##### Key arguments #####
KeyDetails_long_filepath <- "./Data/Metadata/KeyDetails_long.csv" # key metadata, one event per row
Master_details_filepath <- "./Data/Metadata/Master Station Listings.xlsx" #key metadata, one station per row
all_amax_filepath <- "./Data/all_level_amax.csv" # all level amax master data file.

output_table <- "./Data/Tables_for_Reporting/Table_Level_ranking2.csv" # output file for level AMAX rankings.

##### Functions #####
lfwy <- \(x){ # calculate water year corresponding to given date
  y=lfstat::water_year(x, origin=10, assign="start")
  as.numeric(levels(y)[y])
}

rank2 <- \(a,b){rank(c(-1*a, -1*b), ties="min")[1]} # rank of event b within timeseries a, ranked most extreme as 1.

##### DATA IN #####
KeyDetails <- readr::read_csv(KeyDetails_long_filepath)
KeyDetails$rowno <- 1:nrow(KeyDetails)
Master <-readxl::read_excel(Master_details_filepath, sheet="PostQueries_FluvialGauged")

all_amax <- readr::read_csv(all_amax_filepath)

STN <- unique(Master$`Gauge ID`)


#### Table prep ####
KeyDetails$rank <- NA
KeyDetails$empiricalAEP <- NA
KeyDetails$IN_AMAX <- F

# Calculate ranks per station
for(i in seq_along(STN)){
  amax_1stn <- all_amax %>% filter(`Gauge ID` == STN[i], !is.na(stage)) 
  events_1stn <- KeyDetails %>% filter(`Gauge ID` == STN[i])
  N <- nrow(amax_1stn)
  if(N < 1 | nrow(events_1stn) < 1){next}
  ranks <- sapply(events_1stn$EventS, \(x){rank2(x,amax_1stn$stage)})
  inamax <- (ranks %% 1) > 0.01
  ranks <- floor(ranks)
  aep <- ranks/(N+1)
  KeyDetails$rank[events_1stn$rowno] <- ranks
  KeyDetails$empiricalAEP[events_1stn$rowno] <- signif(aep,3)*100
  KeyDetails$IN_AMAX[events_1stn$rowno] <- ifelse(inamax, "Yes", "No")
}

#### Output for final table ####
KeyDetails_final <- KeyDetails %>%
  filter(`Event ranking` =="Y") %>%
  select(Area, `Gauge ID`, EventDate, EventS, IN_AMAX, rank, empiricalAEP)

readr::write_csv(KeyDetails_final, output_table)
