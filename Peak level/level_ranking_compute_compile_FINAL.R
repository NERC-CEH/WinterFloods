##### Adam Griffin, 2023-09-01
# 08458: Winter Floods 2019-21
# Compute ranks of stage measurements for key events
#
# Version 0.1: 2023-09-01. Initial development of code
# Version 0.2: 2023-11-01. Refactoring for wider distribution.


##### SETUP #####
library(tidyverse)
library(readr)
library(readxl)
library(lfstat)

##### Key arguments #####
KeyDetails_long_filepath <- "./Data/Metadata/KeyDetails_long.csv"
Master_details_filepath <- "./Data/Metadata/Master Station Listings.xlsx"
all_amax_filepath <- "./Data/all_level_amax.csv"

##### Functions #####
lfwy <- \(x){
  y=lfstat::water_year(x, origin=10, assign="start")
  as.numeric(levels(y)[y])
}

rank2 <- \(a,b){rank(c(-1*a, -1*b), ties="min")[1]}

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

readr::write_csv(KeyDetails_final, "./Data/Tables_for_Reporting/Table_Level_ranking2.csv")
