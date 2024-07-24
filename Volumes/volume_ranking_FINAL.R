#### Griffin, Adam. 2024-01-01
# EA project 35752: Hydrological analysis of the 2019-2021 flooding

# Main contributor: Adam Griffin
# Info: Ranking event volumes within volume AMAX series computed for Winter Floods period

# Version 0.1: 2024-01-01. Initial development of code
# Version 0.2: 2024-02-01. Refactoring for wider distribution.
# Version 1.0: 2024-07-22. Final version for wider distribution.


#### NOTE: source flow and level data not supplied as a data product in this project.

##### SETUP #####
library(dplyr)
library(lubridate)
library(readr)
library(tidyverse)
library(readxl) # contains dplyr, readr, tidyr, magrittr

rank2 <- function(a,b){rank(-1*c(a,b), ties="min", na.last="keep")[1]}
# rank of event a in timeseries b

##### Key Arguments #####
key_details_filename <- "./Data/Metadata/Master Station Listings.xlsx" # key metadata, one station per row
key_details_long_filename <- "" # key metadata,on event per row
key_storms_filename <- "./Data/Context/key_storms_out_with_ranks.csv" # key metadata, one storm per row, includes storm rankings
id_column_kd <- "Gauge ID" # column name for station identifiers
plot_folder <- "" # folder to save figures
locations_filename <- "" # .csv for locations of stations 
catchments_filename <- "./Data/Context/all_locations_rain.shp" # shapefile of all catchments including raingauges
volumes_folder <- "./Data/Volumes/" # root folder for volume data
event_volumes_filename <- "./Data/Volumes/Specified_event_volumes.csv" #  file for event volumes at all stations.

event_volume_ranks <- "./Data/Volumes/event_volumes_with_ranks.csv" # filepath for output of ranks and return periods of volumes.


##### READ IN DATA #####
key_details <- readxl::read_xlsx(key_details_filename, sheet=5)[,1:22] %>%
  dplyr::filter(!is.na(`Gauge ID`))
station_list <- unique(key_details$`Gauge ID`)
event_volumes <- readr::read_csv(event_volumes_filename)


##### Setup dataframe #####
event_volumes$Site <- stringr::str_remove(event_volumes$Site, "^0+")
event_volumes$Rank1 <- NA
event_volumes$Rank2 <- NA
event_volumes$Rank4 <- NA
event_volumes$Rank6 <- NA
event_volumes$Rank8 <- NA
event_volumes$reclen <- NA
event_volumes$RP1 <- NA
event_volumes$RP2 <- NA
event_volumes$RP4 <- NA
event_volumes$RP6 <- NA
event_volumes$RP8 <- NA


durations <- c(1,2,4,6,8)
for(d in durations){ # for each duration
  # read in volumes
  full_rec_filename <- paste0("./Data/Volumes/", d, "_week_max_Q_combined.csv")
  full_rec <- readr::read_csv(full_rec_filename, col_types="cnDnnn") 
  
  #all stations with volume for a fixed duration - period of record varies
  full_rec$Station <- stringr::str_remove(full_rec$Station, "^0")
  for(s in station_list){
    # remove trailing zeroes
    s_name <- stringr::str_remove(s, "^0+")
    # get correct event
    w <- which(event_volumes$Site == s_name)
    # get subset of data
    event_1stn <- event_volumes[w, ]
    full_rec_1stn <- full_rec %>% dplyr::filter(Station %in% c(s, s_name))
    # find period of record
    por <- sort(unique(full_rec_1stn$WY))
    for(i in seq_len(length(w))){
      # calculate the rank of each of the key events in context of amax series
      event_volumes[[paste0("Rank",d)]][w[i]] <- 
        rank2(event_1stn[[paste0("Vol",d)]][i], full_rec_1stn[[paste0("Week",d)]])
      # get record length
      event_volumes$reclen[w[i]] <- length(por)+1
      try({
        #fit GEV to get approximate return period of volume, based on AMAX series.
        L <- lmomco::lmoms(full_rec_1stn[[paste0("Week", d)]])
        event_volumes[[paste0("RP", d)]][w[i]] <-
          1 / (1 - lmomco::cdfgev(event_1stn[[paste0("Vol", d)]][i],
                                  lmomco::pargev(L)))
      })
    }
  }
}

##### SAVE OUTOUTS #####
# save to file
write_csv(event_volumes, event_volume_ranks)

