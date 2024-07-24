### giaves 2023-11-10
# EA project 35752: Hydrological analysis of the 2019-2021 flooding

# Main contributor: Adam Griffin
# Info: Extract highest groundwater within 5 days (BEFORE only) of given events
# at the closest GW station to the flow station recording the event

# Version 0.1: 2023-11-10. Initial development of code
# Version 0.2: 2023-11-30. Refactoring for wider distribution.
# Version 1.0: 2024-07-22. Final version for wider distribution.

## NOTE: requires source groundwater data from stations. Not supplied with data product.


#### SETUP ####
library(zoo)
library(tidyverse)
library(readxl)

#### KEY FILEPATHS ####
key_details_filename <- "Data/Metadata/Master Station Listings.xlsx" #key flow/stage metadata, one station per row
groundwater_meta_filename <- "Data/Groundwater/Station_shortlist.csv" #key groundwater station metadata, one station per row. Matches GW to flow/stage stations
gw_full_rec_folder <- "Data/Groundwater/FullRecord/" #folder containing raw GW data

gw_metadata_outfile <- "Data/Metadata/Groundwater_Metadata.csv" # file to save metadata to
gw_events_outfile <- "Data/Groundwater/Event_Antecedent_max_level_or_min_dip.csv" #file to save AMAX/AMIN data event antecedent conditions.



#### READ IN DATA ####
Master <- readxl::read_excel(key_details_filename,
                             sheet = "PostQueries_FluvialGauged")
GWStat <- readr::read_csv(groundwater_meta_filename)
GWStat <- GWStat[which(GWStat$Location != ""), ]  # only keep named columns
colnames(GWStat) <- c("Gauge", "GWGauge", "Area", "OS")

#Combine data and metadata
Master0 <- full_join(Master, GWStat, by=c("Gauge", "Area"))
Master <- (merge(Master, GWStat, all = TRUE))
Master <- Master[which(!is.na(Master$GWGauge)), ]
Master$MaxDay <- ymd("2000-01-01")

# Calculate extra metadata
for(i in seq_len(nrow(Master))){ # for each station
  # read in groundwater data
  GW <- read_csv(sprintf(paste0(gw_full_rec_folder,"%s.csv"), Master$GWGauge[i]))
  reclen <- lubridate::time_length(interval(min(GW$Day), max(GW$Day)), "year")
  por <- paste(min(GW$WY), "-", max(GW$WY))
  
  ## get max level of groundwater
  w <- which.max(GW$Level)
  Master$MaxDay[i] <- GW$Day[w]
  Master$MaxDepth[i] <-GW$Level[w]
  Master$POR[i] <- por
  Master$reclen[i] <- reclen
}
colnames(Master)[1] <- "Area"

##### SAVE METADATA #####
Master_out <- Master %>% 
  select(Area, `Gauge ID`, GWGauge, POR, reclen, MaxDay, MaxDepth)
Master_out <- Master_out %>% 
  dplyr::filter(MaxDepth > -Inf)
write_csv(Master_out, gw_metadata_outfile)


##### ANALYSIS #####
# Data processing
# Preallocation, 500 is arbitrarily bigger than the number of stations
OUT <- data.frame(Area = character(500),
                  River = character(500),
                  Gauge.Name = character(500),
                  ID = character(500),
                  NRFA = numeric(500),
                  GW.Gauge = character(500),
                  EventDate = character(500),
                  Level_AP_max = numeric(500),
                  Dip_AP_min = numeric(500))
C <- 0
for (i in 1:dim(Master)[1]) { #for each site
  
  meta_1stn <- Master[i, ]
  
  # read in groundwater data
  GW <- readr::read_csv(sprintf(paste0(gw_full_rec_folder,"%s.csv"),
                         Master$GWGauge[i]))
  GW$Day <- as.Date(GW$Day)
  
  for (j in 1:6) { #for each key event at site i
    
    # Date of event
    Ev1 <- as.Date(One[ , j+10][1])
    Ev2 <- as.Date(One[ , j+16][1])
    
    Ev <- if(!is.na(Ev2)) as.Date(Ev2) else as.Date(Ev1)
    
    if (!is.na(Ev)) {
      
      C <- C+1
      
      # Find the correct day
      EventAP <- (Ev-5):Ev
      GWsubset <- GW[which(GW$Day %in% as.Date(EventAP)), ]
      
      # get extra metadata of stage/flow station
      OUT$Area[C] <- meta_1stn$Area
      OUT$River[C] <- meta_1stn$River
      OUT$Gauge.Name[C] <- meta_1stn$Gauge
      OUT$ID[C] <- meta_1stn$`Gauge ID`
      OUT$NRFA[C] <- meta_1stn$`NRFA ID`
      OUT$GW.Gauge[C] <- meta_1stn$GWGauge
      OUT$EventDate[C] <- as.character(as.Date(Ev))
      
      DC <- which(colnames(GW) == "Dip")
      LC <- which(colnames(GW) == "Level")
      
      # maximum level over the course of the preceding five days
      if (length(LC) == 1) {
        OUT$Level_AP_max[C] <- max(GWsubset[ , LC])
      } else {
        OUT$Level_AP_max[C] <- NA
      }
      
      # maximum depth reading over preceding 5 days
      if (length(DC) == 1) {
        OUT$Dip_AP_min[C] <- min(GWsubset[ , DC])
      } else {
        OUT$Dip_AP_min[C] <- NA
      }
    }
  }
}

OUT <- OUT[1:C, ]
# remove erroneous values
OUT$Level_AP_max[which(OUT$Level_AP_max < -1000000)] <- NA
OUT$Dip_AP_min[which(OUT$Dip_AP_min > 1000000)] <- NA


##### SAVE OUTPUTS #####
write.csv(OUT,
    file = gw_events_outfile,
    row.names = FALSE)