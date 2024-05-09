#### Griffin, Adam. 2024-01-01
# 08458: Winter Floods 2019-21

# Main contributor: Adam Griffin
# Info: Matching storms with antecedent conditions

# Version 0.1: 2024-01-01. Initial development of code
# Version 0.2: 2023-01-11. Refactoring for wider distribution.

setwd("P:/08458 CWI-EA 2019-21 Flood Review")

##### SETUP #####
library(dplyr)
library(lubridate)
library(readr)
library(tidyverse)
library(readxl)

##### Key Arguments #####
key_details_filename <- "./Data/Context/key_details_with_gw_cosmos.csv"
key_storms_filename <- "./Data/Context/Key Storms.csv"
id_column_kd <- "Gauge ID"
catchments_filename <- "./Data/context/all_locations_rain.shp"


feh_flow_filename <- "./Data/Tables_for_Reporting/Table_FEH_stat.csv"
feh_level_filename <- "./Data/Tables_for_Reporting/Table_Level_ranking2.csv"
volume_filename <- "./Data/Volumes/event_volumes_with_ranks.csv"
short_rain_filename <- "./Data/Pluvial/Merged_rainfall_RPs.csv"
accumulation_filename <- "./Data/Context/Accumulation_events.csv"
cosmos_filename <- "./Data/Context/event_cosmos_wvc.csv"
groundwater_filename <- "./Data/Context/gw_for_closest_station.csv"
rate_of_rise_filename <- "./Data/Tables_for_Reporting/Table_14_level.csv"
storm_details_filename <- "./Data/Context/Storm_matching.csv"


##### extra functions #####
source("./Code/Context/rp_plot_group.R")

##### READ IN DATA #####
# Match rainfall, flow, level, COSMOS, groundwater, rate of rise
key_storms <- readr::read_csv(key_storms_filename)
key_details <- readr::read_csv(key_details_filename)
cosmos <- readr::read_csv(cosmos_filename)
feh_flow <- readr::read_csv(feh_flow_filename) 
feh_level <- readr::read_csv(feh_level_filename)
feh_volume <- readr::read_csv(volume_filename)
short_rain <- readr::read_csv(short_rain_filename)
accumulation <- readr::read_csv(accumulation_filename)
gw <- readr::read_csv(groundwater_filename)
storm_matching <- readr::read_csv(storm_details_filename)
rate_of_rise <- readr::read_csv(rate_of_rise_filename)

# reshape master dates to have one date per row.
master_dates_fluvial <- key_details %>%
  select(`Gauge ID`, Easting, Northing, `Event 1`:`Event 6`,
         closest_cosmos, closest_gw, Area) %>%
  pivot_longer(cols = `Event 1`:`Event 6`,
               names_to = "Event_Number",
               values_to = "Date") %>%
  na.omit() %>%
  mutate(Event_Number = as.integer(stringr::str_sub(Event_Number, -1))) %>%
  arrange(`Gauge ID`)


##### ALIGNING EVENTS #####
# Flow
feh_flow <- feh_flow %>%
  dplyr::select("ID"=Site,
                "Date"=`Event given date`,
                "Peak_flow"=`Event peak`,
                "Rank_flow"=`Rank as AMAX`,
                "AEP_flow"=`AEP Preferred`)
all_vals <- left_join(master_dates_fluvial, feh_flow,
                      by=c("Gauge ID"="ID", "Date"="Date"))

# Level
feh_level <- feh_level %>% select("Gauge ID"="Gauge ID",
                                  "EventDate"="EventDate",
                                  "EventS"="Event_Level",
                                  "IN_AMAX"="Level_in_AMAX",
                                  "rank"="Rank_level",
                                  "empiricalAEP"="AEP_level")
all_vals <- left_join(all_vals, feh_level,
                      by=c("Gauge ID"="Gauge ID", "Date"="EventDate"))


# Volume
feh_volume <- feh_volume %>% select("Gauge_ID"="Site",
                                    "Date"="Event.date",
                                    "Volume_2_week"="Vol2",
                                    "Vol_rank"="Rank2")
all_vals <- left_join(all_vals, feh_volume,
                      by=c("Gauge ID"="Gauge_ID", "Date"="Date"))

#<4-day rainfall
short_rain <- short_rain %>%
  dplyr::select("ID"=ID,
                "Location"=Location,
                "Date"=`GivenDate`,
                "Peak_depth"=Depth.6h,
                "RP_AMAX"=RP_AMAX_6h) %>%
  dplyr::mutate(Date=date(Date)) #remove time of day

# need to match raingauge location field to Gauge_ID
short_rain$Gauge_ID <- NA
for(i in 1:nrow(short_rain)){
  w <- which(key_details$Gauge == short_rain$Location[i])
  if(length(w)>0){
    short_rain$Gauge_ID[i] <- key_details$`Gauge ID`[w[1]]
  }
}

short_rain <- short_rain %>% 
  select("Gauge_ID"="Gauge_ID", 
         "Date"="Date",
         "Rain_Depth"="Peak_depth",
         "Rain_RP"="RP_AMAX"),

all_vals <- left_join(all_vals, short_rain,
                      by=c("Gauge ID"="Gauge_ID", "Date"="Date"))

# Long rainfall accumulation
accumulation <- accumulation 
all_vals <- left_join(all_vals, accumulation,
                      by=c("Gauge ID"="ID","Date"="Date"))


rate_of_rise <- rate_of_rise %>%
  dplyr::filter(KeyEvent=="Y") %>%
  dplyr::select(Station, DateTime, ror, ror_rank) %>%
  dplyr::rename("ID"=Station,
                "Date"=DateTime)
all_vals <- left_join(all_vals, rate_of_rise,
                      by=c("Gauge ID"="ID", "Date"="Date"))

# Soil moisture
cosmos <- cosmos %>%
  select("ID"="id",
         "cosmos_id"="cosmos",
         "Date"="event_date",
         "cosmos_rank"="rank",
         "VWC"="VWC")
all_vals <- left_join(all_vals, cosmos,
                      by=c("Gauge ID"="ID", "Date"="Date"))

#Groundwater levels
gw <- gw %>% 
  select("ID"="ID",
         "GW"="GW",
         "gw_rank"="rank",
         "Date"="Date")
all_vals <- left_join(all_vals, gw, by=c("Gauge ID"="ID", "Date"="Date"))


##### COINCIDENT CONDITIONS #####
# ancillary functions to work out joint antecedent conditions
f <- function(flood,flood_min=4, antec,antec_min=6){
  # use this function if both flood and antecedent have minimum thresholds for rank
  z <- 1*(flood<flood_min) + 
    10*(antec<antec_min) + 
    100*(flood>=flood_min) + 
    1000*(antec>=antec_min)
  z[is.na(flood)|is.na(antec)] <- NA
  
  z
}
g <-  function(flood,flood_maflood=20,antec,antec_min=6){
  # use this function if antecedent has min thresholds for rank, but flow(flood) has min rp threshold
  z <- 1*(flood>flood_maflood) + 10*(antec<antec_min) + 100*(flood<=flood_maflood) + 1000*(antec>=antec_min)
  z[is.na(flood)] <- NA
  z[is.na(antec)] <- NA
  z
}
all_vals <- all_vals %>% 
  dplyr::mutate(GW_and_flow = f(Rank_flow, y=gw_rank),
                cosmos_and_flow = f(Rank_flow, y=cosmos_rank, y_min=4),
                acc_and_flow =  f(Rank_flow, y=rank_D90),
                
                GW_and_level = f(Rank_level, y=gw_rank),
                cosmos_and_level = f(Rank_level, y=cosmos_rank, y_min=4),
                acc_and_level =  f(Rank_level, y=rank_D90),
                
                GW_and_ror = f(ror_rank, x_min=10, y=gw_rank),
                cosmos_and_ror = f(ror_rank, x_min=10, y=cosmos_rank, y_min=4),
                acc_and_ror =  f(ror_rank, x_min=10, y=rank_D90),
                
                GW_and_rainfall = g(Rain_RP, x_max=20, y=gw_rank),
                cosmos_and_rainfall = g(Rain_RP, x_max=20, y=cosmos_rank, y_min=4),
                acc_and_rainfall = g(Rain_RP, x_max=20, y=rank_D90),
                
                GW_and_vol = f(Vol_rank, y=gw_rank),
                cosmos_and_vol = f(Vol_rank, y=cosmos_rank, y_min=4),
                acc_and_vol = f(Vol_rank, y=rank_D90))

all_vals$storm_name <- NA
all_vals$storm_group <- NA
all_vals$storm_group_id <- NA
all_vals$storm_start_date <- NA

key_storm_groups <- key_storms %>%
  group_by(group, group_name) %>%
  summarise(start_date_gp = min(start_date)-days(2),
            end_date_gp = max(end_date)+days(2))


##### STORM AND STORM GROUP MATCHING #####
for(i in 1:nrow(all_vals)){
  w <- which(key_storms$start_date <= all_vals$Date[i] &
               key_storms$end_date >= all_vals$Date[i])
  if(length(w) > 0){
    all_vals$storm_name[i] <- key_storms$storm_name[w[1]]
    all_vals$storm_start_date[i] <- key_storms$start_date[w[1]]
  }
  
  v <- which(key_storm_groups$start_date_gp <= all_vals$Date[i] &
               key_storm_groups$end_date_gp >= all_vals$Date[i])
  if(length(v)>0){
    all_vals$storm_group[i] <- key_storm_groups$group_name[v[1]]
    all_vals$storm_group_id[i] <- key_storm_groups$group[v[1]]
  }
  
}


#### SAVE TO FILE ####
write_csv(all_vals, "./Data/Context/Aligned_Events_With_Storm_Names.csv")