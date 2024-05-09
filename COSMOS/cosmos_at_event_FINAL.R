#### Griffin, Adam. 2024-01-15
# 08458: Winter Floods 2019-21

# Main contributor: Adam Griffin
# Info: Matching storms with antecedent conditions

# Version 0.1: 2024-01-15. Initial development of code

setwd("P:/08458 CWI-EA 2019-21 Flood Review")

##### SETUP
library(dplyr)
library(lubridate)
library(readr)
library(tidyverse)
library(readxl)

##### Key Arguments #####
key_details_filename <- "./Data/Master Station Listings.xlsx"
key_details_long_filename <- ""
key_storms_filename <- "./Data/Context/Key Storms.csv"
id_column_kd <- "Gauge ID"
plot_folder <- ""
locations_filename <- ""
catchments_filename <- ""
cosmos_monthquarter_folder <- "./Data/COSMOS/Ranked_MonthQuarter"
cosmos_match_filename <- "./Data/COSMOS-UK_data/Closest_COSMOS_to_each_station.csv"
cosmos_metadata_filename <- "./Data/COSMOS-UK_data/Max_VWC_per_site.csv"

source("./Code/Context/SummarisingFunctions_FINAL.R")

# for each station
# for each event
# find nearest COSMOS station
# find VWC rank for preceding month quarter (rank and rank/reclen)

key_details <- readxl::read_xlsx(key_details_filename, sheet=5)[,1:22]
cosmos_match <- readr::read_csv(cosmos_match_filename) %>% na.omit()
cosmos_files <- list.files(cosmos_monthquarter_folder, full.names=T)

#setup
cosmos_levels_out <- list()

E1 <- which(colnames(key_details) == "Event 1")
E6 <- which(colnames(key_details) == "Event 6")

for(i in seq_len(nrow(key_details))){ # for each station
  S <- sum(!is.na(key_details[i, E1:E6])) # how many events are there?
  gid <- key_details$`Gauge ID`[i]
  c_site <- cosmos_match$Closest_COSMOS_name[cosmos_match$ID == gid]  #what is the closest COSMOS-UK site
  if(length(c_site) > 1){c_site <- c_site[2]}
  site_filename <- paste0(cosmos_monthquarter_folder,
                          "/", str_replace(c_site, " ", "_"), "_monthquarter.csv")
  if(site_filename %in% cosmos_files){
    COSMOS_in <- readr::read_csv(site_filename, col_types = "cnnnn")
  }else{
    next
  }
  for(j in seq_len(S)){ # for each event
    L <- list()
    L$id <- gid
    L$cosmos <- c_site
    y <- key_details[[E1 + (j-1)]][i]
    L$event_date <- y
    L$monthquarter <- monthquarter(y)
    quarterdate <- paste0(
      year(y), "-", str_pad(month(y), 2, "left", "0"), "-Q", L$monthquarter)
    w <- which(COSMOS_in$Month_quarter == quarterdate) - 1
    # extract data from cosmos data
    L$VWC <- COSMOS_in$Mean_VWC[w]
    L$rank <- COSMOS_in$Rank[w]
    L$reclen <- COSMOS_in$Of[w]
    L$monthquarter <- COSMOS_in$Month_quarter[w]
    cosmos_levels_out[[length(cosmos_levels_out)+1]] <- data.frame(L)
    }
}

# convert to data_frame and save to file
cosmos_levels_data_frame <- do.call(rbind.data.frame, cosmos_levels_out)
readr::write_csv(cosmos_levels_data_frame, "./Data/Context/event_cosmos_wvc.csv")
