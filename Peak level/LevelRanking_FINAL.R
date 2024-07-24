##### Adam Griffin, 2023-09-01
# EA project 35752: Hydrological analysis of the 2019-2021 flooding
# Stage extraction for event ranking of stage for all data sources
#


# Version 0.1: 2023-09-01. Initial development of code
# Version 0.2: 2024-02-01. Refactoring for wider distribution.
# Version 1.0: 2024-07-22. Final version for wider distribution.

#### Full 15-minute level data not supplied. Add to folder Data/Level.



#### SETUP ####
library(tidyverse)
library(RODBC)
library(lfstat)
library(readxl)
library(readr)

lfwy <- \(x){
  #convert date to water year
  y=lfstat::water_year(x, origin=10, assign="start")
  as.numeric(levels(y)[y])
}

rank2 <- \(x){rank(-1*x, ties="min")} #ranks biggest as 1, smallest as N

##### Key arguments #####
KeyDetails_long_filepath <- "./Data/Metadata/KeyDetails_long.csv" # key metadata, one event per row
Master_details_filepath <- "./Data/Metadata/Master Station Listings.xlsx" # key metadata, one station per row
all_amax_filepath <- "./Data/Ranks of all_level_amax.csv" # full AMAX rank data
NRFA_amax_wide_filepath <- "./Data/Flow/amax_PF12.csv" #AMAX file as provided by the Peak Flow dataset.
stage_AMAX_folder <- "./Data/Level" # folder for 15-minute stage data

sg_files_folder <- "./Data/SG WYearAMAX" # folder of supplied stage AMAX based on hydrological year

AM_IN <- "" # .csv file containing amax for all stations, one event per row, stations concatenated down table. 

Final_AMAX_out <- "./Data/all_level_amax.csv" #output file for master AMAX level data 

#### DATA IN ####
KeyDetails <- readr::read_csv(KeyDetails_long_filepath )
Master <-readxl::read_excel(Master_details_filepath,
                            sheet="PostQueries_FluvialGauged")
Master_sub <- Master %>% dplyr::select(`NRFA ID`, Area, Gauge, River, `Gauge ID`)

# read in extracted levels for given dates, based on level data.
# AM_IN was stored in oracle tables, but can be supplied by .csv

all_amax_oracle <- dplyr::left_join(AM_IN, Master_sub, by=c("STATION"="NRFA ID")) %>%
  select(STATION, DATE_TIME, STAGE, BATCH_ID, Area, Gauge, River, `Gauge ID`)
all_amax_oracle$Year <- lfwy(all_amax_oracle$DATE_TIME)

colnames(all_amax_oracle)[1:4] <- c("NRFA ID", "date", "stage", "source")
all_amax_oracle <- all_amax_oracle %>%
  dplyr::group_by(`NRFA ID`) %>%
  dplyr::mutate(rank=rank2(stage))



# read in from Peak Flow AMAX
all_amax_nrfa <- readr::read_csv(NRFA_amax_wide_filepath,
                                 col_types = paste0(rep("c", 178), collapse=""))
all_amax_nrfa <- all_amax_nrfa %>%
  dplyr::filter(item %in% c("date", "stage", "rank")) %>%
  tidyr::pivot_longer(cols=`2021`:`1851`, names_to="Year", values_to="item2") %>%
  tidyr::pivot_wider(
    id_cols=c("id", "river", "location", "catchment-area", "qmed", "pooling", "Year"),
    names_from="item",
    values_from="item2") %>%
  dplyr::select(id, Year, date, stage, rank)

# add metadata
all_amax_nrfa$id <- as.numeric(all_amax_nrfa$id)
all_amax_nrfa$Year <- as.numeric(all_amax_nrfa$Year)
all_amax_nrfa$date <- lubridate::ymd(all_amax_nrfa$date)
all_amax_nrfa$stage <- as.numeric(all_amax_nrfa$stage)
all_amax_nrfa$rank <- as.numeric(all_amax_nrfa$rank)
all_amax_nrfa <- dplyr::inner_join(all_amax_nrfa, Master_sub, by=c("id"="NRFA ID"))
all_amax_nrfa$source <- "NRFA_PF"
colnames(all_amax_nrfa)[1] <- "NRFA ID"



# read in level from sgAMAX files
sgfiles <- list.files(sg_files_folder, full.names = T)
sg_id <- sgfiles %>%
  stringr::str_split_i("[.//]+", i=5) %>%
  stringr::str_remove("^0") # strip leading zeros

df_list <- list() # initialise list



##### ANALYSIS #####

for(i in seq_along(sgfiles)){ # for each stage file
  print(paste(i, sg_id[i]))
  df_lines <- readr::read_lines(sgfiles[i])
  w <- which(stringr::str_starts(df_lines, "Time stamp,"))
  df <- readr::read_csv(sgfiles[i], skip=w,
                  col_names = c("DateTime", "stage", "flag"), col_types="cnc")
  
  # convert to date and get water year
  df$date <- date(lubridate::dmy_hms(df$DateTime))
  df$Year <- lfwy(df$DateTime)
  # compute ranks
  df$rank <- rank2(df$stage[!is.na(df$stage)])
  # add metadata
  df$`Gauge ID` <- sg_id[i]
  df$source <- "sgAMAX_file"
  df_list[[length(df_list)+1]] <- df
}
all_amax_sg <- do.call(rbind.data.frame,df_list) %>%
  dplyr::left_join(Master_sub, by=c("Gauge ID"="Gauge ID")) %>%
  dplyr::select(`NRFA ID`, date, stage, source, Gauge, `Gauge ID`, Year, rank, River, Area)



# Only keep one version of each station with priority NRFA peak flow, QCd data, non-QCd stageAMAX, manually extracted AMAX
all_amax_master <- all_amax_nrfa %>%
  dplyr::filter(`NRFA ID` %in% Master_sub$`NRFA ID`)

all_amax_master_oracle <- all_amax_oracle %>%
  dplyr::filter(`NRFA ID` %in% Master_sub$`NRFA ID`, !(`NRFA ID` %in% all_amax_master$`NRFA ID`))

all_amax_master <- rbind.data.frame(all_amax_master, all_amax_master_oracle)

all_amax_master_sg <- all_amax_sg %>%
  filter(`NRFA ID` %in% Master_sub$`NRFA ID`, !(`NRFA ID` %in% all_amax_master$`NRFA ID`))

all_amax_master <- rbind.data.frame(all_amax_master, all_amax_master_sg)




#extract AMAX from SG_combined
sgfiles <- list.files(stage_AMAX_folder, full.names = T)
sg_id <- sgfiles %>%
  stringr::str_split_i("[.//]+", i=5) %>%
  stringr::str_remove("^0") # strip leading zeroes

df_list <- list()
M_sub <- Master_sub$`Gauge ID`[!(Master_sub$`Gauge ID` %in% all_amax_master$`Gauge ID`)]
sgfiles <- sgfiles[sg_id %in% M_sub] 
  # only select files relevant to the Master spreadsheet
sg_id <- sg_id[sg_id %in% M_sub]

sg_list <- list()
for(i in 1:length(sg_id)){ # for each stage file
  print(paste(i, sg_id[i]))
  df <- readr::read_csv(sgfiles[i],
                        skip=1,
                        col_names = c("DateTime", "stage", "flag"),
                        col_types="cnc")
  df <- df[!is.na(df$DateTime),]
  # convert to date and get water year
  df$date <- date(lubridate::ymd_hms(df$DateTime))
  df$Year <- lfwy(df$date)
  
  ### Compute AMAX series from 15-minute data
  df_amax <- df %>%
    dplyr::arrange(desc(date)) %>%
    dplyr::group_by(Year) %>%
    dplyr::slice_max(stage, with_ties=F, n=1)
  df_amax$rank <- rank2(df_amax$stage[!is.na(df_amax$stage)])
  df_amax$`Gauge ID` <- sg_id[i]
  df_amax$source <- "sg15_file"
  df_amax$`NRFA ID` <- Master_sub$`NRFA ID`[Master_sub$`Gauge ID` == sg_id[i]]
  sg_list[[length(sg_list)+1]] <- df_amax
}

##### SAVE OUTPUTS #####

# Bind files together
all_amax_sg15 <- do.call(rbind.data.frame,sg_list) %>%
  dplyr::left_join(Master_sub, by=c("Gauge ID", "NRFA ID")) %>%
  dplyr::select(`NRFA ID`, date, stage, source, Gauge, `Gauge ID`, Year, rank, River, Area)

all_amax_master_sg15 <- all_amax_sg15 %>%
  dplyr::filter(`NRFA ID` %in% Master_sub$`NRFA ID`,
                !(`NRFA ID` %in% all_amax_master$`NRFA ID`))

all_amax_master <- rbind.data.frame(all_amax_master, all_amax_master_sg15)


##### Save to file
readr::write_csv(all_amax_master, Final_AMAX_out)
      
