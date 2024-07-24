#### Griffin, Adam. 2024-01-01
# EA project 35752: Hydrological analysis of the 2019-2021 flooding

# Main contributor: Adam Griffin
# Info: Getting covariates for nonstationary analysis: specifically annual and
# seasonal rainfall and flow totals.

# Version 0.1: 2024-01-01. Initial development of code
# Version 1.0: 2024-07-22. Final version for wider distribution.

##### SETUP #####
library(tidyverse)
library(lfstat)
library(pracma)

rainfall_path <- "./Data/HadUK-Grid_CatAvgDailyRain" # HADUK catchment average daily rainfall folder
dates_path <-  "./Data/Nonstationary/dates_of_events.csv" # key dates of events for non-stationary analysis
events_path <- "./Data/Metadata/KeyDetails_Long.csv" #key metadata of events, one event per row.
NRFA_path <- "./NRFA_PF_V12/suitable-for-pooling" #NRFA Peak flow dataset of "suitable for pooling" stations. Needed for the nonstat package.


#### READ IN DATA ####
RF <- list.files(path = rainfall_path, pattern = "full", full.names = TRUE)
stnno <- as.numeric(satringr::str_split_i(RF, "[-//.]+", i=-5))
KeyLocs <- readr::read_csv(dates_path)
KeyEvents <- readr::read_csv(events_path)


KeyLocs <- KeyEvents %>% select(`NRFA ID`, `Gauge ID`, )

# List of stations for nonstationary analysis with physical covariates
nonstLocs <- c(25009, 27009, 27029, 27034, 28060, 33034, 39014, 39089, 47004,
               53004, 68001, 69023, 70004, 71009, 72005, 73005, 73009)

# just keep rainfall for the relevant stations
RF <- RF[stnno %in% nonstLocs]
stnno <- stnno[stnno %in% nonstLocs]

RFAMAX <- list.files(NRFA_path, pattern=".am", full.names=T)
AMAXstns <- as.numeric(stringr::str_split_i(RFAMAX, pattern="[.//]+", i=-2))

RA_list <- list()
seasonal_list <- list()
annual_list <- list()

#### ANALYSIS ####

# detrending data function (using "linear polynomial" and adding back on mean.
dtrd <- \(x, y){y - pracma::polyval(pracma::polyfit(x, y, 1), x) + mean(y, na.rm=T)}

#Calculate Seasonal and Annual Rainfall Statistics

for(i in seq_along(RF)){ # for each station
  # Get rainfall data
  print(RF[i])
  TS <- readr::read_csv(RF[i], col_names = c("Date","Rainfall"),
                 col_types = "Dn")
  TS$season <- c("DJF","DJF","MAM","MAM","MAM","JJA","JJA","JJA","SON","SON","SON","DJF")[month(TS$Date)]
  # Switch to hydrologcial year
  TS$WY <- lfstat::water_year(TS$Date, origin="Oct")
  TS$WY <- as.numeric(levels(TS$WY)[TS$WY])
  
  # Summarise to season and annual
  seasonal <- TS %>%
    group_by(WY, season) %>%
    summarise(seasRain=sum(Rainfall), .groups="drop")

  seasonal <- seasonal %>%
    group_by(season) %>%
    mutate(detrend=dtrd(WY, seasRain)) %>% ungroup()
  
  seasonal_list[[i]] <- seasonal
  
  # get totals
  annual <- TS %>%
    group_by(WY) %>%
    summarise(ann=sum(Rainfall))
  
  # detrend the totals
  annual$detrend <- dtrd(annual$WY, annual$ann)
  
  annual_list[[i]] <- annual
  
  # Read in AMAX data (needs to work out how many lines to skip)
  RL <- readr::read_lines(RFAMAX[i])
  RA <- readr::read_csv(RFAMAX[i], skip=which(RL == "[AM Values]"),
                 col_names = c("Date","Flow","Stage"),
                 col_types="cnn")
  RA <- RA[-nrow(RA),]
  RA$Date <- dmy(RA$Date)
  RA$Season <- c("DJF","DJF","MAM","MAM","MAM","JJA","JJA","JJA","SON","SON","SON","DJF")[month(RA$Date)]
  RA$WY <- lfstat::water_year(RA$Date, origin="Oct")
  RA$WY <- as.numeric(levels(RA$WY)[RA$WY])
  
  #Get seasonal and annual maximum flow
  RA$DJF <- NA
  RA$MAM <- NA
  RA$JJA <- NA
  RA$SON <- NA
  RA$DJF_dt <- NA
  RA$MAM_dt <- NA
  RA$JJA_dt <- NA
  RA$SON_dt <- NA
  RA$annual <- NA
  RA$seasonal <- NA
  RA$seasonal_dtrd <- NA
  RA$annual_dtrd <- NA
  for(j in seq_len(nrow(RA))){
    for(S in c("DJF", "MAM","JJA","SON")){
      RA[j,S] <- seasonal %>%
        dplyr::filter(season==S, WY==RA$WY[j]) %>%
        dplyr::select(seasRain) %>%
        as.numeric()
      RA[j,paste0(S,"_dt")] <- seasonal %>%
        dplyr::filter(season==S, WY==RA$WY[j]) %>%
        dplyr::select(detrend) %>%
        as.numeric()
    }
    # Calculate seasonal and annual statistics
    RA$seasonal[j] <- seasonal %>%
      dplyr::filter(season==RA$Season[j], WY==RA$WY[j]) %>%
      dplyr::select(seasRain) %>%
      as.numeric()
    RA$seasonal_dtrd[j] <- seasonal %>%
      dplyr::filter(season==RA$Season[j], WY==RA$WY[j]) %>%
      dplyr::select(detrend) %>%
      as.numeric()
    RA$annual[j] <- annual %>%
      dplyr::filter(WY==RA$WY[j]) %>%
      dplyr::select(ann) %>%
      as.numeric()
    RA$annual_dtrd[j] <- annual %>%
      dplyr::filter(WY==RA$WY[j]) %>%
      dplyr::select(detrend) %>%
      as.numeric()
  }
  
  # final metadata
  RA$Station <- stnno[i]
  RA_list[[i]] <- RA
  
  RA <- RA %>% dplyr::select(WY, annual, DJF, MAM, JJA, SON)
  colnames(RA)[1] <- "Year"
  
  
  #### SAVE DATA TO FILE ####
  readr::write_csv(RA, paste0("./Data/NONST/Inputs/",stnno[i],"_covariates.csv"))
  print("Complete")
}
# Save provisional RData in case of failed runs
save(list=c("seasonal_list", "annual_list", "RA_list"), file = "./Data/seasonal3.rda")

