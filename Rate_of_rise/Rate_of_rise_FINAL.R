### Adam Griffin, 2023-07-16
# EA project 35752: Hydrological analysis of the 2019-2021 flooding

# Rate of rise calculations for level and flow measurements, using 6 different periods of calculation for rate.

# Version 0.1: 2023-07-16. Initial development of code
# Version 0.2: 2023-11-01. Refactoring for wider distribution.
# Version 1.0: 2024-07-22. Final version for wider distribution.

#### NOTE: Source flow data not provided as a data product of this project.


##### SETUP
library(lubridate)
library(tidyverse)  # contains dplyr, readr, tidyr, magrittr

find_peaks <- function (x, m = 3){
  # short version finding peaks based on changes of slope direction.
  # x   vector of data at equally-spaced timepoints
  # m   minimum distance between peaks.
  # returns indices of peaks
  
  shape <- diff(sign(diff(x, na.pad = FALSE)))
  pks <- sapply(which(shape < 0), FUN = function(i){
    z <- i - m + 1
    z <- ifelse(z > 0, z, 1)
    w <- i + m + 1
    w <- ifelse(w < length(x), w, length(x))
    if(all(x[c(z : i, (i + 2) : w)] <= x[i + 1])){
      return(i + 1)
    }else{
      return(numeric(0))
    }
  })
  pks <- unlist(pks)
  pks
}

##### Key Arguments #####
data_folder <- "" # 15 minute data, 1 station per file, columns DATETIME,VALUE,FLAG
# each file should end with _XXXX.csv where XXXX is the station identifier.

KeyDetails_filepath <- "./Data/Metadata/Master Station Listings.xlsx" # key metadata, one station per row
KeyDetails_long_filepath <- "./Data/Metadata/KeyDetails_long.csv" #key metadata, one event per row
ID_col_KD <- "Gauge ID" #column name of station identifier
date_col_KD <- "Event Date"  #column name of event identifier

period_vec <- c(0.25,0.5,1,2,4,6) # periods of rise in minutes
data_out_folder <- "./Data/Rate of Rise" # folder containing rate-of-rise timeseries for each flow/level station.




##### READ IN DATA #####
KeyDetails <- read_csv(KeyDetails_filepath) # one station per line
KeyDetails_long <- read_csv(KeyDetails_long_filepath) # one event per line
#KeyDetails <- KeyDetails %>% filter(`Rate of rise`=="Y")


data_files <- list.files(data_folder, pattern=".csv", full.names = T)
# get station names
location_names <- str_split_i(data_files, "[._//]+", i=-2)

for (i in seq_along(KeyDetails)){ # for each station
  print(i)
  stn <- KeyDetails[ID_col_KD][i]
  w <- which(location_names == stn)
  #get station data
  KeyDetails_1stn <- KeyDetails_long %>%
    dplyr::filter(.data[[ID_col_KD]] == stn)
  
  if(length(w)==0){next}
  stnname <- location_names[w]
  # Load one flow record
  print(paste(i, "-----", stnname))
  data_in <- read_csv(data_files[w],
                      col_names=c("DateTime", "Flow", "Flag"),
                      col_types="cdc") %>%
    mutate(DateTime = lubridate::ymd_hms(DateTime), day=date(DateTime))
  data_in <- na.omit(data_in)
  N <- nrow(data_in)
  NRates <- diff(lubridate::year(range(data_in$DateTime)))*5 #POT using 5 events per year, this is the number of top events selected.
  
  # preallocation
  outtable <- data.frame(DateTime=lubridate::POSIXct(),
                         period=numeric(),
                         ror=numeric(),
                         ror_rank=numeric())
  eventtable <- data.frame(DateTime=lubridate::POSIXct(),
                           period=numeric(),
                           ror=numeric(),
                           ror_rank=numeric())
  
  print("data prep done.")
  
  for(k in seq_along(period_vec)){ # for each period
    print(paste(period_vec[k], "minute Rate of Rise"))
    
    # get change in rise over period (15min timesteps = 4 per hour)
    ROR <- c(rep(NA,period_vec[k]*4), diff(data_in$Flow, period_vec[k]*4))
    ROR[is.na(ROR)] <- 0
    ROR[ROR < 0] <- 0
    FP <- find_peaks(ROR, 96) # get the peaks, with 24-hour seperation.
    thresh <- sort(ROR[FP],decreasing=T)[NRates]-0.01
    FP <- FP[ROR[FP] >= thresh] # find POT threshold that keeps POT5 events
    
    outtable_temp <- data.frame(DateTime=data_in$DateTime[FP],
                                period=period_vec[k],
                                ror=ROR[FP]/period_vec[k]) #m or m3/s /hr
    outtable_temp$ror_rank <- 
      (nrow(outtable_temp)+1) - rank(outtable_temp$ror, ties.method="last")
    # rank of rate of rise POTs
    NR <- nrow(outtable_temp)
    
    # ket metadata of event
    D <- KeyDetails_1stn[date_col_KD]
    ND <- length(D)
    eventtable_t2 <-data.frame(DateTime=lubridate::POSIXct(),
                               period=numeric(),
                               ror=numeric(),
                               ror_rank=numeric())
    
    for(d in seq_along(D)){   # for each key event
      Qd <- data_in %>% dplyr::filter(abs(difftime(day, D[d])) < days(5))
      Qmax <- max(Qd$Flow)
      Tmax <- which.max(Qd$Flow)
      # calculate the rate of rise at the time of the event.
      ROR <- c(rep(NA,period_vec[k]*4), diff(Qd$Flow, period_vec[k]*4))
      ROR[is.na(ROR)] <- 0
      ROR[ROR < 0] <- 0
      Rmax <- max(ROR)/period_vec[k] # scale to m3/s or m /hour
      
      #save to output table
      eventtable_temp <- data.frame(DateTime=D[d],
                                    period=period_vec[k],
                                    ror=Rmax,
                                    ror_rank=NA)
      eventtable_t2 <- rbind.data.frame(eventtable_t2, eventtable_temp)
    }
    
    #get rate of rise ranking
    eventtable_t2$ror_rank <- 
      (NR+ND+1) - rank(c(outtable_temp$ror,eventtable_t2$ror),
                       ties.method="last")[NR+(1:ND)]
    #})
    
    #get event rate of rise rankings
    outtable_temp$ror_rank <- 
      (NR+ND+1) - rank(c(outtable_temp$ror,eventtable_t2$ror),
                       ties.method="last")[1:NR]
    
    eventtable <- rbind.data.frame(eventtable, eventtable_t2)
    outtable <- rbind.data.frame(outtable,outtable_temp)
  }
  

##### SAVE OUTPUTS #####
  readr::write_csv(outtable, paste0(data_out_folder,stn,"ror.csv"))
  readr::write_csv(eventtable, paste0(data_out_folder,stn,"_events_ror.csv"))
  print(paste0(data_out_folder,stn,"_ror.csv"))
  gc()
}  