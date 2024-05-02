### Adam Griffin 2023-12-16
# 08458: Winter Floods 2019-21

# Compute trends in rate of rise at all stations

# Version 0.1: 2023-12-16. Initial development of code
# Version 0.2: 2024-02-01. Refactoring for wider distribution.


setwd("P:/08458 CWI-EA 2019-21 Flood Review")

##### SETUP
library(dplyr)
library(lubridate)
library(readr)
library(tidyverse)
library(readxl)
library(Kendall)
library(mblm)

##### extra function #####
mk <- \(x,y){
  if(all(is.na(x))|all(is.na(y))){
   Z <- data.frame(tau=NA, sl=NA, S=NA, D=NA, varS=NA)
  }else{
  Z <- data.frame(Kendall::Kendall(x,y)[])
  }
  Z$MKZ <- ifelse(abs(Z$S) < 1e-10, 0, (Z$S - sign(Z$S))/sqrt(Z$varS))
  return(Z)
}

##### Key Arguments #####
key_details_filename <- "./Data/Master Station Listings UKCEH_post queries.xlsx"
key_details_long_filename <- ""

ror_folderS1 <- "./Code/RateOfRise/Data/Sg_combined"
ror_folderQ1 <- "./Code/RateOfRise/Data/Q_combined"

periods <- c(0.25, 0.5, 1, 2, 4, 6)




##### READ IN DATA #####
ror_filesS1 <- list.files(ror_folderS1, pattern="_AMAX.csv", full.names = T) #S files = stage
ror_filesQ1 <- list.files(ror_folderQ1, pattern="_AMAX.csv", full.names = T) #Q files = flow

# get station numbers from filenames
ror_namesS1 <- sapply(ror_filesS1, \(x){str_remove(str_split_i(x, "[.//_]+", i=7), "^0+")})
ror_namesQ1 <- sapply(ror_filesQ1, \(x){str_remove(str_split_i(x, "[ .//_]+", i=7), "^0+")})

ror_trends_list <- list() # empty list for dataframe



##### processing trends in stage ROR data #####
for(f in ror_filesS){
  
  stn_id <- stringr::str_remove(str_split_i(f, "[.//_]+", i=7), "^0+") #strip leading zeroes
  data <- readr::read_csv(f, show_col_types=F)
  
  for(i in seq_len(length(periods))){
    data_p <- data %>% dplyr::filter(period==periods[i], !is.na(DateTime), !is.na(ror))
    if(nrow(data_p)<3){
      L <- list(GaugeID=stn_id, period=periods[i], trend_sig=NA, trend_Z=NA,
                sen_slope=NA, type="Level", file=f)
    }else{
      trend <- mk(data_p$DateTime, data_p$ror) #compute Mann_Kendall estimator
      breakpoints <- changepoint::cpt.meanvar(data_p$ror)
      data_p$DateTimeX <- as.numeric(data_p$DateTime)/(60*60*24*365.25)
      mm <- mblm::mblm(ror~DateTimeX, data_p) # Thiel-Sen estimator (using repeated medians)
      SS <- mm$coefficients[2] # Sen Slope
      L <- list(GaugeID=stn_id, period=periods[i], trend_sig=trend$sl, trend_Z=trend$MKZ,
                sen_slope=SS, type="Level", file=f)
      ror_trends_list[[length(ror_trends_list)+1]] <- L # add to full data.frame
    }
  }
}

##### processing trends in stage ROR data #####
for(f in ror_filesQ){
  
  stn_id <- stringr::str_remove(str_split_i(f, "[ .//_]+", i=7), "^0+")
  data <- readr::read_csv(f, show_col_types=F)
  
  for(i in seq_len(length(periods))){
    data_p <- data %>% dplyr::filter(period==periods[i], !is.na(DateTime), !is.na(ror))
    
    if(nrow(data_p)<3){ # if not enough data, just set values to NA
      L <- list(GaugeID=stn_id, period=periods[i], trend_sig=NA, trend_Z=NA,
                sen_slope=NA, type="Level", file=f)
    }else{
      trend <- mk(data_p$DateTime, data_p$ror)
      data_p$DateTimeX <- as.numeric(data_p$DateTime)/(60*60*24*365.25)
      mm <- mblm::mblm(ror~DateTimeX, data_p)
      SS <- mm$coefficients[2]
      L <- list(GaugeID=stn_id, period=periods[i], trend_sig=trend$sl, trend_Z=trend$MKZ,
                sen_slope=SS, type="Flow", file=f)
      ror_trends_list[[length(ror_trends_list)+1]] <- L # add to full data.frame
    }
  }
}

# combine lists to single dataframe
ror_trends <- do.call(rbind.data.frame, ror_trends_list)

##### save to file #####
readr::write_csv(ror_trends, "./Data/Context/ROR_trends.csv")
