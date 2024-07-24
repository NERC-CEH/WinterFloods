### Adam Griffin 2023-12-16
# EA project 35752: Hydrological analysis of the 2019-2021 flooding

# Compute trends in rate of rise at all stations based on rates of rise in level and flow.

# Version 0.1: 2023-12-16. Initial development of code
# Version 0.2: 2024-02-01. Refactoring for wider distribution.
# Version 1.0: 2024-07-22. Final version for wider distribution.

#### NOTE: Source level and flow data not supplied as a data product in this project.

##### SETUP
library(dplyr)
library(lubridate)
library(readr)
library(tidyverse) # contains dplyr, readr, tidyr, magrittr
library(readxl)
library(Kendall)
library(mblm)

##### extra function #####
mk <- \(x,y){ # alternate calculation of Mann-Kendall test statistics
  if(all(is.na(x))|all(is.na(y))){
   Z <- data.frame(tau=NA, sl=NA, S=NA, D=NA, varS=NA)
  }else{
  Z <- data.frame(Kendall::Kendall(x,y)[])
  }
  Z$MKZ <- ifelse(abs(Z$S) < 1e-10, 0, (Z$S - sign(Z$S))/sqrt(Z$varS))
  return(Z)
}

##### Key Arguments #####
key_details_filename <- "./Data/Metadata/Master Station Listings.xlsx" # key metadata, one station per row
key_details_long_filename <- "" # key metadata, one event per row

ror_folderS1 <- "./Data/RateOfRise/Sg_combined" #folder containing level rate of rise outputs
ror_folderQ1 <- "./Data/RateOfRise/Q_combined" #folder containing flow rate of rise outputs

ror_trends_outfile <- "./Data/Context/ROR_trends.csv" # filename for trend test outputs

periods <- c(0.25, 0.5, 1, 2, 4, 6) #durations over which RoR is calculated




##### READ IN DATA #####
ror_filesS1 <- list.files(ror_folderS1, pattern=".csv", full.names = T) #S files = stage
ror_filesQ1 <- list.files(ror_folderQ1, pattern=".csv", full.names = T) #Q files = flow

# get station numbers from filenames
ror_namesS1 <- sapply(ror_filesS1,
                      \(x){str_remove(str_split_i(x, "[.//_]+", i=7), "^0+")})
ror_namesQ1 <- sapply(ror_filesQ1,
                      \(x){str_remove(str_split_i(x, "[ .//_]+", i=7), "^0+")})

ror_trends_list <- list() # empty list for dataframe


##### PROCESSING #####

##### processing trends in stage ROR data #####
for(f in ror_filesS){ # for each stage station
  
  stn_id <- stringr::str_remove(str_split_i(f, "[.//_]+", i=7), "^0+") #strip leading zeroes
  data <- readr::read_csv(f, show_col_types=F)
  
  for(i in seq_len(length(periods))){ # for each RoR period
    data_p <- data %>%
      dplyr::filter(period==periods[i], !is.na(DateTime), !is.na(ror))
    if(nrow(data_p)<3){ # can't calculate MK on fewer than 3 rows. 
      L <- list(GaugeID=stn_id, period=periods[i], trend_sig=NA, trend_Z=NA,
                sen_slope=NA, type="Level", file=f)
    }else{
      trend <- mk(data_p$DateTime, data_p$ror) #compute Mann_Kendall estimator
      breakpoints <- changepoint::cpt.meanvar(data_p$ror)
      data_p$DateTimeX <- as.numeric(data_p$DateTime)/(60*60*24*365.25) # sec/yr
      mm <- mblm::mblm(ror~DateTimeX, data_p) # Thiel-Sen estimator (using repeated medians)
      SS <- mm$coefficients[2] # Sen Slope
      L <- list(GaugeID=stn_id, period=periods[i],
                trend_sig=trend$sl, trend_Z=trend$MKZ,
                sen_slope=SS, type="Level", file=f)
      ror_trends_list[[length(ror_trends_list)+1]] <- L # add to full data.frame
    }
  }
}

##### processing trends in stage ROR data #####
for(f in ror_filesQ){ #for each flow station
  
  stn_id <- stringr::str_remove(str_split_i(f, "[ .//_]+", i=7), "^0+") # remove leading zeroes
  data <- readr::read_csv(f, show_col_types=F)
  
  for(i in seq_len(length(periods))){ # for each RoR period
    data_p <- data %>%
      dplyr::filter(period==periods[i], !is.na(DateTime), !is.na(ror))
    
    if(nrow(data_p)<3){ # can't calculate MK on fewer than 3 rows. 
      L <- list(GaugeID=stn_id, period=periods[i], trend_sig=NA, trend_Z=NA,
                sen_slope=NA, type="Level", file=f)
    }else{
      trend <- mk(data_p$DateTime, data_p$ror)
      data_p$DateTimeX <- as.numeric(data_p$DateTime)/(60*60*24*365.25) #sec/yr
      mm <- mblm::mblm(ror~DateTimeX, data_p)
      SS <- mm$coefficients[2]
      L <- list(GaugeID=stn_id, period=periods[i],
                trend_sig=trend$sl, trend_Z=trend$MKZ,
                sen_slope=SS, type="Flow", file=f)
      ror_trends_list[[length(ror_trends_list)+1]] <- L # add to full data.frame
    }
  }
}

##### SAVE OUTPUTS #####

# combine lists to single dataframe
ror_trends <- do.call(rbind.data.frame, ror_trends_list)

##### save to file #####
readr::write_csv(ror_trends, ror_trends_outfile)
