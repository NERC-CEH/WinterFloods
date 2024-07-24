#### giaves. 2023-02-15
# EA project 35752: Hydrological analysis of the 2019-2021 flooding

# Main contributor: Gianni Vesuviano
# Info: Read catchment-average daily rainfalls, assign to water years, aggregate monthly totals, find 1, 2, 3 and 6-month maxima
# Version 0.1: 2023-02-15. Initial development of code
# Version 0.2: 2023-11-01. Refactoring for wider distribution.
# Version 1.0: 2024-07-22. Final version for wider distribution.


### NOTE: source raingauge data not given as data product in this project.

#### SETUP ####
library("zoo")

#### KEY FILEPATHS ####
rain_in_folder <- "Data/HadUK-Grid_CatAvgDailyRain" # folder containing raw catchment average raingauge data
rainfall_nmonth_out <- "Data/Rainfall_long_duration/Maxima_table_" #output prefix for files including accumulation maxima tables. Ends "_N_month.csv", do not include this.
amax_date_out <- "Data/Rainfall_long_duration/Date_of_maxima_" #output prefix for files including dates of accumulation maxima



#### READ IN DATA ####

RF <- list.files(path = rain_in_folder, pattern = "full", full.names = TRUE)
RG <- gsub("[^0-9]", "", RF)

N <- length(RF)

# Pre-allocation
Rain1 <- array(0, dim = c(131, N+1))
Rain2 <- array(0, dim = c(131, N+1))
Rain3 <- array(0, dim = c(131, N+1))
Rain6 <- array(0, dim = c(131, N+1))
Date1 <- array(0, dim = c(131, N+1))
Date2 <- array(0, dim = c(131, N+1))
Date3 <- array(0, dim = c(131, N+1))
Date6 <- array(0, dim = c(131, N+1))

#### DATA PROCESSING ####
for (i in 1:N) { #for each station
  
  # read in the timeseries
  TS <- (read.csv(RF[i], header = FALSE))
  colnames(TS) <- c("Date", "Rain")
  TS$WY <- as.numeric(substr(TS$Date, 1, 4))
  # convert to water year
  TS$WY[as.numeric(substr(TS$Date, 6, 7)) < 10] <- TS$WY[as.numeric(substr(TS$Date, 6, 7)) < 10] - 1
  TS$WYM <- TS$WY * 1000 + as.numeric(substr(TS$Date, 6, 7))
  # check time (before/after 0900) for hydrological day
  TS$WYM[as.numeric(substr(TS$Date, 6, 7)) < 10] <- TS$WYM[as.numeric(substr(TS$Date, 6, 7)) < 10] + 100
  MonTot <- aggregate(TS$Rain, by = list(TS$WYM), sum)
  colnames(MonTot) <- c("WYM", "Rain1")
  
  # calculate N-monthly accumulations
  MonTot$WY <- as.numeric(substr(MonTot$WYM, 1, 4))
  MonTot$Rain2 <- rollsum(MonTot$Rain1, k=2, na.pad=TRUE, align = "r")
  MonTot$Rain3 <- rollsum(MonTot$Rain1, k=3, na.pad=TRUE, align = "c")
  MonTot$Rain6 <- c(NA, rollsum(MonTot$Rain1, k=6, na.pad=TRUE, align = "c")[1:1571])
  
  for (y in 1:130) { # for each year
    
    Y <- y + 1890
    MT <- MonTot[MonTot$WY == Y, ]
    
    Rain1[y+1, i+1] <- max(MT$Rain1)
    D1 <- MT$WYM[which.max(MT$Rain1)]
    # get date of maximum of a given accumulation period
    Date1[y+1, i+1] <- paste(as.numeric(substr(D1, 1, 4)) + as.numeric(substr(D1, 5, 5)), substr(D1, 6, 7), "01", sep = "-")
    Rain2[y+1, i+1] <- max(MT$Rain2)
    D2 <- MT$WYM[which.max(MT$Rain2)]
    Date2[y+1, i+1] <- paste(as.numeric(substr(D2, 1, 4)) + as.numeric(substr(D2, 5, 5)), substr(D2, 6, 7), "01", sep = "-")
    Rain3[y+1, i+1] <- max(MT$Rain3)
    D3 <- MT$WYM[which.max(MT$Rain3)]
    Date3[y+1, i+1] <- paste(as.numeric(substr(D3, 1, 4)) + as.numeric(substr(D3, 5, 5)), substr(D3, 6, 7), "01", sep = "-")
    Rain6[y+1, i+1] <- max(MT$Rain6)
    D6 <- MT$WYM[which.max(MT$Rain6)]
    Date6[y+1, i+1] <- paste(as.numeric(substr(D6, 1, 4)) + as.numeric(substr(D6, 5, 5)), substr(D6, 6, 7), "01", sep = "-")
    
  }

}

# output templating: one column per year, one station per row.
Rain1[1, ] <- c("WY", RG)
Rain1[2:131, 1] <- 1891:2020
Rain2[1, ] <- c("WY", RG)
Rain2[2:131, 1] <- 1891:2020
Rain3[1, ] <- c("WY", RG)
Rain3[2:131, 1] <- 1891:2020
Rain6[1, ] <- c("WY", RG)
Rain6[2:131, 1] <- 1891:2020

Date1[1, ] <- c("WY", RG)
Date1[2:131, 1] <- 1891:2020
Date2[1, ] <- c("WY", RG)
Date2[2:131, 1] <- 1891:2020
Date3[1, ] <- c("WY", RG)
Date3[2:131, 1] <- 1891:2020
Date6[1, ] <- c("WY", RG)
Date6[2:131, 1] <- 1891:2020

#### SAVE TO FILE ####
write.table(Rain1, paste0(rainfall_nmonth_out,"1_month.csv"),
            row.names = FALSE, col.names = FALSE, sep = ",", quote = FALSE)
write.table(Rain2, paste0(rainfall_nmonth_out,"2_month.csv"),
            row.names = FALSE, col.names = FALSE, sep = ",", quote = FALSE)
write.table(Rain3, paste0(rainfall_nmonth_out,"3_month.csv"),
            row.names = FALSE, col.names = FALSE, sep = ",", quote = FALSE)
write.table(Rain6, paste0(rainfall_nmonth_out,"6_month.csv"),
            row.names = FALSE, col.names = FALSE, sep = ",", quote = FALSE)

write.table(Date1, paste0(amax_date_out,"1_month.csv"),
            row.names = FALSE, col.names = FALSE, sep = ",", quote = FALSE)
write.table(Date2, paste0(amax_date_out,"2_month.csv"),
            row.names = FALSE, col.names = FALSE, sep = ",", quote = FALSE)
write.table(Date3, paste0(amax_date_out,"3_month.csv"),
            row.names = FALSE, col.names = FALSE, sep = ",", quote = FALSE)
write.table(Date6, paste0(amax_date_out,"6_month.csv"),
            row.names = FALSE, col.names = FALSE, sep = ",", quote = FALSE)
