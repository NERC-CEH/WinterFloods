### giaves 2023-10-09
# EA project 35752: Hydrological analysis of the 2019-2021 flooding

# Main contributor: Gianni Vesuviano
# Read catchment-average daily rainfalls, find 30-, 60-, 90- and 180-day maxima with arbitrary start times within June 2019-June 2021

# Version 0.1: 2023-10-09. Initial development of code
# Version 0.2: 2023-11-01. Refactoring for wider distribution.
# Version 1.0: 2024-07-22. Final version for wider distribution.

### NOTE: source raingauge data not given as data product in this project.


#### SETUP ####
library("zoo")
library("readxl")

##### KEY FILEPATHS ####
keydetails_filepath <- "Data/Metadata/Master Station Listings.xlsx" # file containing key metadata, one station per row
catchment_rain_in <- "Data/HadUK-Grid_CatAvgDailyRain" # folder containing raw catchment average rainfall data


NDAY <- 30 # length of accumulation. Set to 30, 60, 90 or 180.

#### READ DATA IN ####
Master <- read_excel(keydetails_filepath, sheet = "PostQueries_FluvialGauged")
Master <- Master[ , c(1, 3, 4, 5)]
colnames(Master)[1] <- "NRFA"
colnames(Master)[5] <- "ID"
colnames(Master)[6:11] <- paste0("E", 1:6)
Master$NRFA <- as.numeric(Master$NRFA)

#get list of raw datafiles
RF <- list.files(path = catchment_rain_in, pattern = "full", full.names = TRUE)
RG <- as.numeric(gsub("[^0-9]", "", RF))

N <- length(RF)

C <- 0

Out0 <- data.frame(NRFA = numeric(0),
                    Gauge = character(0),
                    River = character(0),
                    Gauge.ID = character(0),
                    RS = numeric(0),
                    W = integer(0),
                    S = integer(0))

for (i in 1:N) { # for each catchment
  
  print(i)
  
  #read in data
  TS <- (read.csv(RF[i], header = FALSE))
  colnames(TS) <- c("Date", "Rain")
  
  StatInfo <- Master[which(Master$NRFA == RG[i]), ]
  
  # get period overlapping winter floods events
  Temp0 <- TS[TS$Date >= (ymd("2019-06-01")-days(NDAY/2 - 1)) &
                TS$Date <= (ymd("2021-06-30")+days(NDAY/2)),]
  
  #check whether in Winter or Summer half-year.
  Temp0$W <- 0
  Temp0$W[c((77+NDAY/2):(290+NDAY/2), (443+NDAY/2):(655+NDAY/2))]<- 1
  Temp0$S <- 1 - Temp0$W

  #calculate rolling sum over the date.
  Temp0$RS <- rollsum(Temp0$Rain, NDAY, fill = 0, align = "left")
  
  #save to table
  Bind <- data.frame(StatInfo, Temp0[which.max(Temp0$RS), c(5, 3, 4)])
  colnames(Bind) <- colnames(Out)
  Out <- rbind(Out, Bind)
  
}


##### SAVE OUTPUTS #####
colnames(Out30) <- c("NRFA_ID", "Gauge", "River", "ID", "Total", "W", "S")

write.csv(Out, sprintf("Data/Rainfall_long_AMAX/WF_AMAX1_table_%s_day.csv",
                       NDAY),
          row.names = FALSE, quote = FALSE)
