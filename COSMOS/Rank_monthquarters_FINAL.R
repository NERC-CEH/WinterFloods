### giaves 2023-09-05
# EA project 35752: Hydrological analysis of the 2019-2021 flooding

# Main contributor: Gianni Vesuviano
# Info: Rank COSMOS volumetric water content on each calendar monthquarter (not equal to week always)

# Version 0.1: 2023-09-05. Initial development of code
# Version 0.2: 2023-11-30. Refactoring for wider distribution.
# Version 1.0: 2024-07-22. Final version for wider distribution.

#### SETUP ####
library(readr)
library(lubridate)


#### KEY FILEPATHS ####
cosmos_data_folder <- "Data/COSMOS-UK_data/daily" # folder containing raw COSMOS data
cosmos_mq_outfile <- "Data/COSMOS-UK_data/Ranked_Monthquarter/" # output folder for ranked data

CF <- list.files(cosmos_data_folder, full = TRUE)
N <- length(CF)
SiteName <- rep("", N)

##### ANALYSIS #####
for (i in 1:N) { # for each COSMOS-UK station
  
  # read in data
  SiteName[i] <- strsplit(readLines(CF[i], n = 3)[3], ",")[[1]][2]
  SiteName[i] <- gsub(" ", "_", SiteName[i])
  # get metadata from header of file
  Rec <- read.csv(CF[i], skip = 5)[ , 1:2]
  colnames(Rec) <- c("Date", "VWC")
  Rec$Date <- ymd(Rec$Date)
  Rec$M <- month(Rec$Date, 6, 7)
  Rec$D <- day(substr(Rec$Date, 9, 10))
  Rec$Q <- Rec$D
  
  
  # Compute month quarter (more consistent than weeks)
  for(d in 1:dim(Rec)[1]) {
    if (Rec$M[d] == "02") {
      Rec$Q[d] <- sum(c(1, Rec$D[d]>8, Rec$D[d]>15, Rec$D[d]>21))
      # gives quarter of month in Feb (28-29 days)
    } else {
      Rec$Q[d] <- sum(c(1, Rec$D[d]>9, Rec$D[d]>16, Rec$D[d]>24))
      # gives quarter of month otherwise (30-31 days)
    }
  }
  
  
  # output data setup
  # names for month quarters
  Rec$MQ <- paste(Rec$M, Rec$Q, sep = "-Q")
  # dates for start of month quarters
  Rec$DateQ <- paste0(substr(Rec$Date, 1, 5), Rec$MQ)
  VWCQ <- aggregate(Rec$VWC, by = list(Rec$DateQ), mean, na.rm = TRUE)
  colnames(VWCQ) <- c("DateQ", "VWCQ")
  
  # remove any rows with missing values
  RecNAO <- na.omit(Rec)
  L <- aggregate(RecNAO$VWC, by = list(RecNAO$DateQ), length)
  colnames(L) <- c("DateQ", "L")
  
  # merge dates and values into single data frame
  Rec <- merge(Rec, VWCQ)
  Rec <- merge(Rec, L, all = TRUE) #outer join (all rows from both tables)
  Rec <- Rec[order(Rec$Date), ] # sort by date
  
  UMQ <- sort(unique(Rec$MQ))
  
  Rec <- Rec[ , c("DateQ", "VWCQ", "MQ", "L")] # only keep needed columns
  Rec <- unique(Rec)
  
  RA <- rep(NA, 5)
  
  
  #####  ANALYSIS #####
  for (j in 1:length(UMQ)) { # for each month quarter
    RJ <- Rec[which(Rec$MQ == UMQ[j]), ] #find the correct data for given moth quarter
    RJ <- RJ[rev(order(RJ$VWC, na.last = FALSE)), ]
    RJ$Rank <- 1:dim(RJ)[1] # calculate ranks
    RJ$Rank[is.na(RJ$VWC)] <- NA
    RJ$Of <- max(RJ$Rank, na.rm = TRUE) # find number of years in record
    RJ$Of[is.na(RJ$VWC)] <- NA
    RA <- rbind(RA, RJ)
  }
  
  ##### SAVE OUTPUTS #####
  RA <- RA[order(RA$Date), c(1, 2, 4, 5, 6)] #sort by date
  RA <- RA[-which(is.na(RA$Date)), ] # remove events without dates
  colnames(RA) <- c("Month_quarter", "Mean_VWC", "Based_on_days", "Rank", "Of")
  
  readr::write_csv(RA,
      sprintf(paste0(cosmos_mq_outfile,"%s_monthquarter.csv"), SiteName[i]))
  
}
