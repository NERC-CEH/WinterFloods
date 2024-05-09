### giaves 2023-09-05
# 08458: Winter Floods 2019-21

# Main contributor: GV
# Info: Rank COSMOS volumetric water content on each calendar day

# Version 0.1: 2023-09-05. Initial development of code
# Version 0.2: 2023-11-30. Refactoring for wider distribution.


#### SETUP ####
library(readr)
library(lubridate)

#### KEY FILEPATHS ####
cosmos_daily_folder <- "Data/COSMOS-UK_data/daily"
cosmos_dayrank_outfile <- "Data/COSMOS-UK_data/Ranked_Daily/"

CF <- list.files(cosmos_daily_folder, full = TRUE)
N <- length(CF)

SiteName <- rep("", N)

for (i in 1:N) {
  
  SiteName[i] <- strsplit(readLines(CF[i], n = 3)[3], ",")[[1]][2]
  SiteName[i] <- gsub(" ", "_", SiteName[i])
  Rec <- read.csv(CF[i], skip = 5)[ , 1:2]
  colnames(Rec) <- c("Date", "VWC")
  Rec$Date <- as.POSIXlt(Rec$Date)
  Rec$MD <- substr(Rec$Date, 6, 10)
  UMD <- sort(unique(Rec$MD))
  
  RA <- rep(NA, 5)
  for (j in 1:length(UMD)) { #For each day of the year
    # Find the data from that DoY
    RJ <- Rec[which(Rec$MD == UMD[j]), ]
    # Rank data
    RJ <- RJ[rev(order(RJ$VWC, na.last = FALSE)), ]
    RJ$Rank <- 1:dim(RJ)[1]
    RJ$Rank[is.na(RJ$VWC)] <- NA
    # Record Length
    RJ$Of <- max(RJ$Rank, na.rm = TRUE)
    RJ$Of[is.na(RJ$VWC)] <- NA
    RA <- rbind(RA, RJ)
  }
  
  RA <- RA[order(RA$Date), c(1, 2, 4, 5)]
  RA <- RA[-which(is.na(RA$Date)), ]
  
  readr::write_csv(RA,
            sprintf(paste0(cosmos_dayrank_outfile ,"%s_daily.csv"), SiteName[i]))
  
}
