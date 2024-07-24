### giaves 2023-10-12
# EA project 35752: Hydrological analysis of the 2019-2021 flooding

# Main contributor: Gianni Vesuviano, UKCEH
# Find single highest COSMOS volumetric water content for each site,
# based on daily data and month-quarter collation.

# Version 0.1: 2024-01-15. Initial development of code
# Version 1.0: 2024-07-22. Final version for wider distribution.

#### SETUP ####

#### KEY FILEPATHS ####
cosmos_daily_folder <- "Data/COSMOS/daily"
cosmos_outfile <- "Data/COSMOS-UK_data/Max_VWC_per_site.csv"

#### READ IN DATA ####
CF <- list.files(cosmos_daily_folder, full = TRUE)
N <- length(CF)

# Pre-allocation
SiteName <- rep("", N)
POR <- rep("", N)
nyears <- rep(0, N)
DateVWC <- rep("", N)
MaxVWC <- rep(0, N)


##### ANALYSIS ####
for (i in 1:N) {
  
  # Get name from header of datafile
  SiteName[i] <- strsplit(readLines(CF[i], n = 3)[3], ",")[[1]][2]
  # get data from rest of .csv file
  Rec <- read.csv(CF[i], skip = 5)[ , 1:2]
  colnames(Rec) <- c("Date", "VWC")
  Rec$Date <- as.Date(Rec$Date)
  Rec <- na.omit(Rec)
  # for each year
  Yrs <- unique(substr(Rec$Date, 1, 4))
  
  POR[i] <- paste(min(Yrs), max(Yrs), sep = "-") # period of record
  
  nyears[i] <- length(unique(substr(Rec$Date, 1, 4))) #length of record
  
  MV <- Rec[which.max(Rec$VWC), ] # year with max wvc
  
  DateVWC[i] <- as.character(as.Date(unlist(MV)[1])) #date of max VWC
  
  MaxVWC[i] <- unlist(MV)[2] # value of max VWC
  
}



##### SAVE OUTPUTS #####
OUT <- data.frame(Site = SiteName,
                  POR = POR,
                  nyears = nyears,
                  Date = as.character(DateVWC),
                  VWC = MaxVWC)

write.csv(OUT, cosmos_outfile, row.names = FALSE, quote = FALSE)
