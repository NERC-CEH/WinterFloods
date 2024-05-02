### giaves 2023-10-12
# Find single highest COSMOS volumetric water content for each site


#### SETUP ####
rm(list = ls())
setwd("P:/08458 CWI-EA 2019-21 Flood Review")

#### KEY FILEPATHS ####
cosmos_daily_folder <- "Data/COSMOS-UK_data/daily"
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

for (i in 1:N) {
  
  SiteName[i] <- strsplit(readLines(CF[i], n = 3)[3], ",")[[1]][2]
  Rec <- read.csv(CF[i], skip = 5)[ , 1:2]
  colnames(Rec) <- c("Date", "VWC")
  Rec$Date <- as.Date(Rec$Date)
  Rec <- na.omit(Rec)
  # for each year
  Yrs <- unique(substr(Rec$Date, 1, 4))
  
  POR[i] <- paste(min(Yrs), max(Yrs), sep = "-")
  
  nyears[i] <- length(unique(substr(Rec$Date, 1, 4)))
  
  MV <- Rec[which.max(Rec$VWC), ] # year with max wvc
  
  DateVWC[i] <- as.character(as.Date(unlist(MV)[1]))
  
  MaxVWC[i] <- unlist(MV)[2]
  
}

OUT <- data.frame(Site = SiteName,
                  POR = POR,
                  nyears = nyears,
                  Date = as.character(DateVWC),
                  VWC = MaxVWC)

write.csv(OUT, cosmos_outfile, row.names = FALSE, quote = FALSE)
