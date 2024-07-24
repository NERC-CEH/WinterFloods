# Adam Griffin 2023-09-21
# EA project 35752: Hydrological analysis of the 2019-2021 flooding

# Get all AMAX and bundle into a single file for extra analysis
# Add metadata

# Version 0.1: 2023-09-21. Initial development of code
# Version 0.2: 2023-11-01. Refactoring for wider distribution.
# Version 1.0: 2024-07-22. Final version for wider distribution.

##### SETUP #####
library(readxl)
library(readr)

##### Key arguments #####
key_details_filepath <- "Data/Metadata/Master Station Listings.xlsx" 
# key metadata, one station per row
all_amax_filepath <- "Data/Flow/All_AM_valid_with_dates.txt" 
# all valid AMAX, one value per row, all stations concatenated into long table
all_amax2_filepath <-"Data/Flow/AMAXs_QWYearAMAX_vs_Q15.csv" 
# comparison of given AMAX vs estimates using Q15 data

all_amax_ranks_out <-"./Data/all_amax_with_ranks.csv" # output filepath for ranked AMAX.


##### DATA #####
Master <- readxl::read_excel(key_details_filepath,
                             sheet = "PostQueries_FluvialGauged")
Master <- Master[ , c(2, 1, 3, 4, 7, 5, 11:16)]
colnames(Master)[2] <- "NRFA"
colnames(Master)[6] <- "ID"
colnames(Master)[7:12] <- paste0("E", 1:6)
Master$NRFA <- as.numeric(Master$NRFA)

## This is only needed if some station IDs are inconsistent
Master <- as.data.frame(Master)
# pad some station numbers with leading zeroes
N2 <- which(nchar(Master$ID) == 5 & substr(Master$ID, 1, 1) == "2")
N3 <- which(nchar(Master$ID) == 5 & substr(Master$ID, 1, 1) == "3")
Master$ID[c(N2, N3)] <- paste0("0", Master$ID[c(N2, N3)])
M2 <- Master[ , c("NRFA", "ID")]

##### Read In AMAX Data
AllAMAX <- read.csv(all_amax_filepath, header = TRUE)[ , c(1,2,4)]
colnames(AllAMAX)[1] <-"NRFA"
AllAMAX$DateTime <- as.Date(AllAMAX$DateTime)

AllAMAX2 <- read.csv(all_amax2_filepath)[ , 1:3]
colnames(AllAMAX2) <- c("ID", "DateTime", "AMAX")
# Strips suffixes
# This is only needed if some station IDs are inconsistent
for(i in 1:dim(AllAMAX2)[1]) AllAMAX2$ID[i] <- stringr::str_split_i(AllAMAX2$ID[i], ".FQ", i=1)
for(i in 1:dim(AllAMAX2)[1]) AllAMAX2$ID[i] <- stringr::str_split_i(AllAMAX2$ID[i], ".FL", i=1)
AllAMAX2 <- merge(M2, AllAMAX2)
AllAMAX2 <- AllAMAX2[which(!(AllAMAX2$NRFA %in% AllAMAX$NRFA)), 2:4]
AllAMAX <- rbind(AllAMAX, AllAMAX2)


##### Add metadata #####
AllAMAX$ID <- NA
AllAMAX$Area <- NA
for(i in 1:nrow(AllAMAX)){
  s <- AllAMAX$NRFA[i]
  s2 <- as.character(s)
  w <- which(key_details$`NRFA ID` == s | key_details$`NRFA ID` == s2)
  if(length(w)>0){
    AllAMAX$ID[i] <- key_details$`Gauge ID`[w[1]]
    AllAMAX$Area[i] <-  key_details$Area[w[1]]
  }
}

# Rerank data
AllAMAX$rank <- NA
AllAMAX$AMAX <- as.numeric(AllAMAX$AMAX)
U <- unique(AllAMAX$NRFA)
for(u in U){
  w <- which(AllAMAX$NRFA == u)
  ranking <- rank(-1*AllAMAX$AMAX[w], ties="min") 
  # rank goes lowest to highest. negative reverses this quickly
  AllAMAX$rank[w] <- ranking
}

##### Save to file #####
readr::write_csv(AllAMAX, all_amax_ranks_out)
