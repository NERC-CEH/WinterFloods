# 08458: Winter Floods 2019-21
# Main Contributor: Gianni Vesuviano
# Find closest COSMOS site to each river gauging station and export table

# Version 1.0:  2023-11-08


##### SETUP #####
library("readxl")
library("rnrfa")

# Load COSMOS sites and Winter Floods sites
COSMOS <- read.csv("Data/COSMOS-UK_data/SITE_INFO.csv")
Master <- readxl::read_excel("Data/Master Station Listings UKCEH_post queries.xlsx", sheet = "PostQueries_FluvialGauged")
Master <- Master[ , c(1, 3, 4, 7, 5, 2)]
colnames(Master) <- c("NRFA", "Gauge", "River", "Type", "ID", "Area")

# Add easting and northing from NRFA dataset
Cat <- rnrfa::catalogue()
Cat <- Cat[ , c("id", "easting", "northing")]
colnames(Cat) <- c("NRFA", "Easting", "Northing")

Master <- merge(Master, Cat, all.x = TRUE)

# Get many filenames
Q15 <- list.files(path = "Data/Q 15", full = TRUE)
SG15 <- list.files(path = "Data/Sg 15", full = TRUE)
QSG <- c(Q15, SG15)

# Fill in easting and northing for non-NRFA sites
for (i in 1:dim(Master)[1]) {
  
  if (!is.na(Master$Easting[i])) next()
  
  File <- which(grepl(Master$ID[i], QSG) == 1)[1]
  
  if (is.na(File)) next()
  
  EN <- osg_parse(unlist(read.csv(QSG[File], skip = 5, nrow = 1, header = FALSE))[2])
  
  Master$Easting[i] <- EN$easting[1]
  Master$Northing[i] <- EN$northing[1]
  
}

# Fill in easting and northing for more West Mids sites
for (i in 1:dim(Master)[1]) {
  
  if (!is.na(Master$Easting[i])) next()

  G <- Master$Gauge[i]  
  G <- gsub("Rocester Weir", "Rocester", G)
  G <- gsub(" on ", " On ", G)
  G <- gsub("-on-", "-On-", G)
  G <- gsub(" ", "", G)
  G <- gsub("-", "", G)
  
  File <- which(grepl(G, GG) == 1)[1]
  
  if (is.na(File)) next()
  
  EN <- read.table(GG[File], skip = 11, nrow = 2, header = FALSE)
  
  Master$Easting[i] <- EN$V2[1]
  Master$Northing[i] <- EN$V2[2]
  
}

# Manual inserts
SB <- which(Master$Gauge == "South Bridge")
Master$Easting[SB] <- 475420
Master$Northing[SB] <- 259740

IB <- which(Master$Gauge == "IVER BRIDGE")
Master$Easting[IB] <- 504107
Master$Northing[IB] <- 181265



# Pre-allocation of columns
Master$Closest_COSMOS_ID <- NA
Master$Closest_COSMOS_name <- NA
Master$Closest_COSMOS_distance_km <- NA


COSMOS$Closest_Station <- NA
COSMOS$Closest_Station_Name <- NA
COSMOS$Closest_COSMOS_distance_km <- NA
COSMOS$Region <- NA

# Processing
for (i in 1:dim(Master)[1]) {
  
  if (is.na(Master$Easting[i])) next()
  
  # get Euclidean distance
  distance <- sqrt((Master$Easting[i] - COSMOS$EASTING)^2 + (Master$Northing[i] - COSMOS$NORTHING)^2) / 1000
  C <- which.min(distance)
  
  Master$Closest_COSMOS_ID[i] <- COSMOS$SITE_ID[C]
  Master$Closest_COSMOS_name[i] <- COSMOS$SITE_NAME[C]
  Master$Closest_COSMOS_distance_km[i] <- distance[C]
  
}

for (i in 1:dim(COSMOS)[1]) {
  
  if (is.na(Master$Easting[i])) next()
  # get Euclidean distance
  distance <- sqrt((Master$Easting - COSMOS$EASTING[i])^2 + (Master$Northing - COSMOS$NORTHING[i])^2) / 1000
  C <- which.min(distance)
  COSMOS$Closest_Station[i] <- Master$ID[C]
  COSMOS$Closest_Station_Name[i] <- Master$Gauge[C]
  COSMOS$Region[i] <- Master$Area[C]
  COSMOS$Closest_COSMOS_distance_km[i] <- distance[C]
  
}

#### WRITE OUT DATA ####

readr::write_csv(COSMOS, "./Data/COSMOS/Closest_Station_to_each_COSMOS.csv")

