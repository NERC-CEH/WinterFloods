# giaves, 2023-11-01
# EA project 35752: Hydrological analysis of the 2019-2021 flooding

# Main Contributor: Gianni Vesuviano
# Find closest COSMOS site to each river gauging station and export table

# Version 0.1: 2023-11-08 Initial development of code
# Version 1.0: 2024-07-22 Final version for wider distribution


##### SETUP #####
library(readxl)
library(rnrfa)


COSMOS_meta_filename <- "SITE_INFO.csv" # list of COSMOS sites with key metadata
master_metadata_filename <- "Master Station Listings UKCEH_post queries.xlsx" # All locations, event dates, and key analyses to undertake
flow_folder <- "/Q15" # folder with all 15-minute flow timeseries
stage_folder <- "/S15" # folder with all 15-minute stage timeseries

closest_station_out_filename <- "Closest_Station_to_each_COSMOS.csv" # files to save closest sites to
closest_cosmos_out_filename <- "Closest_COSMOS_to_each_station.csv"

##### READ IN DATA #####
# Load COSMOS sites and Winter Floods sites
COSMOS <- read.csv(COSMOS_meta_filename)
Master <- readxl::read_excel(master_metadata_filename,
                             sheet = "PostQueries_FluvialGauged")
Master <- Master[ , c(1, 3, 4, 7, 5, 2)]
colnames(Master) <- c("NRFA", "Gauge", "River", "Type", "ID", "Area")

# Add easting and northing from NRFA dataset
Cat <- rnrfa::catalogue()
Cat <- Cat[ , c("id", "easting", "northing")]
colnames(Cat) <- c("NRFA", "Easting", "Northing")

Master <- merge(Master, Cat, all.x = TRUE)



##### GET METADATA #####
# Get many filenames
Q15 <- list.files(path = flow_folder, full = TRUE)
SG15 <- list.files(path = stage_folder, full = TRUE)
QSG <- c(Q15, SG15)

# Fill in easting and northing for non-NRFA sites
for (i in 1:dim(Master)[1]) {
  
  if (!is.na(Master$Easting[i])) next() # If location unknown, skip
  
  File <- which(grepl(Master$ID[i], QSG) == 1)[1] #find corresponding stage or flow file
  
  if (is.na(File)) next() # if file not found, skip
  
  # Calculate GB easting and northing using header of file
  EN <- osg_parse(unlist(read.csv(QSG[File], skip = 5, nrow = 1, header = FALSE))[2])
  
  Master$Easting[i] <- EN$easting[1]
  Master$Northing[i] <- EN$northing[1]
  
}

# Manual inserts
# Fill in easting and northing for more West Mids sites
for (i in 1:dim(Master)[1]) {
  
  if (!is.na(Master$Easting[i])) next()  # only runs for sites for which E/N not found yet.

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




##### COMPILING OUTPUTS #####

# Pre-allocation of columns
Master$Closest_COSMOS_ID <- NA
Master$Closest_COSMOS_name <- NA
Master$Closest_COSMOS_distance_km <- NA


COSMOS$Closest_Station <- NA
COSMOS$Closest_Station_Name <- NA
COSMOS$Closest_COSMOS_distance_km <- NA
COSMOS$Region <- NA

# Processing closest COSMOS-UK to each flow/stage station
for (i in 1:dim(Master)[1]) {
  
  if (is.na(Master$Easting[i])) next()
  
  # get Euclidean distance
  distance <- sqrt((Master$Easting[i] - COSMOS$EASTING)^2 +
                     (Master$Northing[i] - COSMOS$NORTHING)^2) / 1000
  # find closest COSMOS_UK station
  C <- which.min(distance)
  
  # save relevant metadata
  Master$Closest_COSMOS_ID[i] <- COSMOS$SITE_ID[C]
  Master$Closest_COSMOS_name[i] <- COSMOS$SITE_NAME[C]
  Master$Closest_COSMOS_distance_km[i] <- distance[C]
  
}


# Processing closest stage/flow station for each COSMOS-UK site
for (i in 1:dim(COSMOS)[1]) {
  
  if (is.na(Master$Easting[i])) next()
  # get Euclidean distance
  distance <- sqrt((Master$Easting - COSMOS$EASTING[i])^2 +
                     (Master$Northing - COSMOS$NORTHING[i])^2) / 1000
  # find closest station
  C <- which.min(distance)
  # save relevant metadata
  COSMOS$Closest_Station[i] <- Master$ID[C]
  COSMOS$Closest_Station_Name[i] <- Master$Gauge[C]
  COSMOS$Region[i] <- Master$Area[C]
  COSMOS$Closest_COSMOS_distance_km[i] <- distance[C]
  
}





#### WRITE OUT DATA ####
readr::write_csv(COSMOS, closest_station_out_filename)
readr::write_csv(Master, closest_cosmos_out_filename)
