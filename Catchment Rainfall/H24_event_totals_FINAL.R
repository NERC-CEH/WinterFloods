### giaves 2023-10-25
# EA project 35752: Hydrological analysis of the 2019-2021 flooding

# Find event totals for given events using H24 rainfall data and raingauge data.
# Event rainfall totals extracted for 1h, 6h, 1d, 4d periods.

# Version 0.1: 2023-10-25. Initial development of code
# Version 0.2: 2023-11-01. Refactoring for wider distribution.
# Version 1.0: 2024-07-22. Final version for wider distribution.


#### SETUP ####
library(zoo)
library(readxl)

#### KEY FILEPATHS ####
pluvial_rain_meta <- "./Data/Catchment Rainfall Event Data/Raingauge_table.csv" #list of raingauges to commpare to
pluvial_rain_folder <- "Data/Catchment Rainfall Event Data/H24 rainfall data" # folder containing pluvial rainfall timeseries
pluvial_total_out <- "./Data/Catchment Rainfall Event Data/CatAvg_rainfall_totals.csv" #output filepath for rainfall totals
key_details_filename <- "./Data/Catchment Rainfall Event Data/Places_and_dates_rainfall.xlsx" #metadata for events and locations

# specific files for events in 2019, 2020 and 2021.
H24_19_filepath<- paste0(pluvial_rain_folder,
          "/20231111_0046_average_catavg_H24_historical_All_Locations_2019.csv")
H24_20_filepath <- paste0(pluvial_rain_folder,
          "/20231111_0046_average_catavg_H24_historical_All_Locations_2020.csv")
H24_21_filepath <- paste0(pluvial_rain_folder,
          "/20231111_0046_average_catavg_H24_historical_All_Locations_2021.csv")


#### READ IN DATA ####
RG_Ev <- readxl::read_xlsx(key_details_filename)
RG_Ev <- as.data.frame(RG_Ev)

H24_19 <- read.csv(H24_19_filepath, header = FALSE)
H24_20 <- read.csv(H24_20_filepath, header = FALSE)
H24_21 <- read.csv(H24_21_filepath, header = FALSE)

H24_all <- rbind(H24_19, H24_20, H24_21)
rm(H24_19, H24_20, H24_21) # remove intermediate objects


NEv<- sum(!is.na(RG_Ev[ , 7:12])) # Number of events

# Pre-allocation
EvDepths <- data.frame(Area = character(NEv), Location = character(NEv),
                       Gauge.Name = character(NEv), ID = character(NEv), NRFA = numeric(NEv),
                       Depth.1h = numeric(NEv), StartDate.1h = character(NEv),
                       Depth.6h = numeric(NEv), StartDate.6h = character(NEv),
                       Depth.1d = numeric(NEv), StartDate.1d = character(NEv),
                       Depth.4d = numeric(NEv), StartDate.4d = character(NEv),
                       Interval = numeric(NEv), GivenDate = character(NEv))

#### DATA PROCESSING ####
C <- 0 # count for final file
# For each station
for (i in 1:dim(RG_Ev)[1]) {
  
  print(i)
  
  if (is.na(RG_Ev$OBJECTID[i])) next()
  
  # Load time series 
  TS <- H24_all[H24_all$V1==RG_Ev$OBJECTID[i], 2:3]
  colnames(TS)[1:2] <- c("DateTime", "Depth")
  TS$DateTime <- as.POSIXct(TS$DateTime, format = "%d/%m/%Y %H:%M")
  TS <- TS[order(TS$DateTime), ]
  
  for (j in 1:6) {
    
    # Event date
    Date <- RG_Ev[i, j+6]
    
    # Skip if NA (most locations don't have 6 events)
    if (is.na(Date)) next()
    
    # Subset full time-series into date +/- 4 days (4-day total needed) +/- 2 days (accuracy of date) 
    DateMin <- Date - 6*86400
    DateMax <- Date + 7*86400
    TS_range <- TS[which(TS$DateTime >= DateMin & TS$DateTime < DateMax), 1:2]
    
    DateMin1h <- Date - 2*86400 - 2700
    DateMax1h <- Date + 3*86400 + 2700
    DateMin6h <- Date - 2*86400 - 20700
    DateMax6h <- Date + 3*86400 + 20700
    DateMin1d <- Date - 2*86400 - 85500
    DateMax1d <- Date + 3*86400 + 85500
    
    # Skip if no data for this time period
    if (dim(TS_range)[1] == 0) next()
    
    # Skip if no rainfall data for this time period
    TS_range$Depth <- as.numeric(TS_range$Depth)
    if (sum(!is.na(TS_range$Depth)) == 0) next()
    
    C <- C+1
    
    # EvDepths$Area[C] <- RG_Ev$Area[i]
    EvDepths$Location[C] <- RG_Ev$River[i]
    EvDepths$Gauge.Name[C] <- RG_Ev$Gauge[i]
    EvDepths$ID[C] <- RG_Ev$`EA Gauge ID`[i]
    EvDepths$NRFA[C] <- RG_Ev$`NRFA ID`[i]

    # Diagnostic code to identify which rain gauges have 60-minute resolution (not 15)    
    if(dim(TS_range)[1] != 1248) if (dim(TS_range)[1] == 312) print(sprintf("%s is an hourly record", i))
    
    Interval<- 15
    
    # Accumulation period length
    Acc1h <- 60 / Interval
    Acc6h <- 360 / Interval
    Acc1d <- 1440 / Interval
    Acc4d <- 5760 / Interval
    
    # Calculate accumulations for 4 periods
    TS_range$Acc1h <- rollapply(
      TS_range$Depth, Acc1h, FUN = sum, align = "left", na.pad = TRUE, na.rm = TRUE)
    TS_range$Acc6h <- rollapply(
      TS_range$Depth, Acc6h, FUN = sum, align = "left", na.pad = TRUE, na.rm = TRUE)
    TS_range$Acc1d <- rollapply(
      TS_range$Depth, Acc1d, FUN = sum, align = "left", na.pad = TRUE, na.rm = TRUE)
    TS_range$Acc4d <- rollapply(
      TS_range$Depth, Acc4d, FUN = sum, align = "left", na.pad = TRUE, na.rm = TRUE)
    
    # Accounts for dates outside the timeseries ranges
    TS_range$Acc1h[(which(TS_range$DateTime < DateMin1h | TS_range$DateTime > DateMax1h))] <- NA
    TS_range$Acc6h[(which(TS_range$DateTime < DateMin6h | TS_range$DateTime > DateMax6h))] <- NA
    TS_range$Acc1d[(which(TS_range$DateTime < DateMin1d | TS_range$DateTime > DateMax1d))] <- NA
    
    # Finds peak rainfall depths within event time range.
    EvDepths$Depth.1h[C] <- TS_range[which.max(TS_range$Acc1h), 3]
    EvDepths$StartDate.1h[C] <- as.character(TS_range[which.max(TS_range$Acc1h), 1])
    
    EvDepths$Depth.6h[C] <- TS_range[which.max(TS_range$Acc6h), 4]
    EvDepths$StartDate.6h[C] <- as.character(TS_range[which.max(TS_range$Acc6h), 1])
    
    EvDepths$Depth.1d[C] <- TS_range[which.max(TS_range$Acc1d), 5]
    EvDepths$StartDate.1d[C] <- as.character(TS_range[which.max(TS_range$Acc1d), 1])
    
    EvDepths$Depth.4d[C] <- TS_range[which.max(TS_range$Acc4d), 6]
    EvDepths$StartDate.4d[C] <- as.character(TS_range[which.max(TS_range$Acc4d), 1])
    
    # Add metadata
    EvDepths$Interval[C] <- Interval
    EvDepths$GivenDate[C] <- as.character(as.POSIXct(as.numeric(Date), tz="UTC"))
    
  }
  
}

# Convert dates to datetimes
EvDepths <- EvDepths[1:C, ]

# If no datetime, assumes event happened at midnight.
EvDepths$StartDate.1h[which(nchar(EvDepths$StartDate.1h) == 10)] <- 
  paste(EvDepths$StartDate.1h[which(nchar(EvDepths$StartDate.1h) == 10)] , "00:00:00")
EvDepths$StartDate.6h[which(nchar(EvDepths$StartDate.6h) == 10)] <- 
  paste(EvDepths$StartDate.6h[which(nchar(EvDepths$StartDate.6h) == 10)] , "00:00:00")
EvDepths$StartDate.1d[which(nchar(EvDepths$StartDate.1d) == 10)] <- 
  paste(EvDepths$StartDate.1d[which(nchar(EvDepths$StartDate.1d) == 10)] , "00:00:00")
EvDepths$StartDate.4d[which(nchar(EvDepths$StartDate.4d) == 10)] <- 
  paste(EvDepths$StartDate.4d[which(nchar(EvDepths$StartDate.4d) == 10)] , "00:00:00")



#### SAVE OUTPUTS ####
write.csv(EvDepths, pluvial_total_out, row.names = FALSE)
