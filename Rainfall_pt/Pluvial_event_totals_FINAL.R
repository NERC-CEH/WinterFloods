### giaves 2023-10-25
# Find event totals for given events

#### SETUP ####
rm(list = ls())
Sys.setenv(tz = "UTC")

setwd("P:/08458 CWI-EA 2019-21 Flood Review")

library(zoo)
library(readxl)

#### KEY FILEPATHS ####
pluvial_rain_meta <- "Code/Pluvial/Raingauge_table.csv"
pluvial_rain_folder <- "Data/Pluvial"
pluvial_total_out <- "Data/Pluvial/Point_rainfall_totals.csv"
key_details_filename <- "Data/Master Station Listings UKCEH_post queries.xlsx"


#### READ IN DATA ####
RG_refs <- as.data.frame(readr::read_csv(pluvial_rain_meta))

RG_files <- list.files(path = pluvial_rain_folder, pattern = "\\.all", full = TRUE, recursive = TRUE)
RG_explog <- list.files(path = pluvial_rain_folder, pattern = "exp.log", full = TRUE, recursive = TRUE)

RG_series <- setdiff(RG_files, RG_explog)

Events <- readxl::read_xlsx(key_details_filename, sheet = "Pluvial analysis")
Events <- Events[ , 1:6] # Event 5 is always NA
colnames(Events)[3:6] <- paste0("E", 1:4)

RG_Ev <- merge(RG_refs, Events, all = TRUE)

NEv<- sum(!is.na(RG_Ev[ , 6:9]))

# Pre-allocation
EvDepths <- data.frame(Area = character(NEv), Location = character(NEv), Gauge.Name = character(NEv), ID = character(NEv),
                       Depth.1h = numeric(NEv), StartDate.1h = character(NEv),
                       Depth.6h = numeric(NEv), StartDate.6h = character(NEv),
                       Depth.1d = numeric(NEv), StartDate.1d = character(NEv),
                       Depth.4d = numeric(NEv), StartDate.4d = character(NEv),
                       Interval = numeric(NEv), GivenDate = character(NEv))
C <- 0
#### DATA PROCESSING ####
# For each station
for (i in 1:dim(RG_Ev)[1]) {
  
  RG_File <- RG_series[grep(RG_Ev$ID[i], RG_series)]
  
  # Skip if it doesn't exist
  if (length(RG_File) == 0) {
    print(sprintf("%s empty", i))
    next()
  }

  # Cases where two files are available (one 15-minute, one 60-minute) - only select 15-minute
  if (length(RG_File) > 1) {
    RG_File <- RG_File[grep("15", RG_File)]
  }

  # Load time series 
  TS <- read.csv(RG_File, skip = 15)
  colnames(TS)[1:2] <- c("DateTime", "Depth")
  TS$DateTime <- as.POSIXct(TS$DateTime, format = "%d/%m/%Y %H:%M:%S")
  
  for (j in 1:4) {
    
    # Event date
    Date <- RG_Ev[i, j+5]
    
    # Skip if NA (most locations don't have 4 events)
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
    
    EvDepths$Area[C] <- RG_Ev$Area[i]
    EvDepths$Location[C] <- RG_Ev$Location[i]
    EvDepths$Gauge.Name[C] <- RG_Ev$Gauge.Name[i]
    EvDepths$ID[C] <- RG_Ev$ID[i]

    # Diagnostic code to identify which rain gauges have 60-minute resolution (not 15)    
    if(dim(TS_range)[1] != 1248) if (dim(TS_range)[1] == 312) print(sprintf("%s is an hourly record", i))
    
    Interval<- as.numeric(RG_Ev$Interval[i])
    
    Acc1h <- 60 / Interval
    Acc6h <- 360 / Interval
    Acc1d <- 1440 / Interval
    Acc4d <- 5760 / Interval
    
    TS_range$Acc1h <- rollapply(TS_range$Depth, Acc1h, FUN = sum, align = "left", na.pad = TRUE, na.rm = TRUE)
    TS_range$Acc6h <- rollapply(TS_range$Depth, Acc6h, FUN = sum, align = "left", na.pad = TRUE, na.rm = TRUE)
    TS_range$Acc1d <- rollapply(TS_range$Depth, Acc1d, FUN = sum, align = "left", na.pad = TRUE, na.rm = TRUE)
    TS_range$Acc4d <- rollapply(TS_range$Depth, Acc4d, FUN = sum, align = "left", na.pad = TRUE, na.rm = TRUE)
    
    TS_range$Acc1h[(which(TS_range$DateTime < DateMin1h | TS_range$DateTime > DateMax1h))] <- NA
    TS_range$Acc6h[(which(TS_range$DateTime < DateMin6h | TS_range$DateTime > DateMax6h))] <- NA
    TS_range$Acc1d[(which(TS_range$DateTime < DateMin1d | TS_range$DateTime > DateMax1d))] <- NA
    
    EvDepths$Depth.1h[C] <- TS_range[which.max(TS_range$Acc1h), 3]
    EvDepths$StartDate.1h[C] <- as.character(TS_range[which.max(TS_range$Acc1h), 1])
    EvDepths$Depth.6h[C] <- TS_range[which.max(TS_range$Acc6h), 4]
    EvDepths$StartDate.6h[C] <- as.character(TS_range[which.max(TS_range$Acc6h), 1])
    EvDepths$Depth.1d[C] <- TS_range[which.max(TS_range$Acc1d), 5]
    EvDepths$StartDate.1d[C] <- as.character(TS_range[which.max(TS_range$Acc1d), 1])
    EvDepths$Depth.4d[C] <- TS_range[which.max(TS_range$Acc4d), 6]
    EvDepths$StartDate.4d[C] <- as.character(TS_range[which.max(TS_range$Acc4d), 1])
    EvDepths$Interval[C] <- Interval
    EvDepths$GivenDate[C] <- as.character(Date)
    
  }
  
}

EvDepths <- EvDepths[1:C, ]

EvDepths$StartDate.1h[which(nchar(EvDepths$StartDate.1h) == 10)] <- paste(EvDepths$StartDate.1h[which(nchar(EvDepths$StartDate.1h) == 10)] , "00:00:00")
EvDepths$StartDate.6h[which(nchar(EvDepths$StartDate.6h) == 10)] <- paste(EvDepths$StartDate.6h[which(nchar(EvDepths$StartDate.6h) == 10)] , "00:00:00")
EvDepths$StartDate.1d[which(nchar(EvDepths$StartDate.1d) == 10)] <- paste(EvDepths$StartDate.1d[which(nchar(EvDepths$StartDate.1d) == 10)] , "00:00:00")
EvDepths$StartDate.4d[which(nchar(EvDepths$StartDate.4d) == 10)] <- paste(EvDepths$StartDate.4d[which(nchar(EvDepths$StartDate.4d) == 10)] , "00:00:00")

#### SAVE OUTPUTS ####

write.csv(EvDepths, pluvial_total_out, row.names = FALSE)
