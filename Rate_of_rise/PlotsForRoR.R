#### Griffin, Adam. 2023-12-01
# EA project 35752: Hydrological analysis of the 2019-2021 flooding

# Main contributor: Adam Griffin
# Info: Rate of rise plots to describe key event level rates of rise of over time.

# Version 0.1: 2023-12-01. Initial development of code
# Version 0.2: 2023-12-08. Refactoring for wider distribution.
# Version 1.0: 2024-07-22. Final version for wider distribution.


#### SETUP ####
library(tidyverse) # contains dplyr, readr, tidyr, magrittr
library(sf)
library(ggplot2)

ror_events_stage <- "" # file containing event level rates of rise with ranks and trends.

gridrefs_filename <-"./Data/Locations/AllShapefiles_Grid References.csv" # grid references of key locations
  
metadata_filename <-".Data/Master Station Listings.csv" # key metadata, one station per row
  
uk_outline_filename <- "/Data/Shapefiles/uk_outline_1000m.shp" # outline of UK.


### READ IN DATA ##

AllXY <- read_csv(gridrefs_filename ) 
AllNames <- read_csv(metadata_filename) 
hyd_area <- read_sf(uk_outline_filename) 

EventStage_table <- readr::read_csv(ror_events_stage)

# Align grid references
AllNames$X <- NA
AllNames$Y <- NA

# get correct easting and northing
for(i in 1:nrow(AllNames)){
  w <- which(AllXY$Gauge == AllNames$Gauge[i] | AllXY$Location == AllNames$Gauge[i])
  if(length(w) == 1){
  AllNames$X[i] <- AllXY$X[w]
  AllNames$Y[i] <- AllXY$Y[w]
  }else{
    print(i)
    print(AllNames$Gauge[i])
    print(w)
    next
  }
}

# subset by correct stations
AllNames <- AllNames[AllNames$`Gauge ID` %in% EventStage_table$Station,]
ES <- EventStage_table %>% 
  group_by(Station) %>%
  summarise(maxrank=min(rank1), trend=min(trend))


# Get ROR trends from data
AllNames$maxrank <- NA
AllNames$ror_trend <- NA

for(i in 1:nrow(AllNames)){
  j <- which(ES$Station == AllNames$`Gauge ID`[i])
  AllNames$maxrank[i] <- ES$maxrank[j]
  AllNames$ror_trend[i] <- ES$trend[j]
}
# highlight significant trends
AllNames$ror_signif <- 1*(AllNames$ror_trend < 0.05)
AllNames <- st_as_sf(AllNames,coords = 28:29)


#### Plotting ###
png("./Plots/maxrank.png", width=80, height=80, units="mm",
    pointsize=10, res=300)
ggplot(hyd_area) +
  geom_sf() +
  geom_sf(data=AllNames, aes(colour=maxrank)) +
  scale_colour_gradient(high="blue", low="red", limits=c(0,20)) + 
  theme_bw() +
  lims(x=c(1.5e5,6.5e5), y=c(0,6.5e5))
dev.off()

png("./Plots/trendMK.png", width=80, height=80, units="mm",
    pointsize=10, res=300)
ggplot(hyd_area) +
  geom_sf() +
  geom_sf(data=AllNames, aes(colour=ror_trend)) +
  scale_colour_gradient(low="blue", high="red") + 
  theme_bw() +
  lims(x=c(1.5e5,6.5e5), y=c(0,6.5e5))
dev.off()

png("./Plots/sigtrend1hr.png", width=80, height=80, units="mm",
    pointsize=10, res=300)
ggplot(hyd_area) +
  geom_sf() +
  geom_sf(data=AllNames, aes(colour=factor(ror_signif))) +
  labs(colour="Signif trend?")  +
  theme_bw() +
  lims(x=c(1.5e5,6.5e5), y=c(0,6.5e5))
dev.off()

