library(tidyverse)

AllXY <- read_csv("P:/08458 CWI-EA 2019-21 Flood Review/Data/Locations/AllShapefiles_Grid References.csv")
AllNames <- read_csv("P:/08458 CWI-EA 2019-21 Flood Review/Data/MSL_post queries.csv", locale=locale(encoding="latin1"))

AllNames$X <- NA
AllNames$Y <- NA

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


library(sf)
library(ggplot2)
hyd_area <- read_sf("P:/08458 CWI-EA 2019-21 Flood Review/Data/Locations/uk_outline_1000m.shp")

AllNames <- AllNames[AllNames$`Gauge ID` %in% EventStage_table$Station,]
ES <- EventStage_table %>% group_by(Station) %>% summarise(maxrank=min(rank1), trend=min(trend))



AllNames$maxrank <- NA
AllNames$ror_trend <- NA


for(i in 1:nrow(AllNames)){
  j <- which(ES$Station == AllNames$`Gauge ID`[i])
  AllNames$maxrank[i] <- ES$maxrank[j]
  AllNames$ror_trend[i] <- ES$trend[j]
}
AllNames$ror_signif <- 1*(AllNames$ror_trend < 0.05)
AllNames <- st_as_sf(AllNames,coords = 28:29)

png("./Plots/makrank.png", width=80, height=80, units="mm", pointsize=10, res=300)
ggplot(hyd_area) +
  geom_sf() +
  geom_sf(data=AllNames, aes(colour=maxrank)) +
  scale_colour_gradient(high="blue", low="red", limits=c(0,20)) + 
  theme_bw() +
  lims(x=c(1.5e5,6.5e5), y=c(0,6.5e5))
dev.off()

png("./Plots/trendMK.png", width=80, height=80, units="mm", pointsize=10, res=300)
ggplot(hyd_area) +
  geom_sf() +
  geom_sf(data=AllNames, aes(colour=ror_trend)) +
  scale_colour_gradient(low="blue", high="red") + 
  theme_bw() +
  lims(x=c(1.5e5,6.5e5), y=c(0,6.5e5))
dev.off()

png("./Plots/sigtrend1hr.png", width=80, height=80, units="mm", pointsize=10, res=300)
ggplot(hyd_area) +
  geom_sf() +
  geom_sf(data=AllNames, aes(colour=factor(ror_signif))) +
  labs(colour="Signif trend?")  +
  theme_bw() +
  lims(x=c(1.5e5,6.5e5), y=c(0,6.5e5))
dev.off()

