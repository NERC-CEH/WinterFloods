# Land cover to catchments

#### Griffin, Adam. 2024-01-01
# 08458: Winter Floods 2019-21

# Main contributor: Adam Griffin
# Info: Ranking volumes within volume AMAX series

# Version 0.1: 2024-01-01. Initial development of code

setwd("P:/08458 CWI-EA 2019-21 Flood Review")

##### SETUP
library(dplyr)
library(lubridate)
library(readr)
library(tidyverse)
library(readxl)
library(sf)
library(Kendall)
library(RODBC)

#for each duration (30-180)
#for each station
#for each event
#get rainfall for preceding N days or nearest already known N days
#get rank for it.


##### KEY VALUES
key_details_filename <- "./Data/Master Station Listings UKCEH_post queries.xlsx"
key_storms_filename <- "./Data/Context/Key Storms.csv"
id_column_kd <- "Gauge ID"
plot_folder <- ""

to_urban_filename <- "./Data/LandCoverMaps/ChangeToUrban_WF.shp"
from_urban_filename <- "./Data/LandCoverMaps/ChangeFromUrban_WF.shp"
lcm_urban_filename <- "./Data/LandCoverMaps/Urban_in_21_WF.shp"

counts_filename <- "./Data/LandCoverMaps/Simple_just_use_count.shp"
to_filename <- "./Data/LandCoverMaps/Simple_TO.shp"
simple_1990_filename <- "./Data/LandCoverMaps/Simple1990_classes.shp"
simple_2015_filename <- "./Data/LandCoverMaps/SImple2015_classes.shp"
all_2021_filename <- "./Data/LandCoverMaps/allclasses_2021.shp"

uk_outline_filename <- "./Data/Locations/EW_HYD_AREAS.shp"

#### EXTRA FUNCTIONS ####
get_water_year <- \(x){
  y <- lfstat::water_year(x, origin=10)
  as.numeric(levels(y)[y])
}

mk_if_long <- \(x,y,minrec=20){
  if(length(x)>=minrec){
    Z <- data.frame(Kendall(x,y)[])
  }else{
    Z <- data.frame(tau=NA, sl=NA, S=NA, D=NA, varS=NA)
  }
  Z$MKZ <- ifelse(abs(Z$S) < 1e-10, 0, (Z$S - sign(Z$S))/sqrt(Z$varS))
  return(Z)
}

ts_if_long <- \(x,y,minrec=20){
 if(length(x)>=minrec){
   df <- data.frame(Year=x, Flow=y)
   Z <- mblm::mblm(Flow~Year, df)
   return(Z$coefficients["Year"])
 }else{
   return(NA)
 }
  return(NA)
}

f <- \(x){as.numeric(stringr::str_extract(x, "[0-9]+"))}


# Metadata
key_details0 <- readxl::read_xlsx(key_details_filename, sheet=5)[,1:22]
key_details <- readxl::read_xlsx(key_details_filename, sheet=5)[,1:22] %>% 
  dplyr::filter(!is.na(`NRFA ID`)) %>%
  dplyr::select(`NRFA ID`, `Gauge ID`, `Event 1`:`Event 6`) %>%
  tidyr::pivot_longer(cols=`Event 1`:`Event 6`,
                      names_to="Event Number",
                      names_transform=f,
                      values_to="Date") %>%
  dplyr::filter(!is.na(Date))

station_list <- as.numeric(unique(key_details0$`NRFA ID`))
station_list <- station_list[!is.na(station_list)]
uk_outline <- sf::read_sf(uk_outline_filename)
st_crs(uk_outline) <- "EPSG:27700"


#### AMAX DATA ####
CH <- odbcConnect(dsn = "wla", uid = "swa2", pwd = "swa2")
WF2P <- sqlQuery(CH,
                 "select * from NRFA.DATALOAD_AMAX_FLOW where BATCH_ID = 'WINTER_FLOODS_2'",
                 believeNRows = FALSE)
WF3P <- sqlQuery(CH,
                 "select * from NRFA.DATALOAD_AMAX_FLOW where BATCH_ID = 'WINTER_FLOODS_3'",
                 believeNRows = FALSE)
WF3P <- WF3P[!(WF3P$STATION %in% WF2P$STATION),]
WF <- rbind(WF2P, WF3P) %>%
  mutate(Year=get_water_year(DATE_TIME)) %>%
  select(STATION, Year, DATE_TIME, FLOW)
colnames(WF) <- c("Station","Year", "Date", "Flow")

PF_AMAX0 <- read_csv("C:/Users/adagri/Documents/NRFA_PF_V12/amax.csv")
PF_AMAX1 <- PF_AMAX0 %>% filter(item %in% c("date", "flow")) %>%
  pivot_longer(cols=`2021`:`1851`, names_to="Year",
               values_to="flow",
               values_transform=as.character,
               values_drop_na=TRUE) %>%
  pivot_wider(names_from="item", values_from="flow")
PF_AMAX1$Year <- as.numeric(PF_AMAX1$Year)
PF_AMAX1$date <- lubridate::ymd(PF_AMAX1$date)
PF_AMAX1$flow <- as.numeric(PF_AMAX1$flow)
PF_AMAX1 <- PF_AMAX1 %>% select(id, Year, date, flow)
colnames(PF_AMAX1) <- c("Station", "Year", "Date", "Flow")

PF_all <- rbind(PF_AMAX1, WF[!(WF$Station %in% PF_AMAX1$Station),])

PF_MK <- PF_all %>%
  group_by(Station) %>%
  summarise(reclen=n(), MK=mk_if_long(Year, Flow), TS=ts_if_long(Year, Flow)) %>%
  unpack(MK)
colnames(PF_MK) <- c("Station", "RecLen", "MK_tau", "MK_pval", "MK_S", "MK_D", "MK_varS", "MKZ", "TSE")

PF_MK$Gauge_ID <- NA
key_details0$`NRFA ID` <- as.numeric(key_details0$`NRFA ID`)

PF_MK <- PF_MK %>% filter(Station %in% station_list)

for(i in seq_len(nrow(PF_MK))){
  k <- which(as.numeric(key_details0$`NRFA ID`)==as.numeric(PF_MK$Station[i]))
  if(length(k)>0){
    if(length(k)>1){
      print(paste(k, key_details0$`Gauge ID`[k[1]]))
    }
    PF_MK$Gauge_ID[i] <- key_details0$`Gauge ID`[k[1]]
  }
}


#### READ IN DATA ####

catchments_to_urban <- read_sf(to_urban_filename)
catchments_from_urban <- read_sf(from_urban_filename)
urban_coverage <- read_sf(lcm_urban_filename)

all_catchments_TO <- read_sf(to_filename)
all_catchments_2015 <- read_sf(simple_2015_filename)
all_catchments_2021 <- read_sf(all_2021_filename)


catchments_to_urban <- catchments_to_urban %>% arrange(Gauge_ID)
catchments_from_urban <- catchments_from_urban %>% arrange(Gauge_ID)
urban_coverage <- urban_coverage %>% arrange(Gauge_ID)

all_catchments_TO <- all_catchments_TO %>% arrange(Gauge_ID)
all_catchments_2015 <- all_catchments_2015 %>% arrange(Gauge_ID)
all_catchments_2021 <- all_catchments_2021 %>% arrange(Gauge_ID)


#### DATA FORMATTING ####
# match all LCM classes to Simple Classes
simple_2021 <- all_catchments_2021 %>%
  mutate(simple1 = HISTO_1 + HISTO_2,
         simple2 = HISTO_3,
         simple3 = HISTO_4 + HISTO_5 + HISTO_6 + HISTO_7 + HISTO_8 + HISTO_9 +
           HISTO_10 + HISTO_11,
         simple4 = HISTO_14,
         simple5 = HISTO_20 + HISTO_21,
         simple6 = HISTO_12) %>%
  select(Gauge_ID, NRFA_ID, Area, Gauge, River, Easting, Northing, source, simple1:simple6)

#### extra functions
f <- \(x){
  # Map from all classes to simple classes
  L <- list(c(1,2),3,c(4,5,6,7,8,9,10,11,16,18,19), 14, c(20,21), c(12,13,15,17))
  w <- which(sapply(L, \(l){x %in% l}))
  w
}

skip_duplicate <- \(df){
  n <- nrow(df)
  u <- which(df$Gauge_ID[-1] == df$Gauge_ID[-n])
  df[-u,]
}

catchments_to_urban <- skip_duplicate(catchments_to_urban)
catchments_from_urban <- skip_duplicate(catchments_from_urban)
urban_coverage <- skip_duplicate(urban_coverage)
simple_2021 <- skip_duplicate(simple_2021)

#Merge catchments to urbanisation
simple_2021b <- left_join(simple_2021,
                          as.data.frame(urban_coverage[,c("Gauge_ID", "_count")]),
                          by="Gauge_ID")

all_catchments_TOb <- left_join(all_catchments_TO,
                                as.data.frame(urban_coverage[,c("Gauge_ID", "_count")]),
                                by="Gauge_ID")

simple_2021c <- simple_2021b %>% mutate(across(.cols=starts_with("simple"), .fns= ~ . /`_count`))

simple_2021c$MKZ <- NA
all_catchments_TO$MK <- NA

#Match MKZ to urbanisation
for(i in seq_len(nrow(simple_2021c))){
  w <- which(PF_MK$Gauge_ID == simple_2021c$Gauge_ID[i])
  simple_2021c$MKZ[i] <- PF_MK$MKZ[w[1]]
}

all_catchments_TO <- skip_duplicate(all_catchments_TO)
all_catchments_2021 <- skip_duplicate(all_catchments_2021)

PF_MK$to_urb_pc <- NA
PF_MK$from_urb_pc <- NA
PF_MK$urb_2021_pc <- NA

for(i in seq_len(length(station_list))){
  k <- which(PF_MK$Station == station_list[i])
  wto <- which(catchments_to_urban$NRFA_ID == station_list[i])
  wfrom <- which(catchments_from_urban$NRFA_ID == station_list[i])
  wurb <- which(urban_coverage$NRFA_ID == station_list[i])
  if(length(wto)!=1){next}

  #if(length(k) < 1){print(station_list[i])}
  PF_MK$to_urb_pc[k] <- catchments_to_urban$`_mean`[wto]
  PF_MK$from_urb_pc[k] <- catchments_from_urban$`_mean`[wfrom]
  PF_MK$urb_2021_pc[k] <- urban_coverage$`_mean`[wurb]
}

# Factor for plotting
PF_MK$MK_sig <- factor((PF_MK$MK_pval<0.05)*sign(PF_MK$MKZ), levels=c(1,0,-1), labels=c("Pos Trend", "No Trend", "Neg Trend"), ordered=T)

write_csv(PF_MK, "./Data/Context/UrbanAndTrend.csv")

PF_MK <- read_csv("./Data/Context/UrbanAndTrend.csv")
PF_shapes <- read_sf("./Data/Context/Urban_in_21_WF.shp")

PF_shapes2 <- left_join(PF_shapes, PF_MK, by=c("Gauge_ID")) #%>% arrange(desc(CAREA))

#### PLOTTING ####

ggplot() +
  geom_sf(data=PF_shapes, aes(fill=`_mean`*100)) + 
  scale_fill_viridis_c(limits=c(0.1, 100), trans="log10",
                         guide=guide_colorbar()) +
  labs(fill="Urban coverage (%)")

ggplot() +
  geom_sf(data = uk_outline,
          fill = "white",
          colour = "grey60") +
  geom_sf(data=PF_shapes2, aes(fill=MKZ)) + 
  theme_bw()

new_scale <- function(new_aes) {
  structure(ggplot2::standardise_aes_names(new_aes), class = "new_aes")
}
#only plot change in urbanisation with non-missing trend statistic
PF_shapes3 <- PF_shapes2 %>% dplyr::filter(!is.na(MK_sig))
PF_shapes3 <- PF_shapes3 %>% dplyr::mutate(pc_change=to_urb_pc - from_urb_pc)




ggplot() +
  geom_sf(data = uk_outline,
          fill = "white",
          colour = "grey60") +
  geom_sf(data=PF_shapes2, aes(fill=MK_sig)) + 
  scale_fill_manual(values=c("Neg Trend"="dodgerBlue", "No Trend"="grey70", "Pos Trend"="brown1")) +
  labs(fill="Trend in AMAX") +
  theme_bw()

g1 <- ggplot(PF_shapes3) + 
  geom_histogram(aes(x=urb_2021_pc*100, y=after_stat(density), fill=MK_sig)) +
  scale_fill_manual(values=c("Pos Trend"="brown1","No Trend"="grey70", "Neg Trend"="dodgerBlue")) +
  labs(fill="Trend in AMAX", y="Prob Density", x="Urban Extent (%)") +
  theme_bw(base_size=8) +
  facet_grid(rows=vars(MK_sig))

g2 <- ggplot(PF_shapes3) + 
  geom_point(aes(x=pc_change*100, y=MKZ, col=MK_sig)) +
  scale_colour_manual(values=c("Neg Trend"="dodgerBlue", "No Trend"="grey50", "Pos Trend"="brown1")) +
  theme_bw(base_size=8) +
  labs(col="Trend in AMAX", x="Percentage change in urban 90-15", y="MKZ statistic") 

g3 <- ggplot(PF_shapes3) + 
  geom_point(aes(x=from_urb_pc*100, y=MKZ, col=MK_sig)) +
  scale_colour_manual(values=c("Neg Trend"="dodgerBlue", "No Trend"="grey50", "Pos Trend"="brown1")) +
  theme_bw(base_size=8) +
  labs(col="Trend in AMAX", x="Percentage change FROM urban 90-15", y="MKZ statistic") 

g4 <- ggplot(PF_shapes3) + 
  geom_point(aes(x=urb_2021_pc*100, y=MKZ, col=MK_sig)) +
  scale_colour_manual(values=c("Neg Trend"="dodgerBlue", "No Trend"="grey50", "Pos Trend"="brown1")) +
  theme_bw(base_size=8) +
  labs(col="Trend in AMAX", x="Percentage Urban 2021", y="MKZ statistic") 

png("./Images/urban_vs_flow_trendB.png", width=160, height=80, units="mm", res=300, pointsize=8)
cowplot::plot_grid(
  g2,
  g4,
  nrow = 1,
  ncol = 2,
  labels = c("(a)","(b)","(c)"),
  label_size = 11
)
dev.off()

all_vals <- readr::read_csv("./Data/Context/Aligned_events.csv")
all_vals2 <- all_vals %>% left_join(PF_MK, by=c("Gauge ID"="Gauge_ID")) %>% 
  group_by(`Gauge ID`) %>%
  slice_min(order_by=Rank_flow, n=1, na_rm=T)

f <-\(x){sum(x<2, x<3, x<4, x<11)}
all_vals2$rank_f <- sapply(all_vals2$Rank_flow, f)
all_vals2$rank_factor <- factor(all_vals2$rank_f, 
                                levels=4:0,
                                labels=c("AMAX1",
                                         "AMAX2",
                                         "AMAX3",
                                         "AMAX<10",
                                         paste0("AMAX","\U2265","10")))
png("./Images/UrbanVMaxRP_flow.png", width=90, height=90, units="mm", res=240, pointsize=9)
  ggplot(all_vals2) +
    geom_point(aes(x=100/as.numeric(AEP_flow), y=urb_2021_pc*100, col=rank_factor)) +
    scale_x_continuous(trans="log10") + 
    labs(x="Max return period in WF (yrs)", y="Urban Extent in 2021 (%)", colour="AMAX rank") +
    scale_colour_manual(
      values = c("black", "red", "purple", "dodgerblue3", "grey50"),
      drop = F
    ) +
    theme_bw(base_size=8)
dev.off()

hist_storms <- readr::read_csv("./Data/Context/Extent_of_old_storms_regional.csv", show_col_types=F)
hist_storms <- hist_storms %>%
  dplyr::filter(Area==params$region) 
hist_storms1 <- hist_storms %>% 
  select("Storm"=Storm_name, "Start"=Start_Date, "End"= End_Date, "Flow"=flow, "Level"=level) 

feh_flow <- readr::read_csv(feh_flow_filename)


all_vals3 <- all_vals %>% left_join(PF_MK, by=c("Gauge ID"="Gauge_ID")) %>% 
  group_by(`Gauge ID`) %>%
  slice_min(order_by=ror_rank, n=1, na_rm=T)

all_vals <- all_vals %>% filter(Volume_2_week > 0 | is.na(Volume_2_week))

ror_trends <- read_csv("./Data/Context/ROR_Trends.csv") %>%
  mutate(trend = (trend_sig<=0.05)*sign(trend_Z)) %>%
  filter(period==1)
ror_trends <- ror_trends[str_detect(ror_trends$file, fixed("/Q 15/035008_ror.csv"), negate=T),]

all_vals4 <- left_join(ror_trends, all_vals, by=c("GaugeID"="Gauge ID"))




