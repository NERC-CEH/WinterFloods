library(tidyverse)
library(texmex)
library(lmomco)

setwd("P:/08458 CWI-EA 2019-21 Flood Review/Code/Groundwater")

allAMAX <- readr::read_csv(
  "P:/08458 CWI-EA 2019-21 Flood Review/Data/Groundwater/FullRecord_Level_AMAX.csv")

allAMIN <- readr::read_csv(
  "P:/08458 CWI-EA 2019-21 Flood Review/Data/Groundwater/FullRecord_Dip_AMIN.csv")

events <- readr::read_csv(
  "P:/08458 CWI-EA 2019-21 Flood Review/Data/Groundwater/Event_Antecedent_max_level_or_min_dip2.csv")
allAMAX$Mrow <- 1:nrow(allAMAX)
events$Nrow <- 1:nrow(events)

events$rank_AMAX <- NA
events$empirical_AMAX <- NA
events$GEV_AMAX <- NA

events$rank_AMIN <- NA
events$empirical_AMIN <- NA
events$GEV_AMIN <- NA

U <- sort(unique(c(allAMAX$GW.Gauge, allAMIN$GW.Gauge)))
V <- sort(unique(events$GW.Gauge))
V <- V[V %in% U]
for(i in seq_along(V)){
  print(paste(i,V[i]))
  AMAX_1stn <- allAMAX %>% dplyr::filter(GW.Gauge == V[i])
  am <- AMAX_1stn %>% select(WY, Level_AMAX)
  DIP_1stn <- allAMIN %>% dplyr::filter(GW.Gauge==V[i])
  dip <- DIP_1stn %>% select(WY, Dip_AMIN)
  events_1stn <- events %>% dplyr::filter(GW.Gauge == V[i])
  ev <- events_1stn %>% select(EventDate, Level_AP_max, Dip_AP_min, Nrow)
  names(ev) <- c("WY", "Level_AMAX", "Dip_AMIN", "Nrow")
  
  if(any(!is.na(ev$Level_AMAX))){
    AMAX_event_rank <- sapply(ev$Level_AMAX, \(x){2 + nrow(AMAX_1stn) - rank(c(x, am$Level_AMAX))[1]})
    AMAX_distn <- evm(Level_AMAX, data=am, family=gev)
    rpAMAX <- 1-texmex::pgev(q=ev$Level_AMAX, mu=AMAX_distn$par[1],  sigma=AMAX_distn$par[2],  xi=AMAX_distn$par[3])
    events$rank_AMAX[ev$Nrow] <- AMAX_event_rank
    events$GEV_AMAX[ev$Nrow] <- rpAMAX
    events$empirical_AMAX[ev$Nrow] <- AMAX_event_rank/(nrow(AMAX_1stn)+1)
    
    am[nrow(am)+1,] <- am[nrow(am),]
    am$WY[nrow(am)] <- am$WY[nrow(am)]+1
    am$plotdate <- ymd(paste0(am$WY,"-01-01"))
    g <- ggplot(am) + 
      geom_step(aes(x=plotdate, y=Level_AMAX)) +
      geom_point(data=ev, aes(x=WY, y=Level_AMAX), col="red", pch=4, size=1.5, stroke=1.5) +
      lims(y=c(0.95*min(am$Level_AMAX), max(am$Level_AMAX))) +
      theme_bw() +
      labs(x="Date", y="AMAX level(mAOD)")
    g
    png(paste0("./Plots/", V[i], "_level.png"), width=100, height=60, units="mm", res=180, pointsize=9)
    print(g)
    dev.off()
    
  }
  if(any(!is.na(ev$Dip_AMIN))){
    ev$DipAMIN_fit <- -1*ev$Dip_AMIN
    dip$DipAMIN_fit <- -1*dip$Dip_AMIN
    AMIN_event_rank <- sapply(ev$Dip_AMIN, \(x){rank(c(x, dip$Dip_AMIN))[1]})
    AMIN_distn <- evm(DipAMIN_fit, data=dip, family=gev)
    rpAMIN <- 1- texmex::pgev(q=ev$DipAMIN_fit, mu=AMIN_distn$par[1],  sigma=AMIN_distn$par[2],  xi=AMIN_distn$par[3])
    events$rank_AMIN[ev$Nrow] <- AMIN_event_rank
    events$GEV_AMIN[ev$Nrow] <- rpAMIN
    events$empirical_AMIN[ev$Nrow] <- AMIN_event_rank/(nrow(DIP_1stn)+1)
    
    dip[nrow(dip)+1,] <- dip[nrow(dip),]
    dip$WY[nrow(dip)] <- dip$WY[nrow(dip)]+1
    dip$plotdate <- ymd(paste0(dip$WY,"-01-01"))
    h <- ggplot(dip) + 
      geom_step(aes(x=plotdate, y=Dip_AMIN)) +
      geom_point(data=ev, aes(x=WY, y=Dip_AMIN), col="red", pch=4, size=1.5, stroke=1.5) +
      lims(y=c(min(dip$Dip_AMIN), max(dip$Dip_AMIN))) +
      theme_bw() +
      labs(x="Date", y="AMAX depth(m)")
    h
    png(paste0("./Plots/", V[i], "_dipped.png"), width=100, height=60, units="mm", res=180, pointsize=9)
    print(h)
    dev.off()
  }
}

events$InAMAX <- (events$rank_AMAX %% 1) > 0.01
events$InAMAX <- ifelse(events$InAMAX, "Yes", "No")
events$InAMIN <- (events$rank_AMIN %% 1) > 0.01
events$InAMIN <- ifelse(events$InAMIN, "Yes", "No")
events$rank_AMAX <- floor(events$rank_AMAX)
events$rank_AMIN <- floor(events$rank_AMIN)


events_level <- events %>%
  dplyr::filter(!is.na(rank_AMAX)) %>%
  select(Area, GW.Gauge, EventDate, Level_AP_max, InAMAX, rank_AMAX, empirical_AMAX, GEV_AMAX)
events_level$empirical_AMAX <- signif(events_level$empirical_AMAX, 3)
events_level$GEV_AMAX <- signif(events_level$GEV_AMAX, 3)

write_csv(events_level, "./Data/Table_Groundwater_Level.csv")

events_dip <- events %>%
  dplyr::filter(!is.na(rank_AMIN)) %>%
  select(Area, GW.Gauge, EventDate, Dip_AP_min, InAMIN, rank_AMIN, empirical_AMIN, GEV_AMIN)
events_dip$empirical_AMIN <- signif(events_dip$empirical_AMIN, 3)
events_dip$GEV_AMIN <- signif(events_dip$GEV_AMIN, 3)

write_csv(events_dip, "./Data/Table_Groundwater_Dipped.csv")
# 
# 
# events_dip <- events_out %>%  %>% select()
# events_amax
# 
# 
# f <- \(x){paste(min(x), "-", max(x))}
# allAMAX_grouped_A <- allAMAX %>% group_by(GW.Gauge) %>% summarise(por=f(WY), reclen=n())
# allAMAX_grouped_B <- allAMAX %>% group_by(GW.Gauge) %>% slice_max(Level_AMAX, n=1)
# allAMAX_grouped <- full_join(allAMAX_grouped_A, allAMAX_grouped_B)
# write_csv(allAMAX_grouped, "./Table10.csv")
