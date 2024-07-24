### giaves 2023-09-11
# EA project 35752: Hydrological analysis of the 2019-2021 flooding

# Main contributor: Gianni Vesuviano
# Info: Plot DDF curves for volume relationships

# Version 0.1: 2023-09-11. Initial development of code
# Version 0.2: 2024-02-01. Refactoring for wider distribution.
# Version 1.0: 2024-07-22. Final version for wider distribution.


##### SETUP #####
library(tidyverse) # contains dplyr, readr, tidyr, magrittr
library(readxl)
library(lmom)
library(stringr)

##### KEY ARGUMENTS #####
events_filepath <- "Data/Tables_for_reporting/Table_4.csv" # event dates for volumes
key_details_filepath <- "Data/Master Station Listings UKCEH_post queries.xlsx" # key metadata, one station per row

vol_combined_filepath <- function(x){
  paste0("Data/Volumes/",x,"_week_max_Q_combined.csv")} # locationn of combined flow volumes.

col_lines <- RColorBrewer::brewer.pal(8,"BuPu")[4:8] # plotting palette

volume_plot_folder <- "./Data/Volumes/Plots/" #filepath for saving figures


##### READ IN DATA #####
Events <- readr::read_csv(events_filepath)

meta_in <- readxl::read_xlsx(key_details_filepath,
                             sheet = "PostQueries_FluvialGauged") %>% 
                             select(1, 5, 2)

AMAX1WK <- readr::read_csv(vol_combined_filepath(1))
AMAX2WK <- readr::read_csv(vol_combined_filepath(2))
AMAX4WK <- readr::read_csv(vol_combined_filepath(4))
AMAX6WK <- readr::read_csv(vol_combined_filepath(6))
AMAX8WK <- readr::read_csv(vol_combined_filepath(8))

STNs <- unique(Events$Site) %>% na.omit()


# Plotting for each station
for(i in seq_along(STNs)){
  
  # Get data for one station
  area <- meta_in$Area[which(
    stringr::str_remove(meta_in$`Gauge ID`, "^0") == 
      stringr::str_remove(STNs[i], "^0"))]
  print(paste(i, STNs[i]))
  w <- which(AMAX4WK$Station == STNs[i])
  v <- which(Events$Site == STNs[i])
  xaxis <- 10^(seq(-2.5,-0.01,length.out=101))
  
  
  # fit a GEV for each duration at this station: 1 - 8 weeks
  ### 1-week volume
  A1 <- AMAX1WK[w, ]
  A1 <- A1[A1$Comp1 >= 0.95, ] # only choose years with over 95% complete data
  # fit L-moments
  L1 <- lmom::pelgev(lmom::samlmu(A1$Week1))
  # Compute ranks
  Rank1 <- sapply(Events$Vol.1.week[v], \(x){length(which(A1$Week1 > x)) + 1})
  # Highlight events lower than whole AMAX series
  RankFlag1 <- sapply(Events$Vol.1.week[v],
                      \(x){if (x < min(A1$Week1)) "*" else ""})
  # get 1 in x AEP from GEV
  RP1 <- 1 / (1 - lmom::cdfgev(Events$Vol.1.week[v], L1))
  # get reduced variate for plotting
  RPx1 <- lmom::quagev(1-xaxis, L1)
  
  A2 <- AMAX2WK[w, ]
  A2 <- A2[A2$Comp2 >= 0.95, ]
  L2 <- lmom::pelgev(lmom::samlmu(A2$Week2))
  Rank2 <- sapply(Events$Vol.2.week[v], \(x){length(which(A2$Week2 > x)) + 1})
  RankFlag2 <- sapply(Events$Vol.2.week[v],
                      \(x){if (x < min(A2$Week2)) "*" else ""})
  RP2 <- 1 / (1 - lmom::cdfgev(Events$Vol.2.week[v], L2))
  RPx2 <- lmom::quagev(1-xaxis, L2)
  
  A4 <- AMAX4WK[w, ]
  A4 <- A4[A4$Comp4 >= 0.95, ]
  L4 <- lmom::pelgev(lmom::samlmu(A4$Week4))
  Rank4 <- sapply(Events$Vol.4.week[v], \(x){length(which(A4$Week4 > x)) + 1})
  RankFlag4 <- sapply(Events$Vol.4.week[v],
                      \(x){if (x < min(A4$Week4)) "*" else ""})
  RP4 <- 1 / (1 - lmom::cdfgev(Events$Vol.4.week[v], L4))
  RPx4 <- lmom::quagev(1-xaxis, L4)
  
  A6 <- AMAX6WK[w, ]
  A6 <- A6[A6$Comp6 >= 0.95, ]
  L6 <- lmom::pelgev(lmom::samlmu(A6$Week6))
  Rank6 <- sapply(Events$Vol.6.week[v], \(x){length(which(A6$Week6 > x)) + 1})
  RankFlag6 <- sapply(Events$Vol.6.week[v],
                      \(x){if (x < min(A6$Week6)) "*" else ""})
  RP6 <- 1 / (1 - lmom::cdfgev(Events$Vol.6.week[v], L6))
  RPx6 <- lmom::quagev(1-xaxis, L6)
  
  A8 <- AMAX8WK[w, ]
  A8 <- A8[A8$Comp8 >= 0.95, ]
  L8 <- lmom::pelgev(lmom::samlmu(A8$Week8))
  Rank8 <- sapply(Events$Vol.8.week[v], \(x){length(which(A8$Week8 > x)) + 1})
  RankFlag8 <- sapply(Events$Vol.8.week[v],
                      \(x){if (x < min(A8$Week8)) "*" else ""})
  RP8 <- 1 / (1 - lmom::cdfgev(Events$Vol.8.week[v], L8))
  RPx8 <- lmom::quagev(1-xaxis, L8)
    
  # Compile information for plotting
  L <- list(list("amax"=A1$Week1, "events"=Events$Vol.1.week[v],
                 "rp"=RP1, xax=RPx1),
            list("amax"=A2$Week2, "events"=Events$Vol.2.week[v],
                 "rp"=RP2, xax=RPx2),
            list("amax"=A4$Week4, "events"=Events$Vol.4.week[v],
                 "rp"=RP4, xax=RPx4),
            list("amax"=A6$Week6, "events"=Events$Vol.6.week[v],
                 "rp"=RP6, xax=RPx6),
            list("amax"=A8$Week8, "events"=Events$Vol.8.week[v],
                 "rp"=RP8, xax=RPx8))
  
  # save to png
  png(paste0(volume_plot_folder, area, "_", STNs[i], "_WM.png"),
      width=80, height=80, units="mm", pointsize=9, res=300)
    par(mar=c(3,5,1.4,0.4), mgp=c(4,1,0))
    
    plot(1/xaxis, L[[5]]$xax,
         ylim=c(min(L[[1]]$xax), max(L[[5]]$xax)),
         log="x", type='l',
         xlab="", ylab="Volume (Ml)",
         las=1, col=col_lines[1])
    
    title(xlab="Return period (years)", line=2)
    title(main=paste("Station", STNs[i]), line=0.5)
    
    for(i in 1:5){
      lines(1/xaxis, L[[i]]$xax, col=col_lines[i])
      points(L[[i]]$rp, L[[i]]$events, pch=19, col=col_lines[i])
    }
    
    legend("topleft",
           legend=c("1-week", "2-week", "4-week", "6-week", "8-week"),
           col=col_lines, lwd=1, cex=0.7,
           title="Duration")
  dev.off()
}