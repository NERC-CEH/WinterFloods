# Adam Griffin 2024-01-31
# 08458: Winter Floods 2019-21

# Compute confidence intervals on peak flow using bootstrapping methods

# Version 0.1: 2024-01-31. Initial development of code
# Version 0.2: 2024-02-14. Refactoring for wider distribution.


##### SETUP
library(dplyr)
library(lubridate)
library(readr)
library(tidyverse)
library(readxl)

key_details_filename <- "./Data/Context/key_details_with_gw_cosmos.csv"
key_details2_filename <- "./Data/Metadata/Master Station Listings.xlsx"
peak_flow_filename <- "./Data/Flow/FEH_AMAX_flood_frequency_analysis.csv"
ess_filename <- "Data/WINFAP-ESS/ESS-on_NRFA-11.csv"

peak_flow <- readr::read_csv(peak_flow_filename)
ess_in <- readr::read_csv(ess_filename)
key_details <- readr::read_csv(key_details_filename)

key_details2 <- readxl::read_xlsx(key_details2_filename, sheet=3)

peak_flow <- peak_flow %>% 
  dplyr::mutate(RP = 100/as.numeric(`AEP Preferred`),
                yL = -log(RP - 1))


peak_flow$LB <- NA
peak_flow$UB <- NA
M <- 300

key_details <- key_details2

for(i in seq_len(nrow(peak_flow))){
  print(i)
  w <- which(str_remove(key_details$`Gauge ID`,"^0+") ==
               str_remove(peak_flow$Site[i],"^0+"))
  if(peak_flow$`AEP Preferred`[i] == "No AMAX"){next}
  if(length(w)>0){
    st <- key_details$`NRFA ID`[w[1]]
    reclen <- as.numeric(key_details$`Length of data record`[w[1]])
    v <- which(ess_in$Station == st)
    if(length(v)>0){
      if(is.na(reclen)){
        reclen <- 2022 - as.numeric(key_details$`Gauging station start year`[w[1]])
      }
      t3 <- -1*ess_in$GLOkappa[v[1]]
      QMED <- ess_in$QMED[v[1]]
      beta <- ess_in$GLObeta[v[1]]

      QT <- rep(NA, M)
      for(m in 1:M){
        X <- ilaprosUtils::rglo(reclen, loc=QMED, scale=beta*QMED, -t3)
        lmo <- lmomco::lmoms(X)
        QT[m] <- 1/(1 - lmomco::cdfglo(peak_flow$`Event peak`[i], lmomco::parglo(lmo)))
      }
      ulb <- quantile(QT, c(0.025, 0.975))
      peak_flow$UB[i] <- round(signif(max(ulb), 3),1)
      peak_flow$LB[i] <- round(signif(min(ulb), 3),1)
    }
  }
}

peak_flow$ci_print <- paste0(round(peak_flow$RP,1), " (", peak_flow$LB, " - ", peak_flow$UB, ")")

peak_flow$ci_print[peak_flow$`AEP Preferred`=="No AMAX"] <- "No AMAX"

readr::write_csv(peak_flow, "./Data/WM_PF.csv")
