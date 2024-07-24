#### Griffin, Adam. 2023-09-01
# EA project 35752: Hydrological analysis of the 2019-2021 flooding

# Main contributor: Adam Griffin
# Info: Script to perform cluster analysis of POT series based on kernel density clustering (Merz et al., 2016, JoH)

# Version 0.1: 2023-09-01. Initial development of code
# Version 0.2: 2023-11-01. Refactoring for wider distribution.
# Version 1.0: 2024-07-22. Final version for wider distribution.

##### SETUP #####
library(ggplot2)
library(dplyr)
library(lubridate)
library(extRemes)
library(lmomco)
library(circular)
library(boot)
library(Kendall)
library(tidyverse) # contains dplyr, readr, tidyr, magrittr
source("./cluster_analysis_functions_FINAL.R")

##### Key Arguments #####
KeyDetails_filename <- "" # file containning all key location metadata (1 station per row)
KeyDetails_long_filename <- "" #file containing all key event metadata (1 event per row)
POT_database_filename <- ""
# POT_database should have STATION, DATE_TIME, VALUE, FLAG for columns
ID_col_KD <- "Gauge ID"
ID_col_POT <- "STATION"
Plot_folder <- "" # output folder for figures

DAY_TOLERANCE <- 5 # 5 day tolerance of stated event date

KeyDetails <- readr::read_csv(KeyDetails_filename) # one station per row
KeyDetails_long <- readr::read_csv(KeyDetails_long_filename) # one event per row

POT_database <- readr::read_csv(POT_database_filename,
  col_names = c("STATION", "DATE_TIME", "VALUE", "FLAG"),
  col_types = "ccdc"
)


## Preallocate results columns
KeyDetails_long$PoissonRate_1yr <- NA
KeyDetails_long$Signif_1yr <- FALSE
KeyDetails_long$PoissonRate_2yr <- NA
KeyDetails_long$Signif_2yr <- FALSE
KeyDetails_long$ROWNO <- 1:nrow(KeyDetails_long)

L <- unique(POT_database[ID_col_POT])
# station identifiers in the POT database should match those in the summary file.

for (i in seq_along(L)) {
	# find the right station
  stn <- L[i]
  ww <- which(KeyDetails[ID_col] == stn)
  # get data for given station and timeperiod
  POT_1stn <- POT_database %>%
    dplyr::filter(STATION == stn) %>%
    dplyr::select(DATE_TIME, VALUE) %>%
    dplyr::mutate(DATE_TIME = lubridate::ymd_hms(DATE_TIME, truncated = 3))
  KD_1stn <- KeyDetails_long %>%
    dplyr::filter(`Gauge ID` == KeyDetails$`Gauge ID`[ww])
  for (bw_yr in c(1 * 365, 2 * 265)) {
  # perform cluster analysis on POT data
    clust1 <- denclust(POT_1stn$DATE_TIME,
      bwi = bw_yr,
      daysNotSeconds = F,
      M = 500,
      plotnow = T,
      plotfilename =
        paste0(Plot_folder, KeyDetails$`Gauge ID`[ww], "_", bw_yr, "_yr.png")
    )
	# update output object
    clust1$bwi <- bw_yr
    clust1$gaugeID <- KeyDetails[ID_col_KD][i]
    cluster_list[[length(cluster_list) + 1]] <- clust1
    names(cluster_list)[length(cluster_list)] <- paste0(clust1$gaugeID, "_", bw_yr)
    clust1$density_out <- data.frame(
      daterange = clust1$density_out[, 1],
      kd_extended = clust1$density_out[, 2],
      date = as.Date(clust1$density_out[, 1])
    )
	# Check density object for reporting
    DO <- clust1$density_out
    for (j in seq_len(nrow(KD_1stn))) {
      w_days <- which(abs(lubridate::difftime(DO$date, KD_1stn$EventDate[j],
                                              units = "days")) < DAY_TOLERANCE)
      DO_sub <- DO[w_days, ]
      poisrate <- max(DO_sub$kd_extended * 365, na.rm = T)
      signif <- poisrate / 365 > clust1$conf_interval[2] # checks for statistical significance
      if (bw_yr == 1) {
        KeyDetails_long$PoissonRate_1yr[KD_1stn$ROWNO[j]] <- poisrate
        KeyDetails_long$Signif_1yr[KD_1stn$ROWNO[j]] <- signif
      } else {
        KeyDetails_long$PoissonRate_2yr[KD_1stn$ROWNO[j]] <- poisrate
        KeyDetails_long$Signif_2yr[KD_1stn$ROWNO[j]] <- signif
      }
    }
  }
}

#### WRITE OUT DATA ####

readr::write_csv(KeyDetails_long, file = paste0(Plot_folder, "KeyDetails_out.csv"))
