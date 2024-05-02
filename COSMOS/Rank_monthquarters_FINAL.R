### giaves 2023-09-05
# Rank COSMOS volumetric water content on each calendar day 


#### SETUP ####
rm(list = ls())
library(readr)
library(lubridate)
setwd("P:/08458 CWI-EA 2019-21 Flood Review")

#### KEY FILEPATHS ####
cosmos_data_folder <- "Data/COSMOS-UK_data/daily"
cosmos_mq_outfile <- "Data/COSMOS-UK_data/Ranked_Monthquarter/"

CF <- list.files(cosmos_data_folder, full = TRUE)
N <- length(CF)
SiteName <- rep("", N)

for (i in 1:N) {
  
  SiteName[i] <- strsplit(readLines(CF[i], n = 3)[3], ",")[[1]][2]
  SiteName[i] <- gsub(" ", "_", SiteName[i])
  Rec <- read.csv(CF[i], skip = 5)[ , 1:2]
  colnames(Rec) <- c("Date", "VWC")
  Rec$Date <- ymd(Rec$Date)
  Rec$M <- month(Rec$Date, 6, 7)
  Rec$D <- day(substr(Rec$Date, 9, 10))
  Rec$Q <- Rec$D
  # Compute month quarter (more consistent than weeks)
  for(d in 1:dim(Rec)[1]) {
    if (Rec$M[d] == "02") {
      Rec$Q[d] <- sum(c(1, Rec$D[d]>8, Rec$D[d]>15, Rec$D[d]>21))
      # gives quarter of month in Feb
    } else {
      Rec$Q[d] <- sum(c(1, Rec$D[d]>9, Rec$D[d]>16, Rec$D[d]>24))
      # gives quarter of month otherwise
    }
  }
  Rec$MQ <- paste(Rec$M, Rec$Q, sep = "-Q")
  Rec$DateQ <- paste0(substr(Rec$Date, 1, 5), Rec$MQ)
  VWCQ <- aggregate(Rec$VWC, by = list(Rec$DateQ), mean, na.rm = TRUE)
  colnames(VWCQ) <- c("DateQ", "VWCQ")
  
  RecNAO <- na.omit(Rec)
  L <- aggregate(RecNAO$VWC, by = list(RecNAO$DateQ), length)
  colnames(L) <- c("DateQ", "L")
  
  Rec <- merge(Rec, VWCQ)
  Rec <- merge(Rec, L, all = TRUE)
  Rec <- Rec[order(Rec$Date), ]
  
  UMQ <- sort(unique(Rec$MQ))
  
  Rec <- Rec[ , c("DateQ", "VWCQ", "MQ", "L")]
  Rec <- unique(Rec)
  
  RA <- rep(NA, 5)
  for (j in 1:length(UMQ)) {
    RJ <- Rec[which(Rec$MQ == UMQ[j]), ]
    RJ <- RJ[rev(order(RJ$VWC, na.last = FALSE)), ]
    RJ$Rank <- 1:dim(RJ)[1]
    RJ$Rank[is.na(RJ$VWC)] <- NA
    RJ$Of <- max(RJ$Rank, na.rm = TRUE)
    RJ$Of[is.na(RJ$VWC)] <- NA
    RA <- rbind(RA, RJ)
  }
  
  RA <- RA[order(RA$Date), c(1, 2, 4, 5, 6)]
  RA <- RA[-which(is.na(RA$Date)), ]
  colnames(RA) <- c("Month_quarter", "Mean_VWC", "Based_on_days", "Rank", "Of")
  
  readr::write_csv(RA,
      sprintf(paste0(cosmos_mq_outfile,"%s_monthquarter.csv"), SiteName[i]))
  
}
