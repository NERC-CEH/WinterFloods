### giaves 2023-11-09
# 08458: Winter Floods 2019-21

# Main contributor: GV
# Info: Plot highest COSMOS volumetric water content for each site

# Version 0.1: 2023-11-09. Initial development of code
# Version 0.2: 2023-11-30. Refactoring for wider distribution.

#### SETUP ####
library(viridis)

#### KEY FILEPATHS ####
monthquarter_folder <- "Data/COSMOS-UK_data/Ranked_Monthquarter"
plot_outfolder <- "Images/COSMOS 7-day plots/"


CF <- list.files(monthquarter_folder, full = TRUE)
CFname <- list.files(monthquarter_folder, full = FALSE)
N <- length(CF)

for (i in 1:N) {
  
  ### Read in data
  SiteName <- unlist(strsplit(CFname[i], "_monthquarter.csv"))
  Rec <- read.csv(CF[i])[1:3]
  colnames(Rec)[1:2] <- c("Date", "VWC")
  
  WFRec <- Rec[which(Rec$Date >= "2019-06-Q1" & Rec$Date <= "2021-06-Q4"), ]
  
  #determine ranges
  MaxVWC <- which.max(WFRec$VWC)
  
  Day <- substr(WFRec$Date[MaxVWC], 6, 10)
  
  PD <- which(substr(Rec$Date, 6, 10) == Day)
  
  # plotting dataframe
  PlotDates <- Rec[PD, ]
  PlotDates$Year <- as.numeric(substr(PlotDates$Date, 1, 4))
  PlotDates <- PlotDates[which(!is.na(PlotDates$VWC)), ]
  
  if (length(Day) == 1){
    
    MN <- as.numeric(substr(Day, 1, 2))
    MQ <- substr(Day, 5, 5)
    
	# month quarters
    MonthDays <- if (MQ == 1) "01-08" else if (MQ == 2) "09-15" else if (MQ == 3) "16-23"
    if (MQ == 4) {
      if (MN %in% c(1, 3, 5, 7, 8, 10, 12)) MonthDays <- "24-31" else MonthDays <- "24-30"
    }
    if (MN == 2) MonthDays <- if (MQ == 1) "01-07" else if (MQ == 2) "08-14" else if (MQ == 3) "15-22" else "23-28/29"
    
    MonthText <- paste(MonthDays, month.abb[as.numeric(substr(Day, 1, 2))])
    
  }

    
  
  #### PLOTTING ####
  
  if(dim(PlotDates)[1] == 0){
  # No data plot
    png(sprintf(paste0(plot_outfolder,"/%s-7-day-VWC.png"), SiteName),
        units = "mm", width = 80, height = 80, res = 600, family = "FreeSans")
    par(mar = c(0, 0, 0, 0))
    plot(NA, xlim = c(0, 1), ylim = c(0, 1))
    text(0.5, 0.5, sprintf("No data at %s\nfor June 2019 - June 2021", gsub("_", " ", SiteName)))
    dev.off()
  }else{
	# colour palette
    vir <- if (max(na.omit(PlotDates$Based_on_days)) == 8) rev(viridis(8)) else rev(viridis(7))
    Y20 <- which(PlotDates$Year == 2020)
    Y21 <- if(as.numeric(substr(Day, 1, 2)) <= 6) which(PlotDates$Year == 2021) else which(PlotDates$Year == 2019)
    
    png(sprintf(paste0(plot_outfolder,"/%s-7-day-VWC.png"), SiteName),
        units = "mm", width = 80, height = 80, res = 600, family = "FreeSans")
    par(mar = c(3, 3, 1, 1), mgp = c(2, 0.75, 0), cex = 2/3)
    plot(NA,
         xlab = "Year",
         ylab = "Mean VWC (%)",
         main = sprintf("%s: mean VWC for %s", gsub("_", " ", SiteName), MonthText),
         xlim = c(min(PlotDates$Year-0.5), max(PlotDates$Year+0.5)),
         ylim = c(0, 1.05*max(PlotDates$VWC, na.rm = TRUE)),
         xaxs = "i", yaxs = "i", xaxt = "n")
    
    for (Y in 1:length(PD)){
      polygon(x=c(PlotDates$Year[Y]-0.5,PlotDates$Year[Y]-0.5,
                  PlotDates$Year[Y]+0.5,PlotDates$Year[Y]+0.5),
              y=c(0, PlotDates$VWC[Y], PlotDates$VWC[Y], 0),
              #col = vir[PlotDates$Based_on_days[Y]]
              )
      text(PlotDates$Year[Y], min(PlotDates$VWC)/2, PlotDates$Based_on_days[Y], cex = 1.5)
      if (length(Y20) == 1){
        polygon(x=c(PlotDates$Year[Y20]-0.5, PlotDates$Year[Y20]-0.5,
                    PlotDates$Year[Y20]+0.5, PlotDates$Year[Y20]+0.5),
                y=c(0, PlotDates$VWC[Y20], PlotDates$VWC[Y20], 0),
                col = "red",
                density = 10)
      }
      if (length(Y21) == 1){
        polygon(x=c(PlotDates$Year[Y21]-0.5, PlotDates$Year[Y21]-0.5,
                    PlotDates$Year[Y21]+0.5, PlotDates$Year[Y21]+0.5),
                y=c(0, PlotDates$VWC[Y21], PlotDates$VWC[Y21], 0),
                col = "red",
                density = 10)
      }
      abline(h = max(PlotDates$VWC), lty = 2, col = "gray50")
      axis(1, at = PlotDates$Year)
    }
    dev.off()
  }
}