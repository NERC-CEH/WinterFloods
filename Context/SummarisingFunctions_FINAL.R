#### Griffin, Adam. 2024-01-01
# EA project 35752: Hydrological analysis of the 2019-2021 flooding

# Main contributor: Adam Griffin
# Info: Key functions for summarising and plotting key events and the antecedent conditions

# Version 0.1: 2024-01-01. Initial development of code
# Version 0.2: 2023-01-11. Refactoring for wider distribution.
# Version 1.0: 2024-07-22. Final version for wider distribution.

##### SETUP #####
library(Kendall)
library(boot)
library(lubridate)
library(dplyr)
library(ggplot2)
library(sf)


monthquarter <- function(date) {
  # get monthquarter from date
  d <- day(y)
  if (month(y) == 2) {
    mq <- sum(c(1, d > 8, d > 15, d > 21))
  } else {
    mq <- sum(c(1, d > 9, d > 16, d > 24))
  }
  L
}

MKZs <- function(x) {
  # Manual calculation of Z-score associated with Mann-Kendall test 
  S <- Kendall::MannKendall(x)$S
  varS <- Kendall::MannKendall(x)$varS
  #Calculate the Zs statistic
  if (S > 0) {
    Zs <- (S-1)/sqrt(varS)
  } else if (S < 0) {
    Zs <- (S+1)/sqrt(varS)
  } else {
    Zs <- 0
  }
  return(Zs)  # MkZs
}

MKZ_blockboot <- function(x, bootl=4){
  # Block-bootstrapped version of Mann-Kendall test
  # 
  # x       timeseries of annual (or monthly etc.) values
  # bootl   length of block in timesteps.
  b4 <- sort(boot::tsboot(x, MKZs, R=10000,  sim="fixed", l=bootl)$t)
  ci <- b4[c(250,9750)]  # B4 lower
  truemk <- MKZs(x)
  sigp <- (truemk > ci[2] | truemk < ci[1])
  attributes(truemk) <- NULL
  return(list(mkz=truemk, mk_ciL=ci[1], mk_ciU=ci[2], sigp=sigp))
}

denclust <- function(dateline, bwi, daysNotSeconds=TRUE, M=500, plotnow=TRUE,
                     plotfilename="./FirstDensityExample.png"){
  # Manual computation of density clustering as defined in Merz et al. (2016)
  #
  # dateline  vector  dates of occurances
  # bwi       float   bandwidth in timesteps (days as standard)
  # daysNotSeconds    if true, assumes date objects not datetime objects
  # M         int     number of Monte Carlo replicates
  # plotnow           if true, also plots to screen
  # plotfilename      filename string for .png image
  # cs_dates_line     vector of dates to highlight in plot
  
  daystamps <- as.numeric(dateline)
  if(!daysNotSeconds){daystamps <- daystamps/(24*60*60)}
  raw_dates <- date(dateline)
  daterange <- seq(date(min(raw_dates))-1,date(max(raw_dates))+1,by="day")
  daterangestamps <- as.numeric(daterange)
  
  ND <- length(daterange)
  NE <- length(daystamps)
  daystamps_extended <- c(2*min(daystamps) - rev(daystamps[-1]),
                          daystamps,
                          2*max(daystamps) - rev(daystamps)[-1])
  kd_extended <- rep(0,ND)
  
  # Kernel rate density estimate [Merz et al 2016]
  for(t in 1:ND){
    kd_extended[t] <- 1/bwi*sum(
      dnorm((daterangestamps[t] - daystamps_extended)/bwi, 0, 1))
  }
  #### MONTE CARLO CONFIDENCE INTERVAL ####
  sq2pi <- sqrt(2*pi)
  f <- \(x){exp(-x^2/2)/sq2pi}
  kdmat <- matrix(0, nrow=M, ncol=ND)
  for(m in 1:M){
    #if(m < 10 | m %% 50 == 0){message(paste0(100*m/M, "% MC complete."))}
    simm <- cumsum(rexp(NE, NE/ND))
    for(t in 1:ND){
      kdmat[m,t] <- 1/bwi*sum(f((t - simm)/bwi))
    }
  }
  # Monte Carlo 90% confidence intervals
  kdav <- apply(
    apply(kdmat[,floor(ND/2 - bwi):ceiling(ND/2 + bwi)], 2, 
          FUN=\(x){quantile(x,probs=c(0.05,0.95))}),
    1, mean)
  
  s1 <- sum(rle(kd_extended > kdav[2])$values)
  s2 <- sum(rle(kd_extended < kdav[1])$values)
  #print(paste0(s1, " significant clusters and ", s2, " significant absences."))
  dt <- cbind(daterange, kd_extended)
  w <- which(dt[-1,2] > kdav[2] & dt[-nrow(dt),2] < kdav[2]) + 1
  clusstart <- daterange[w]
  
  ### PLOT ###
  if(plotnow){
    g <- \(){
      par(mar=c(3,3,1,1), mgp=c(2,1,0))
      plot(daterange, 365*kd_extended, type='l',
           xlab=paste("Date, bw =",bwi), ylab="Rate estimate",
           ylim=c(0,1.2*max(365*kd_extended, 365*kdav)))
      abline(h=365*NE/ND, col="red", lty=2)
      rug(date(raw_dates))
      abline(h=365*kdav, col="blue", lty=3)
    }
    g()
    png(plotfilename,
        width=100, height=90, units="mm", res=300, pointsize=10)
    g()
    dev.off()
  }
  return(list(density_out=dt,
              conf_interval=kdav,
              peaks_troughs=c(s1, s2),
              mean_per_year=365*NE/ND,
              cluster_start_date=clusstart,
              bwi=bwi)
  )
}

sen_not_zyp <- function (x,y){
  # Independent computation of Thiel-Sen estimate, not from zyp package.
  # x   dates
  # y   values of flow
  
  # returns fitted model object
  slopediff <- 
    \(i, xx, yy, n) (yy[1:(n - i)] - yy[(i + 1):n])/(xx[1:(n - i)] - xx[(i + 1):n])
  n <- length(x)
  slopes <- unlist(lapply(1:(n - 1), slopediff, x, y, n))
  sni <- which(is.finite(slopes))
  slope <- median(slopes[sni])
  intercepts <- y - slope * x
  intercept <- median(intercepts)
  res <- list(coefficients = c(intercept, slope),
              slopes = slopes, 
              intercepts = intercepts,
              rank = 2,
              residuals = (y-slope * x+intercept),
              x = x,
              y = y)
  names(res$coefficients) = c("Intercept", "slope")
  class(res) = c("zyp", "lm")
  return(res)
}

add_storms <- function(data_in, date_col, key_storms){
  # add_storms adds an extra column to the input data with the storm name
  #
  # data_in     dataframe with a date column
  # date_col    string identifying the date column
  # key_storms  dataframe of key storms: storm name, start date, end date, group name, group id
  #
  # returns a copy of the dataframe with the additional storm information.
  
  key_storm_groups <- key_storms %>%
    group_by(group, group_name) %>%
    summarise(start_date_gp = min(start_date)-days(2),
              end_date_gp = max(end_date)+days(2))
  
  data_in$storm_name <- NA
  data_in$storm_start_date <- lubridate::ymd(18000101)
  data_in$storm_group <- NA
  data_in$storm_group_id <- NA
  
  for(i in 1:nrow(data_in)){
    w <- which(key_storms$start_date <= all_vals$Date[i] &
                 key_storms$end_date >= all_vals$Date[i])
    if(length(w) > 0){
      data_in$storm_name[i] <- key_storms$storm_name[w[1]]
      data_in$storm_start_date[i] <- key_storms$start_date[w[1]]
    }
    
    v <- which(key_storm_groups$start_date_gp <= data_in$Date[i] &
                 key_storm_groups$end_date_gp >= data_in$Date[i])
    if(length(v)>0){
      data_in$storm_group[i] <- key_storm_groups$group_name[v[1]]
      data_in$storm_group_id[i] <- key_storm_groups$group[v[1]]
    }
    
  }
  
  data_in$storm_start_date[
    data_in$storm_start_date == lubridate::ymd(18000101)] <- NA
  
  return(data_in)
}


make_rank <- function(
    measure, data_in, rank_col, date_col, id_col, region_col, regions=NULL,
    key_storms, storm_col, storm_groups=NULL, rp=FALSE){
  
  # compute maximum rank of given statistic during a given storm group
  #
  # measure       string identifying metric e.g. flow, groundwater
  # data_in       dataframe with measured data and dates
  # rank_col      string of column name in data_in with rp
  # date_col      string of column name in data_in with dates
  # id_col        string of column name in data_in with station id
  # region_col    string of column name in data_in with region/area names
  # regions       vector of regions of interest. If null performs nationally
  #               without regional grouping
  # key_storms    dataframe of key storms: storm_col, start date, end date
  # storm_col     string of column name in key_storms with storm/season name
  # storm_groups  vector of storms or seasons of interest as found in
  #               key_storms$storm_col

  # setup
  ks_out_list <- list()
  data_in$rowno <- 1:nrow(data_in)
  data_in$rank_f <- NA
  data_in$rank_factor <- NA
  
  if(is.null(storm_groups)){
    storm_groups <- unique(key_storms[[storm_col]])  #could be storms or seasons
  }

  if(is.null(region)){
    data_in$Dummy <- "National"
    region_col <- "Dummy"
    regions <- "National"
  }

  for(gp in storm_groups){ # for each storm/season
    for(reg in regions){ # for each region
    
      ks_out_list[[gp]] <- list() # set up part of dataframe
      ks_out_list[[gp]]$group_name <- gp
      ks_out_list[[gp]]$Area <- reg
      ks_out_list[[gp]]$measure <- measure
      
      #get group start and end dates
      gp_start <-
        min(key_storms$start_date[key_storms[[storm_col]] == gp], na.rm = T)
      gp_end <-
        max(key_storms$end_date[key_storms[[storm_col]] == gp], na.rm = T)
      gp_range <- seq(from = gp_start, to = gp_end, by = "day")
      
      #get data for one storm/season in one region
      one_storm <- data_in %>%
        dplyr::filter(.data[[date_col]] %in% gp_range,
                      !is.na(.data[[rank_col]]),
                      .data[[region_col]] == reg) %>%
        group_by(.data[[id_col]]) %>%
        slice_min(order_by = .data[[rank_col]], n = 1)
      
      ## take the rank and put it into a factor for plotting

      if(!rp){
        f <- \(x) { sum(x < 2, x < 3, x < 4, x < 11)}
        labs <-c(
          "AMAX1",
          "AMAX2",
          "AMAX3",
          "AMAX<10",
          paste0("AMAX", "\U2265", "10"))
      }else{
        f <- \(x) {sum(x > 50, x > 20, x > 5, x > 2)}
        labs <- c(
          "AEP < 1 in 50",
          "AEP < 1 in 20",
          "AEP < 1 in 5",
          "AEP < 1 in 2",
          paste0("AEP ", "\U2265", " 1 in 2"))
      }
      one_storm$rank_f <- sapply(one_storm[[rank_col]], f)
      one_storm$rank_factor <- factor(
        one_storm$rank_f,
        levels = 4:0,
        labels = labs
        )
      one_storm <- one_storm %>% dplyr::filter(!is.na(rank_factor))
      # count the number of events above AMAX1/2/3/10 and total counts
      ks_out_list[[gp]][["AMAX1"]] <- sum(
        one_storm$rank_factor == labs[1])
      ks_out_list[[gp]][["AMAX2"]] <- sum(
        one_storm$rank_factor %in% labs[1:2])
      ks_out_list[[gp]][["AMAX3"]] <- sum(
        one_storm$rank_factor %in%  labs[1:3])
      ks_out_list[[gp]][["AMAX10"]] <- sum(
        one_storm$rank_factor %in%  labs[1:4])
      ks_out_list[[gp]][["obs"]] <- nrow(one_storm)
      
      
      ks_out_list[[gp]] <- data.frame(ks_out_list[[gp]])
      
      data_in[data_in$rowno %in% one_storm$row_no, ] <- one_storm
      
    }
  }
  # glue together the parts of the dataframe and return
  ks_out <- do.call(rbind.data.frame, ks_out_list)
  data_in <- data_in %>% select(!any_of(rowno, Dummy))
  return(list(ks_out=ks_out, data_in=data_in))
}


make_rank_map <- function(
    measure, data_in, rank_col, date_col, id_col, region_col, regions=NULL,
    key_storms, storm_col, storm_groups=NULL, rp=FALSE,
    folder_for_maps=NULL, uk_outline=NULL, uk_rivers=NULL){
  
  # make maps indicating maximum rank of given statistic during a given storm group
  #
  # measure       string identifying metric e.g. flow, groundwater
  # data_in       sf dataframe with measured data and dates
  # rank_col      string of column name in data_in with rp
  # date_col      string of column name in data_in with dates
  # id_col        string of column name in data_in with station id
  # region_col    string of column name in data_in with region/area names
  # regions       vector of regions of interest. If null performs nationally
  #               without regional grouping
  # key_storms    dataframe of key storms: storm_col, start date, end date
  # storm_col     string of column name in key_storms with storm/season name
  # storm_groups  vector of storms or seasons of interest as found in
  #               key_storms$storm_col
  # rp            if TRUE, plot return periods not rank
  # folder_for_maps   filepath of folder to save map in
  # uk_outline        shapefile of outline of region of interest
  # uk_rivers         shapefile of main rivers
  
    subfolder <- paste0(measure, "Rank")
    legend_title <- paste(measure, " Rank")
    if(!dir.exists(paste0(folder_for_maps, subfolder))){
      dir.create(paste0(folder_for_maps, subfolder))
    }
    plot_path <- paste0(folder_for_maps, subfolder,"/", gp, "_",subfolder)
    
    if(is.null(storm_groups)){
      storm_groups <- unique(key_storms[[storm_col]])  #could be storms or seasons
    }
    
    if(is.null(region)){
      data_in$Dummy <- "National"
      region_col <- "Dummy"
      regions <- "National"
    }
    
    if(!rp){
      labs <-c("AMAX1", "AMAX2", "AMAX3", "AMAX<10",paste0("AMAX", "\U2265", "10"))
    }else{
      labs <- c("RP>50", "RP>20", "RP>5", "RP>2", paste0("RP", "\U2264", "2"))
    }
    
    for(gp in storm_groups) { # for each storm/season
      
      #get group start and end dates
      gp_start <-
        min(key_storms$start_date[key_storms[[storm_col]] == gp], na.rm = T)
      gp_end <-
        max(key_storms$end_date[key_storms[[storm_col]] == gp], na.rm = T)
      gp_range <- seq(from = gp_start, to = gp_end, by = "day")
      
      for (reg in regions) { # for each region
        one_storm <- data_in %>%
          dplyr::filter(.data[[date_col]] %in% gp_range,
                        !is.na(.data[[rank_col]]),
                        .data[[region_col]] == reg) %>%
          group_by(.data[[id_col]]) %>%
          slice_min(order_by = .data[[rank_col]], n = 1)
        
        # setup the plot
        g_flow <- ggplot()
        if (!is.null(uk_outline)) {
          # add a country outline
          g_flow <- g_flow + 
            geom_sf(data = uk_outline, fill = "white", colour = "grey60")
        }
        if (!is.null(uk_rivers)) {
          # add rivers if provided
          g_flow <- g_flow +
            geom_sf(data = uk_rivers,
                    colour = "powderblue",
                    aes(linewidth = as.factor(MAIN_RIVER)))
        }
        if (nrow(data_in_1storm) < 2) {
          ## Insufficient data plot, needs a bit of fudging to get the legend
          data_in$Dummy <- factor(0, levels = 4:0, labels = labs)
          
          g_flow <- g_flow +
            geom_sf(data = data_in, aes(colour = dummy), alpha = 0) +
            coord_sf(datum = st_crs(uk_outline)) +
            theme_bw(base_size = 8) +
            scale_colour_manual(
              values = c("black", "red", "purple", "dodgerblue3", "grey50"),
              drop = F) +
            guides(colour = guide_legend(override.aes = list(alpha = 1)),
                   linewidth = "none") +
            labs(colour = paste(measure, "rank")) +
            annotation_custom(grid::gTree(children = grid::gList(
              grid::textGrob(
                paste0("Insufficient Data"),
                hjust = 0.5, x = 0.5, vjust = 0.5, y = 0.5)
            )))
        }else{
          g_flow <- geom_sf(data = data_map,
                            fill = "black",
                            size = 0.25) +
            geom_sf(data = one_storm[order(one_storm[[rank_col]], decreasing = F),],
                    aes(colour = .data[[rank_col]]), size = 2) +
            coord_sf(
              xlim = c(bbox[1], bbox[3]),
              ylim = c(bbox[2], bbox[4]),
              expand = T,
              datum = st_crs(data_map))
          if (!rp) {
            g_flow <- g_flow +
              scale_colour_viridis_c(
                limits = c(0.3, 100),
                trans = "log10",
                breaks = c(0.3, 1, 3, 10, 30, 100),
                labels = c("", "<1", 3, 10, 30, ">100"),
                guide = guide_colorbar())
          } else{
            g_flow <- g_flow +
              scale_colour_manual(
                values = c("black", "red", "purple", "dodgerblue3", "grey50"),
                drop = F)
          }
          g_flow <- g_flow +
            labs(colour = paste(measure, ifelse(rp, "1 in x AEP", "Rank"))) +
            guides(size = "none", linewidth = "none") +
            theme_bw(base_size = 8)
        }
      }
    }
    
    png(paste0(plot_path, ".png"), width=80, height=70, units="mm",
        res=600, pointsize=10)
      print(g_flow)
    dev.off()
    pdf(paste0(plot_path, ".pdf"), width=80/25.4, height=70/25.4, pointsize=10)
      print(g_flow)
    dev.off()
    
    return(g_flow)
}

regional_rank_grouped <- function(region, measure, data_in, rank_col, date_col,
                                  id_col_data, storm_groups, key_storms){
  # give maximum rank of given statistic across a region during a given storm group
  # region    string identifying region
  # measure   string identifying metric e.g. flow, groundwater
  # data_in   dataframe with measured data and dates
  # rank_col  string of column name in data_in with ranks
  # date_col  string of column name in data_in with dates
  # id_col_data string of column name in data_in with station id
  # storm_groups vector of storm group names
  # key_storms  dataframe of key storms: storm name, start date, end date, group name, group id
  
  ks_out_list <- list()
  
  for(gp in storm_groups){ #for each storm group
    
    ks_out_list[[gp]] <- list() # set up part of dataframe
    ks_out_list[[gp]]$group_name <- gp
    ks_out_list[[gp]]$Area <- region
    ks_out_list[[gp]]$measure <- measure
    
    ks <- key_storms %>% dplyr::filter(key_storms$group_name == gp)
    
    gp_start <-
      min(ks$start_date, na.rm = T) - days(2)
    gp_end <-
      max(ks$end_date, na.rm = T)
    gp_range <- 
      seq(from = gp_start, to = gp_end, by = "day")
    
    # subset the data to just that storm season in that region,
    # take the highest rank at each station
    one_storm <- data_in %>%
      dplyr::filter(.data[[date_col]] %in% gp_range,
                    Area == region, !is.na(.data[[rank_col]])) %>%
      group_by(.data[[id_col_data]]) %>%
      slice_max(order_by = .data[[rank_col]], n = 1)
    
    ## take the rank and put it into a factor for plotting
    f <- \(x) {sum(x < 2, x < 3, x < 4, x < 11)}
    one_storm$rank_f <- sapply(one_storm[[rank_col]], f)
    one_storm$rank_factor <- factor(
      one_storm$rank_f,
      levels = 4:0,
      labels = c(
        "AMAX1",
        "AMAX2",
        "AMAX3",
        "AMAX<10",
        paste0("AMAX", "\U2265", "10")
      )
    )
    # count the number of events above AMAX1/2/3/10 and total counts
    one_storm <- one_storm %>% dplyr::filter(!is.na(rank_factor))
    ks_out_list[[gp]][["AMAX1"]] <- sum(
      one_storm$rank_factor == "AMAX1")
    ks_out_list[[gp]][["AMAX2"]] <- sum(
      one_storm$rank_factor %in% c("AMAX1", "AMAX2"))
    ks_out_list[[gp]][["AMAX3"]] <- sum(
      one_storm$rank_factor %in% c("AMAX1", "AMAX2", "AMAX3"))
    ks_out_list[[gp]][["AMAX10"]] <- sum(
      one_storm$rank_factor %in% c("AMAX1", "AMAX2", "AMAX3", "AMAX<10"))
    ks_out_list[[gp]][["obs"]] <- nrow(one_storm)
    
  }
  # glue together the parts of the dataframe and return
  ks_out <- do.call(rbind.data.frame, ks_out_list)
  return(ks_out)
}

regional_rp_grouped <- function(region, measure, data_in, rank_col, date_col,
                                id_col_data, storm_groups, key_storms){
  # give maximum return period of given statistic across a region during a given storm group
  # region    string identifying region
  # measure   string identifying metric e.g. flow, groundwater
  # data_in   dataframe with measured data and dates
  # rank_col  string of column name in data_in with rp
  # date_col  string of column name in data_in with dates
  # id_col_data string of column name in data_in with station id
  # storm_groups vector of storm group names
  # key_storms  dataframe of key storms: storm name, start date, end date, group name, group id
  
  ks_out_list <- list()
  
  for(gp in storm_groups){ #for each storm group
    
    ks_out_list[[gp]] <- list() # set up part of dataframe
    ks_out_list[[gp]]$group_name <- gp
    ks_out_list[[gp]]$Area <- region
    ks_out_list[[gp]]$measure <- measure
    
    ks <- key_storms %>% dplyr::filter(key_storms$group_name == gp)
    
    gp_start <-
      min(ks$start_date, na.rm = T) - days(2)
    gp_end <-
      max(ks$end_date, na.rm = T)
    gp_range <- 
      seq(from = gp_start, to = gp_end, by = "day")
    
    # subset the data to just that storm season in that region,
    # take the highest rank at each station
    one_storm <- data_in %>%
      dplyr::filter(.data[[date_col]] %in% gp_range,
                    Area == region, !is.na(.data[[rank_col]])) %>%
      group_by(.data[[id_col_data]]) %>%
      slice_max(order_by = .data[[rank_col]], n = 1)
    
    ## take the rank and put it into a factor
    f <- \(x) {sum(x > 2, x > 5, x > 20, x > 50)}
    one_storm$rank_f <- sapply(one_storm[[rank_col]], f)
    one_storm$rank_factor <- factor(
      one_storm$rank_f,
      levels = 4:0,
      labels = c(
        "AMAX1",
        "AMAX2",
        "AMAX3",
        "AMAX<10",
        paste0("AMAX", "\U2265", "10")
      )
    )
    one_storm <- one_storm %>% dplyr::filter(!is.na(rank_factor))
    # count the number of events above AMAX1/2/3/10 and total counts
    ks_out_list[[gp]][["AMAX1"]] <- sum(
      one_storm$rank_factor == "AMAX1")
    ks_out_list[[gp]][["AMAX2"]] <- sum(
      one_storm$rank_factor %in% c("AMAX1", "AMAX2"))
    ks_out_list[[gp]][["AMAX3"]] <- sum(
      one_storm$rank_factor %in% c("AMAX1", "AMAX2", "AMAX3"))
    ks_out_list[[gp]][["AMAX10"]] <- sum(
      one_storm$rank_factor %in% c("AMAX1", "AMAX2", "AMAX3", "AMAX<10"))
    ks_out_list[[gp]][["obs"]] <- nrow(one_storm)
    
  }
  # glue together the parts of the dataframe and return
  ks_out <- do.call(rbind.data.frame, ks_out_list)
  return(ks_out)
}

rank_grouped <- function(
    measure, data_in, rank_col, date_col, id_col_data, storm_groups, key_storms){
  
  # give maximum rank of given statistic during a given storm group
  # region    string identifying region
  # measure   string identifying metric e.g. flow, groundwater
  # data_in   dataframe with measured data and dates
  # rank_col  string of column name in data_in with rp
  # date_col  string of column name in data_in with dates
  # id_col_data string of column name in data_in with station id
  # storm_groups vector of storm group names
  # key_storms  dataframe of key storms: storm name, start date, end date, group name, group id
  
  ks_out_list <- list()
  
  for(gp in storm_groups){ #for each storm group
    
    ks_out_list[[gp]] <- list() # set up part of dataframe
    ks_out_list[[gp]]$group_name <- gp
    ks_out_list[[gp]]$Area <- "National"
    ks_out_list[[gp]]$measure <- measure
    
    #get group start and end dates
    gp_start <-
      min(key_storms$start_date[key_storms$group_name == gp], na.rm = T)
    gp_end <-
      max(key_storms$end_date[key_storms$group_name == gp], na.rm = T)
    gp_range <- seq(from = gp_start, to = gp_end, by = "day")
    
    # subset the data to just that storm season in that region,
    # take the highest rank at each station
    one_storm <- data_in %>%
      dplyr::filter(.data[[date_col]] %in% gp_range, !is.na(.data[[rank_col]])) %>%
      group_by(.data[[id_col_data]]) %>%
      slice_min(order_by = .data[[rank_col]], n = 1)
    
    f <- \(x) {sum(x < 2, x < 3, x < 4, x < 11)}
    
    one_storm$rank_f <- sapply(one_storm[[rank_col]], f)
    one_storm$rank_factor <- factor(
      one_storm$rank_f,
      levels = 4:0,
      labels = c(
        "AMAX1",
        "AMAX2",
        "AMAX3",
        "AMAX<10",
        paste0("AMAX", "\U2265", "10")
      )
    )
    one_storm <- one_storm %>% dplyr::filter(!is.na(rank_factor))
    
    ks_out_list[[gp]][["AMAX1"]] <- sum(
      one_storm$rank_factor == "AMAX1")
    ks_out_list[[gp]][["AMAX2"]] <- sum(
      one_storm$rank_factor %in% c("AMAX1", "AMAX2"))
    ks_out_list[[gp]][["AMAX3"]] <- sum(
      one_storm$rank_factor %in% c("AMAX1", "AMAX2", "AMAX3"))
    ks_out_list[[gp]][["AMAX10"]] <- sum(
      one_storm$rank_factor %in% c("AMAX1", "AMAX2", "AMAX3", "AMAX<10"))
    ks_out_list[[gp]][["obs"]] <- nrow(one_storm)
  }
  # glue together the parts of the dataframe and return
  ks_out <- do.call(rbind.data.frame, ks_out_list)
  return(ks_out)
  
}

rp_grouped <- function(
    measure, data_in, rank_col, date_col, id_col_data, storm_groups){
  
  ks_out_list <- list()
  
  for(gp in storm_groups){ #for each storm group
    
    ks_out_list[[gp]] <- list() # set up part of dataframe
    ks_out_list[[gp]]$group_name <- gp
    ks_out_list[[gp]]$Area <- "National"
    ks_out_list[[gp]]$measure <- measure
    
    gp_start <-
      min(key_storms$start_date[key_storms$group_name == gp], na.rm = T)
    gp_end <-
      max(key_storms$end_date[key_storms$group_name == gp], na.rm = T)
    gp_range <- seq(from = gp_start, to = gp_end, by = "day")
    
    one_storm <- data_in %>%
      dplyr::filter(.data[[date_col]] %in% gp_range, !is.na(.data[[rank_col]])) %>%
      group_by(.data[[id_col_data]]) %>%
      slice_min(order_by = .data[[rank_col]], n = 1)
    
    f <- \(x) {
      sum(x > 2, x > 5, x > 20, x > 50)
    }
    
    one_storm$rank_f <- sapply(one_storm[[rank_col]], f)
    one_storm$rank_factor <- factor(
      one_storm$rank_f,
      levels = 4:0,
      labels = c(
        "AMAX1",
        "AMAX2",
        "AMAX3",
        "AMAX<10",
        paste0("AMAX", "\U2265", "10")
      )
    )
    one_storm <- one_storm %>% dplyr::filter(!is.na(rank_factor))
    
    ks_out_list[[gp]][["AMAX1"]] <- sum(one_storm$rank_factor == "AMAX1")
    ks_out_list[[gp]][["AMAX2"]] <- sum(one_storm$rank_factor %in% c("AMAX1", "AMAX2"))
    ks_out_list[[gp]][["AMAX3"]] <- sum(one_storm$rank_factor %in% c("AMAX1", "AMAX2", "AMAX3"))
    ks_out_list[[gp]][["AMAX10"]] <- sum(one_storm$rank_factor %in% c("AMAX1", "AMAX2", "AMAX3", "AMAX<10"))
    ks_out_list[[gp]][["obs"]] <- nrow(one_storm)
  }
  # glue together the parts of the dataframe and return
  ks_out <- do.call(rbind.data.frame, ks_out_list)
  return(ks_out)
  
}


make_maps_rank_grouped <- function(
    measure, data_in, rank_col, date_col, id_col_data, id_col_key,
    gp, save_image=T){
  
  # make maps of rank of given statistic during a given storm group
  # measure   string identifying metric e.g. flow, groundwater
  # data_in   dataframe with measured data and dates
  # rank_col  string of column name in data_in with rp
  # date_col  string of column name in data_in with dates
  # id_col_data string of column name in data_in with station id
  # id_col_key string of column name with storm group id
  # gp name of storm group
  # save_image  if TRUE, saves to .png else just plots to current device
  
  
  # Save location and title
  subfolder <- paste0(measure, "Rank")
  legend_title <- paste(measure, " Rank")
  if(!dir.exists(paste0(folder_for_maps,  subfolder))){
    dir.create(paste0(folder_for_maps,  subfolder))
  }
  plot_path <- paste0(folder_for_maps, subfolder,"/", gp,
                      "_",subfolder, ".png")
  
  gp_start <- min(key_storms$start_date[key_storms$group_name == gp], na.rm=T)
  gp_end <- max(key_storms$end_date[key_storms$group_name == gp], na.rm=T)
  gp_range <- seq(from=gp_start, to=gp_end, by="day")
  
  data_in_1storm <- data_in %>%
    dplyr::filter(.data[[date_col]] %in% gp_range, !is.na(.data[[rank_col]])) %>% 
    group_by(.data[[id_col_data]]) %>%
    slice_min(order_by=.data[[rank_col]], n=1)
  
  # Get relevant data for given storm group
  data_map <- key_details_map %>% 
    dplyr::filter(.data[[id_col_key]] %in% data_in[[id_col_data]])
  
  ### Plotting empty plot
  if(nrow(data_in_1storm) < 2){
    ## Insufficient data plot, needs a bit of fudging to get the legend
    g_flow <- ggplot() +
      geom_sf(data = uk_outline,
              fill = "white",
              colour = "grey60") +
      geom_sf(data=key_details_map, aes(colour=dummy), alpha=0) +
      coord_sf(datum=st_crs(data_map)) +
      theme_bw(base_size=8) +
      scale_colour_manual(
        values = c("black",
                   "red",
                   "purple",
                   "dodgerblue3",
                   "grey50"),
        drop = F
      ) +
      guides(colour = guide_legend(override.aes = list(alpha=1)),
             linewidth="none") +
      labs(colour = paste(measure, "rank"), x="Easting", y="Northing") +
      annotation_custom(grid::gTree(children = grid::gList(
        grid::textGrob(
          paste0("Insufficient Data"),
          hjust = 0.5,
          x = 0.5,
          vjust = 0.5,
          y = 0.5
        )
      )))
  }else{
    one_storm <- merge(x=key_details_map,
                       y=data_in_1storm,
                       by.x=id_col_key,
                       by.y=id_col_data,
                       all=F)
    ## take the rank and put it into a factor for plotting
    f <-\(x){sum(x<2, x<3, x<4, x<11)}
    one_storm$rank_f <- sapply(one_storm[[rank_col]], f)
    one_storm$rank_factor <- factor(one_storm$rank_f, 
                                    levels=4:0,
                                    labels=c("AMAX1",
                                             "AMAX2",
                                             "AMAX3",
                                             "AMAX<10",
                                             paste0("AMAX","\U2265","10")))
    one_storm <- one_storm %>% dplyr::filter(!is.na(rank_factor))
    
    
    ## Plotting ##
    g_flow <- ggplot() +
      geom_sf(data = uk_outline,
              fill = "white",
              colour = "grey60") +
      geom_sf(data=data_map,
              fill="black",
              size=0.25) +
      geom_sf(data = one_storm[order(one_storm$rank_f, decreasing = F), ],
              aes(colour = rank_factor), size = 1) +
      coord_sf(datum=st_crs(data_map)) +
      scale_colour_manual(
        values = c("black", "red", "purple", "dodgerblue3", "grey50"),
        drop = F
      ) +
      labs(colour = paste(measure, "rank"), x="Easting", y="Northing") +
      theme_bw(base_size=8) +
      guides(linewidth="none")
  }
  
  ## Save image ##
  png(plot_path, width=78, height=68, units="mm", res=240, pointsize=7)
  print(g_flow)
  dev.off()
  
  return(g_flow)
}

make_maps_rp_grouped <- function(measure, data_in, rp_col, id_col_data, id_col_key, gp, date_col,
                                 save_image=T){
  
  # make maps of return period of given statistic during a given storm group
  # measure   string identifying metric e.g. flow, groundwater
  # data_in   dataframe with measured data and dates
  # rp_col  string of column name in data_in with rp
  # date_col  string of column name in data_in with dates
  # id_col_data string of column name in data_in with station id
  # id_col_key string of column name with storm group id
  # gp name of storm group
  # save_image  if TRUE, saves to .png else just plots to current device
  
  subfolder <- paste0(measure, "RP")
  legend_title <- paste(measure, " RP")
  if(!dir.exists(paste0(folder_for_maps,  subfolder))){
    dir.create(paste0(folder_for_maps,  subfolder))
  }
  plot_path <- paste0(folder_for_maps, subfolder, "/", gp,
                      "_", subfolder, ".png")
  
  gp_start <- min(key_storms$start_date[key_storms$group_name == gp], na.rm=T)
  gp_end <- max(key_storms$end_date[key_storms$group_name == gp], na.rm=T)
  gp_range <- seq(from=gp_start, to=gp_end, by="day")
  
  data_in_1storm <- data_in %>%
    dplyr::filter(.data[[date_col]] %in% gp_range,
                  !is.na(.data[[rp_col]])) %>% 
    group_by(.data[[id_col_data]]) %>%
    slice_max(order_by=.data[[rp_col]], n=1)
  
  data_map <- key_details_map %>% 
    dplyr::filter(.data[[id_col_key]] %in% data_in[[id_col_data]])
  if(nrow(data_in_1storm)<2){
    ## Insufficient data plot, needs a bit of fudging to get the legend
    g_flow <- ggplot() +
      geom_sf(data = uk_outline,
              fill = "white",
              colour = "grey60") +
      geom_sf(data=key_details_map, aes(colour=rp_dummy), alpha=0) +
      coord_sf(datum=st_crs(data_map)) +
      theme_bw(base_size=8) +
      scale_colour_viridis_c(limits=c(0.3,100), trans="log10",
                             breaks=c(0.3,1,3,10,30,100),
                             labels=c("","<1",3,10,30,">100"),
                             guide=guide_colorbar()) +
      labs(colour = paste("1 in x", measure, "AEP"), x="Easting", y="Northing") +
      guides(linewidth="none") +
      annotation_custom(grid::gTree(children = grid::gList(
        grid::textGrob(
          paste0("Insufficient Data"),
          hjust = 0.5,
          x = 0.5,
          vjust = 0.5,
          y = 0.5
        )
      )))
  }else{
    one_storm <- merge(x=key_details_map,
                       y=data_in_1storm,
                       by.x=id_col_key,
                       by.y=id_col_data,
                       all=F)
    one_storm <- one_storm %>% dplyr::filter(!is.na(.data[[rp_col]]))
    one_storm[[rp_col]] <- pmin(one_storm[[rp_col]], 300)
    ## Plotting ##
    g_flow <- ggplot() +
      geom_sf(data = uk_outline,
              fill = "white",
              colour = "grey60") +
      geom_sf(data=data_map,
              fill="black",
              size=0.25) +
      geom_sf(data = one_storm[order(one_storm[[rp_col]], decreasing = F), ],
              aes(colour = .data[[rp_col]]), size = 2) +
      coord_sf(datum=st_crs(data_map)) +
      scale_colour_viridis_c(limits=c(0.3,300), trans="log10",
                             breaks=c(0.3,1,3,10,30,100,300),
                             labels=c("","<1",3,10,30,100,">300"),
                             guide=guide_colorbar()) +
      labs(colour = paste("1 in x", measure, "AEP"), x="Easting", y="Northing") +
      guides(size="none", linewidth="none") + 
      theme_bw(base_size=8)
  }
  
  ## Save image ##
  if(save_image){
    png(plot_path, width=78, height=68, units="mm", res=240, pointsize=7)
    print(g_flow)
    dev.off()
  }
  return(g_flow)
}

make_maps_regional_RP_grouped <- function(
    measure, region, data_in, rp_col, id_col_data, date_col,
    id_col_key, gp, bbox, save_image=T){
  
  # make maps of rank of given statistic during a given storm group
  # measure   string identifying metric e.g. flow, groundwater
  # region    string identifying EA region
  # data_in   dataframe with measured data and dates
  # rp_col  string of column name in data_in with rp
  # date_col  string of column name in data_in with dates
  # id_col_data string of column name in data_in with station id
  # id_col_key string of column name with storm group id
  # gp name of storm group
  # bbox    4-tuple of bounding box for plot (xmin,ymin,xmax,ymax)
  # save_image  if TRUE, saves to .png else just plots to current device
  
  subfolder <- paste0(measure, "RP/", region)
  legend_title <- paste(measure, " RP")
  if(!dir.exists(paste0(folder_for_maps,  subfolder))){
    dir.create(paste0(folder_for_maps,  subfolder), recursive=T)
  }
  plot_path <- paste0(folder_for_maps, subfolder,"/", gp,
                      "_",region,"_", legend_title, ".png")
  
  regionP <- 
    region_to_print_table$printname[region_to_print_table$filename==region]
  
  
  gp_start <- min(key_storms$start_date[key_storms$group_name == gp], na.rm=T)
  gp_end <- max(key_storms$end_date[key_storms$group_name == gp], na.rm=T)
  gp_range <- seq(from=gp_start, to=gp_end, by="day")
  
  data_in_1storm <- data_in %>%
    dplyr::filter(.data[[date_col]] %in% gp_range, Area == region, !is.na(.data[[rp_col]])) %>% 
    group_by(.data[[id_col_data]]) %>%
    slice_max(order_by=.data[[rp_col]], n=1)
  
  data_map <- key_details_map %>% 
    dplyr::filter(.data[[id_col_key]] %in% data_in[[id_col_data]], Area == region)
  if(nrow(data_in_1storm)<2){
    ## Insufficient data plot, needs a bit of fudging to get the legend
    g_flow <- ggplot() +
      geom_sf(data = uk_outline,
              fill = "white",
              colour = "grey60") +
      geom_sf(data = basic_rivers,
              colour = "powderblue",
              aes(linewidth=as.factor(MAIN_RIVER))) +
      scale_linewidth_manual(values=c("0"=0.2,"1"=0.5)) + 
      geom_sf(data=key_details_map, aes(colour=rp_dummy), alpha=0) +
      coord_sf(xlim=c(bbox[1], bbox[3]),
               ylim=c(bbox[2], bbox[4]),
               expand=T,
               datum=st_crs(data_map)) +
      theme_bw(base_size=8) +
      scale_colour_viridis_c(limits=c(0.3,300), trans="log10",
                             breaks=c(0.3,1,3,10,30,100,300),
                             labels=c("","<1",3,10,30,100,">300"),
                             guide=guide_colorbar()) +
      guides(linewdith="none") +
      labs(colour = paste0("1 in x ", measure, " AEP"), x="Easting", y="Northing") +
      
      annotation_custom(grid::gTree(children = grid::gList(
        grid::textGrob(
          paste0("Insufficient Data"),
          hjust = 0.5,
          x = 0.5,
          vjust = 0.5,
          y = 0.5
        )
      )))

  }else{
    one_storm <- merge(x=key_details_map,
                       y=data_in_1storm,
                       by.x=id_col_key,
                       by.y=id_col_data,
                       all=F)
    one_storm <- one_storm %>% dplyr::filter(!is.na(.[[rp_col]]))
    one_storm[[rp_col]] <- pmin(one_storm[[rp_col]], 300)
    ## Plotting ##
    g_flow <- ggplot() +
      geom_sf(data = uk_outline,
              fill = "white",
              colour = "grey60") +
      geom_sf(data = basic_rivers,
              colour = "powderblue",
              aes(linewidth=as.factor(MAIN_RIVER))) + 
      scale_linewidth_manual(values=c("0"=0.2,"1"=0.5)) +
      geom_sf(data=data_map,
              fill="black",
              size=0.25) +
      geom_sf(data = one_storm[order(one_storm[[rp_col]], decreasing = F), ],
              aes(colour = .data[[rp_col]]), size = 2) +
      coord_sf(xlim=c(bbox[1], bbox[3]),
               ylim=c(bbox[2],bbox[4]),
               expand=T,
               datum=st_crs(data_map)) +
      scale_colour_viridis_c(limits=c(0.3,100), trans="log10",
                             breaks=c(0.3,1,3,10,30,100),
                             labels=c("","<1",3,10,30,">100"),
                             guide=guide_colorbar()) +
      labs(colour = paste("1 in x", measure, "AEP"), x="Easting", y="Northing") +
      guides(size="none", linewidth="none") + 
      theme_bw(base_size=8) 
  }
  
  ## Save image ##
  if(save_image){
    png(plot_path, width=78, height=68, units="mm", res=240, pointsize=7)
    print(g_flow)
    dev.off()
  }
  return(g_flow)
}

make_maps_regional_rank_grouped <- function(
    region, measure, data_in, rank_col, date_col,
    id_col_data, id_col_key, gp, bbox, save_image=T){
  
  # make maps of rank of given statistic during a given storm group
  # measure   string identifying metric e.g. flow, groundwater
  # region    string identifying EA region
  # data_in   dataframe with measured data and dates
  # rank_col  string of column name in data_in with rp
  # date_col  string of column name in data_in with dates
  # id_col_data string of column name in data_in with station id
  # id_col_key string of column name with storm group id
  # gp name of storm group
  # bbox    4-tuple of bounding box for plot (xmin,ymin,xmax,ymax)
  # save_image  if TRUE, saves to .png else just plots to current device
  
  subfolder <- paste0(measure, "Rank/", region)
  legend_title <- paste(measure, "Rank")
  if(!dir.exists(paste0(folder_for_maps,  subfolder))){
    dir.create(paste0(folder_for_maps,  subfolder), recursive=T)
  }
  plot_path <- paste0(folder_for_maps, subfolder,"/", gp,
                      "_",region,"_", legend_title, ".png")
  
  regionP <- 
    region_to_print_table$printname[region_to_print_table$filename==region]
  
  gp_start <- min(key_storms$start_date[key_storms$group_name == gp], na.rm=T)
  gp_end <- max(key_storms$end_date[key_storms$group_name == gp], na.rm=T)
  gp_range <- seq(from=gp_start, to=gp_end, by="day")
  
  data_in_1storm <- data_in %>%
    dplyr::filter(.data[[date_col]] %in% gp_range,
                  Area==region, !is.na(.data[[rank_col]])) %>% 
    group_by(.data[[id_col_data]]) %>%
    slice_max(order_by=.data[[rank_col]], n=1)
  
  data_map <- key_details_map %>% 
    dplyr::filter(.data[[id_col_key]] %in% data_in[[id_col_data]],
                  Area == region)
  if(nrow(data_in_1storm)<2){
    ## Insufficient data plot, needs a bit of fudging to get the legend
    g_flow <- ggplot() +
      geom_sf(data = uk_outline,
              fill = "white",
              colour = "grey60") +
      geom_sf(data = basic_rivers,
              colour = "powderblue",
              aes(linewidth=as.factor(MAIN_RIVER))) +
      scale_linewidth_manual(values=c("0"=0.2,"1"=0.5)) +
      geom_sf(data=key_details_map, aes(colour=dummy), alpha=0) +
      coord_sf(xlim=c(bbox[1], bbox[3]),
               ylim=c(bbox[2], bbox[4]),
               expand=T,
               datum=st_crs(data_map)) +
      theme_bw(base_size=8) +
      scale_colour_manual(
        values = c("black",
                   "red",
                   "purple",
                   "dodgerblue3",
                   "grey50"),
        drop = F
      ) +
      guides(colour = guide_legend(override.aes = list(alpha=1)),
             linewidth="none") +
      labs(colour = paste(measure, "rank"), x="Easting", y="Northing") +
      annotation_custom(grid::gTree(children = grid::gList(
        grid::textGrob(
          paste0("Insufficient Data"),
          hjust = 0.5,
          x = 0.5,
          vjust = 0.5,
          y = 0.5
        )
      )))
    
  }else{
    one_storm <- merge(x=key_details_map,
                       y=data_in_1storm,
                       by.x=id_col_key,
                       by.y=id_col_data,
                       all=F)
    if("Area.x" %in% colnames(one_storm)){
      colnames(one_storm)[which(colnames(one_storm) == "Area.x")] <- "Area"
    }
    one_storm <- one_storm %>% dplyr::filter(Area == region)
    ## take the rank and put it into a factor for plotting
    f <-\(x){sum(x<2, x<3, x<4, x<11)}
    one_storm$rank_f <- sapply(one_storm[[rank_col]], f)
    one_storm$rank_factor <- factor(one_storm$rank_f, 
                                    levels=4:0,
                                    labels=c("AMAX1",
                                             "AMAX2",
                                             "AMAX3",
                                             "AMAX<10",
                                             paste0("AMAX","\U2265","10")))
    
    one_storm <- one_storm %>% dplyr::filter(!is.na(rank_factor))
    ## Plotting ##
    g_flow <- ggplot() +
      geom_sf(data = uk_outline,
              fill = "white",
              colour = "grey60") +
      geom_sf(data = basic_rivers,
              colour = "powderblue") +
      geom_sf(data=data_map,
              fill="black",
              size=0.25) +
      geom_sf(data = one_storm[order(one_storm$rank_f, decreasing = F), ],
              aes(colour = rank_factor), size = 2) +
      coord_sf(xlim=c(bbox[1], bbox[3]),
               ylim=c(bbox[2],bbox[4]),
               expand=T,
               datum=st_crs(data_map)) +
      scale_colour_manual(
        values = c("black", "red", "purple", "dodgerblue3", "grey50"),
        drop = F
      ) +
      labs(colour = paste(measure, "rank"), x="Easting", y="Northing") +
      theme_bw(base_size=8) +
      guides(linewidth="none")

  }
  
  ## Save image ##
  if(save_image){
    png(plot_path, width=78, height=68, units="mm", res=240, pointsize=7)
    print(g_flow)
    dev.off()
  }
  return(g_flow)
}
