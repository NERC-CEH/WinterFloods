#### Griffin, Adam. 2024-01-15
# 08458: Winter Floods 2019-21

# Main contributor: Adam Griffin
# Info: Matching storms with antecedent conditions

# Version 0.1: 2024-01-15. Initial development of code

setwd("P:/08458 CWI-EA 2019-21 Flood Review")
#### SETUP ####
library(lfstat)
library(ggplot2)
library(nonstat) # this needs quite a specific folder structure,
# refer to documentation for where files are saved.

lfwy <- \(x){
  y=lfstat::water_year(x, origin=10, assign="start")
  as.numeric(levels(y)[y])
}

#### KEY FILEPATHS ####
folder <- "./Code/NonstationaryFFA/Data/NONST"
dist <- "GEV"
RPs <- c(2, 10, 20, 50, 100)
CI <- 90
key_details_filename <- "./Data/Master Station Listings UKCEH_post queries.xlsx"
all_amax_filename <- "./Data/Peaks from Continuous/AMAX_from_Q_15_list.csv"
GLO_RP_out <- "./Data/NONST/Outputs/KeyEvents_AEP_GLO.csv"
GEV_RP_out <- "./Data/NONST/Outputs/KeyEvents_AEP_GEV.csv"

# Trend testing ---------------------------------------------
MK.test(folder, sig.level = 5)
Pettitt.test(folder, sig.level = 5)
PELT.test(folder)

# Flood frequency analysis (time only as a covariate) -------
# Run the fitting function
GEVfits <- fit.time(dist, folder, RPs, CI)
GLOfits <- fit.time("GLO", folder, RPs, CI)

save(GEVfits, GLOfits, file="GEVfits.rda")
# Run the plotting functions
load("./Data/NONST/GEVfits.rda")

for(gauge in names(GEVfits)){
type <- "Varying location and scale"
fit.results <- GEVfits
RPs <- c(2, 10, 20, 50, 100) # return periods to plot
CI.RP <- 100 # return period for confidence interval plot

plot.all(folder, gauge, fit.results, RPs)

fit.results <- GLOfits
plot.all(folder, gauge, fit.results, RPs)
}


f <- \(x){as.numeric(stringr::str_extract(x, "[0-9]+"))}
# Metadata
key_details <- readxl::read_xlsx(key_details_filename, sheet=5) %>% 
  dplyr::filter(`Non-stationary probability analysis (water year covariate)` == "Y")
  dplyr::filter(!is.na(`NRFA ID`)) %>%
  dplyr::select(`NRFA ID`, `Gauge ID`, `Event 1`:`Event 6`) %>%
  tidyr::pivot_longer(cols=`Event 1`:`Event 6`,
                      names_to="Event Number",
                      names_transform=f,
                      values_to="Date") %>%
  dplyr::filter(!is.na(Date))

# Pre-allocation
KeyEvents$fit.stat <- NA
KeyEvents$fit.var.loc <- NA
KeyEvents$fit.var.scale <- NA
KeyEvents$fit.var.loc.scale <- NA
KeyEvents$rowNo <- 1:nrow(KeyEvents)
STN <- unique(KeyEvents$`Gauge ID`)

AMAX_15 <- read_csv(all_amax_filename)
AMAX_15$ID <- str_split_i(AMAX_15$Station, "[.]+", i=1) %>% str_remove("^0")
extra_GLOFITS <- list()

# Run fitting for non-NRFA stations with AMAX values
# GLO fitting
for(k in 1:length(STN)){
  if(!(STN[k] %in% AMAX_15$ID)){next}
  AMAX_1stn <- AMAX_15 %>% dplyr::filter(ID == STN[k]) %>%
    rename(Flow = AMAX) %>%
    mutate(WaterYear = lfwy(DateTime),
           WaterYearIndex = WaterYear - min(WaterYear),
           ScaledWaterYearIndex = scale(WaterYearIndex))
  L <- list()
  L$data <- AMAX_1stn
  L$fit.stat <- texmex::evm(y=Flow, data=data.frame(AMAX_1stn), family=glo,
              method="bootstrap")
  L$fit.var.loc <- texmex::evm(y=Flow, data=data.frame(AMAX_1stn), family=glo,
                     mu= ~1+ScaledWaterYearIndex,
                     method="bootstrap")
  L$fit.var.scale <- texmex::evm(y=Flow, data=data.frame(AMAX_1stn), family=glo,
                   phi= ~1+ScaledWaterYearIndex,
                       method="bootstrap")
  L$fit.var.loc.scale <- texmex::evm(y=Flow, data=data.frame(AMAX_1stn), family=glo,
                           mu= ~1+ScaledWaterYearIndex,
                           phi= ~1+ScaledWaterYearIndex,
                           method="bootstrap")
  extra_GLOFITS[[STN[k]]] <- L
}

extra_GEVFITS <- list()
#GEV nonst fitting for non-NRFA stations
for(k in seq_along(STN)){
  if(!(STN[k] %in% AMAX_15$ID)){next}
  AMAX_1stn <- AMAX_15 %>% dplyr::filter(ID == STN[k]) %>%
    rename(Flow = AMAX) %>%
    mutate(WaterYear = lfwy(DateTime),
           WaterYearIndex = WaterYear - min(WaterYear),
           ScaledWaterYearIndex = scale(WaterYearIndex))
  L <- list()
  L$data <- AMAX_1stn
  L$fit.stat <- texmex::evm(y=Flow, data=data.frame(AMAX_1stn), family=gev,
                    method="bootstrap")
  L$fit.var.loc <- texmex::evm(y=Flow, data=data.frame(AMAX_1stn), family=gev,
                       mu= ~1+ScaledWaterYearIndex,
                       method="bootstrap")
  L$fit.var.scale <- texmex::evm(y=Flow, data=data.frame(AMAX_1stn), family=gev,
                         phi= ~1+ScaledWaterYearIndex,
                         method="bootstrap")
  L$fit.var.loc.scale <- texmex::evm(y=Flow, data=data.frame(AMAX_1stn), family=gev,
                             mu= ~1+ScaledWaterYearIndex,
                             phi= ~1+ScaledWaterYearIndex,
                             method="bootstrap")
  extra_GEVFITS[[STN[k]]] <- L
}


#### Determining RP of events based on model fit ####
# GLO RP computation
for (k in seq_along(STN)) {
  ke_1stn <- KeyEvents %>% dplyr::filter(`Gauge ID` == STN[k])
  n_id <- ke_1stn$`NRFA ID`[1]
  w <- which(names(GLOfits) == n_id)
  if(length(w) < 1){next}
  DAT <- GLOfits[[w]]$data
  for (fit in c("fit.stat",
                "fit.var.loc",
                "fit.var.scale",
                "fit.var.loc.scale")) {
    coeff <- GLOfits[[w]][[fit]]$map$coefficients
    q_fn <- GLOfits[[w]][[fit]]$map$family$prob
    covar <- GLOfits[[w]][[fit]]$map$data$D
    mu <- covar$mu %*% coeff[str_starts(names(coeff), "mu")]
    sigma <- covar$phi %*% coeff[str_starts(names(coeff), "phi")]
    xi <- covar$xi %*% coeff[str_starts(names(coeff), "xi")]
    coeff_table <- data.frame(mu=mu, sigma=sigma, xi=xi)
    y <- sapply(year(ke_1stn$EventDate), 
                \(z){max(which(DAT$WaterYear ==z))})
    if(any(y<0)){print(paste(k,fit))}
    Q_T <- q_fn(
      x = ke_1stn$EventQ, coeff_table[y, , drop=F], model = "GLO")
    if (sum(!is.na(Q_T))==nrow(ke_1stn)) KeyEvents[ke_1stn$rowNo, fit] <- 1-Q_T
  }
}
readr::write_csv(KeyEvents, GLO_RP_out)

# GEV RP computation
for (k in seq_along(STN)) {
  ke_1stn <- KeyEvents %>% dplyr::filter(`Gauge ID` == STN[k])
  n_id <- ke_1stn$`NRFA ID`[1]
  w <- which(names(GEVfits) == n_id)
  if(length(w) < 1){next}
  DAT <- GEVfits[[w]]$data
  for (fit in c("fit.stat",
                "fit.var.loc",
                "fit.var.scale",
                "fit.var.loc.scale")) {
    coeff <- GEVfits[[w]][[fit]]$map$coefficients
    q_fn <- GEVfits[[w]][[fit]]$map$family$prob
    covar <- GEVfits[[w]][[fit]]$map$data$D
    mu <- covar$mu %*% coeff[str_starts(names(coeff), "mu")]
    sigma <- covar$phi %*% coeff[str_starts(names(coeff), "phi")]
    xi <- covar$xi %*% coeff[str_starts(names(coeff), "xi")]
    coeff_table <- data.frame(mu=mu, sigma=sigma, xi=xi)
    y <- sapply(year(ke_1stn$EventDate), 
                \(z){max(which(DAT$WaterYear ==z))})
    if(any(y<0)){print(paste(k,fit))}
    Q_T <- q_fn(
      x = ke_1stn$EventQ, coeff_table[y, , drop=F], model = "GEV")
    if (sum(!is.na(Q_T))==nrow(ke_1stn)) KeyEvents[ke_1stn$rowNo, fit] <- 1-Q_T
  }
}

readr::write_csv(KeyEvents, GEV_RP_out)






# Flood frequency analysis (physical covariates) ------------
# Run the fitting function
detrend.cov <- "ScaledWaterYearIndex"
option <- 2
best_fit_method <- "BIC"

# Plotting function fixed for WinterFloods Project
PlotFit <- function (dist, fit = fit_results, ret_periods, all_data_raw, 
          CI, gaugeNumber, gaugeName, detrend.cov) 
{
  dist <- fit$model$map$family$name
  covs <- fit$model$map$formulae
  loc <- Reduce(paste, deparse(covs$mu))
  scale <- Reduce(paste, deparse(covs$phi))
  fitType <- ifelse(loc == "~1" & scale == "~1", 
                    "Stationary", "Non-stationary")
  covsInfo <- paste0("Location: ", as.character(loc), 
                     "; Scale: ", as.character(scale))
  all_data <- all_data_raw[, !names(all_data_raw) %in% c("WaterYear", 
                                                         "WaterYearIndex")]
  covariateData <- all_data[, !names(all_data) %in% "Flow"]
  regressors <- names(covariateData)
  if (!is.null(detrend.cov)) {
    all_data <- detrend.fn(all_data, detrend.cov, regressors)
  }
  all_data$ScaledWaterYearIndex <- as.numeric(all_data$ScaledWaterYearIndex)
  covDataReshaped <- melt(all_data, id.var = "Flow")
  plot1Title <- ifelse(nchar(gaugeName) > 0,
                       paste0("Gauging station ", gaugeNumber, " (", gaugeName, ") \n Correlations between flow and covariate data"), 
                       paste0("Gauging station ", gaugeNumber, " \n Correlations between flow and covariate data"))
  f <- ggplot(covDataReshaped, aes(x = Flow, y = value)) + 
    geom_point(aes(color = variable)) +
    facet_grid(variable ~ ., scales = "free_y") +
    theme(legend.position = "none") + 
    labs(x = expression(paste("Flow (", m^3, "/s)")), 
         y = "Value") +
    ggtitle(plot1Title) +
    theme(plot.title = element_text(hjust = 0.5))
  if (length(ret_periods) > 8) 
    print("Only the first eight return periods will be plotted in the 'model results' plot.")
  lastRP <- min(length(ret_periods), 8)
  ret_periods_sub = ret_periods[1:lastRP]
  statRLsData <- data.frame(RP = fit$statRLs$RP, RL = fit$statRLs$Estimate)
  statRLsData <- statRLsData[which(word(statRLsData$RP, 1, 
                                        sep = "-") %in% ret_periods_sub), ]
  if (fit$condRLs[1, 1] == "The selected model is stationary. Conditional return levels are not available for stationary models.") {
    statRLsData$RP2 <- as.numeric(word(statRLsData$RP, 1, 
                                       sep = "-"))
    statRLsData$RP <- with(statRLsData, reorder(RP, RP2))
    plot2Title <- ifelse(nchar(gaugeName) > 0,
                         paste0("Gauging station ", gaugeNumber, " (", gaugeName, ") (",dist, ") \n Selected model: ", covsInfo, " \n Stationary flow estimates"), 
                         paste0("Gauging station ", gaugeNumber, " (", dist, ") \n Selected model: ", covsInfo, " \n Stationary flow estimates"))
    g <- ggplot() +
      geom_segment(data = all_data_raw, aes(x = WaterYear, y = Flow, yend = 0, xend = WaterYear), size = 2) + 
      geom_hline(data = statRLsData, aes(yintercept = RL, color = RP), size = 1, linetype = "dashed") + 
      scale_colour_brewer(name = "Return Period", palette = "Dark2") + 
      labs(x = "Water year", y = expression(paste("Flow (", m^3, "/s)"))) + 
      ggtitle(plot2Title) +
      theme(plot.title = element_text(hjust = 0.5))
  }
  else {
    CRLsData = fit$condRLs
    CRLsData <- CRLsData[, grep("CI", names(CRLsData), invert = TRUE)]
    CRLsData <- CRLsData[, grep("Year|year", names(CRLsData),  value = TRUE)]
    CRLsData <- CRLsData[, grep("Index", names(CRLsData), value = TRUE, invert = TRUE)]
    condRLsData <- melt(CRLsData, id.var = "WaterYear")
    names(condRLsData)[2] <- "RP"
    condRLsData <- 
      condRLsData[which(word(condRLsData$RP, 1, sep = "-") %in% ret_periods_sub), ]
    plot2Title <- ifelse(nchar(gaugeName) > 0,
        paste0("Gauging station ", gaugeNumber, " (", gaugeName, ") (", dist, ") \n Selected model: ", covsInfo, " \n Stationary and conditional flow estimates"), 
        paste0("Gauging station ", gaugeNumber, " (", dist, ") \n Selected model: ", covsInfo, " \n Stationary and conditional flow estimates"))
    g <- ggplot() + 
      geom_segment(data = fit$condRLs, aes(x = WaterYear, y = Flow, yend = 0, xend = WaterYear), size = 2) + 
      geom_point(data = condRLsData, aes(x = WaterYear, y = value, color = RP)) +
      geom_hline(data = statRLsData,  aes(yintercept = RL, color = RP), size = 1, linetype = "dashed") + 
      scale_colour_brewer(name = "Return Period", palette = "Dark2") +
      labs(x = "Water year", y = expression(paste("Flow (", m^3, "/s)"))) + 
      ggtitle(plot2Title) +
      theme(plot.title = element_text(hjust = 0.5))
  }
  FlowData <- data.frame(Dummy = rep(0, length(fit$model$map$data$y)), 
                         Flow = fit$model$map$data$y)
  N <- length(fit$model$map$data$y)
  plot3Title <- ifelse(nchar(gaugeName) > 0, 
    paste0("Gauging station ", gaugeNumber, " (", gaugeName, ") (", dist, ") \n Selected model: ", covsInfo, " \n Encounter probability plot"), 
    paste0("Gauging station ", gaugeNumber, " (", dist, ") \n Selected model: ", covsInfo, " \n Encounter probability plot"))
  if (fit$margRLs[1, 1] == "The selected model is stationary. Marginal return levels are not available for stationary models.") {
    statRLs = fit$statRLs
    names(statRLs)[2:4] <- c("StatLCI", "StatEstimate", "StatUCI")
    statRLs$RPnum <- as.numeric(word(statRLs$RP, 1, sep = fixed("-")))
    statRLs <- statRLs[order(statRLs$RPnum), ]
    statRLs$EP <- 1 - (1 - 1/as.numeric(statRLs$RPnum))^N
    statRLsData <- melt(statRLs[, which(!names(statRLs) %in% 
                                          c("RP", "RPnum"))], id.var = "EP")
    statRLsData$variable2 <- as.character(statRLsData$variable)
    statRLsData$variable2[statRLsData$variable2 %in% c("StatLCI", 
                                                       "StatUCI")] <- paste0(CI, "% confidence interval")
    statRLsData$variable2 <- as.factor(statRLsData$variable2)
    statRLsData$variable2 <- factor(statRLsData$variable2, 
                                    levels(statRLsData$variable2)[c(3, 2, 1)])
    h <- ggplot() +
      geom_line(data = statRLsData, aes(x = EP, y = value, size = variable, linetype = variable2)) + 
      labs(x = paste0("Encounter probability over the ", N, "-year record period"),
           y = expression(paste("Flow (", m^3, "/s)"))) +
      geom_rug(data = FlowData, aes(y = Flow)) +
      scale_size_manual(values = c(1, 1.5, 1)) +
      theme(legend.position = "none") + 
      scale_linetype_manual(values = c("solid", "dashed"), 
                            labels = c("Stationary flow estimates", 
                                       paste0(CI, "% confidence interval for the stationary estimates"))) + 
      theme(legend.position = "bottom", legend.direction = "vertical", legend.title = element_blank()) +
      guides(size = "none") + 
      ggtitle(plot3Title) +
      theme(plot.title = element_text(hjust = 0.5))
  }
  else {
    margRLs <- fit$margRLs
    names(margRLs)[2:4] <- c("MargLCI", "MargEstimate", "MargUCI")
    statRLs <- data.frame(RP = fit$statRLs$RP, RL = fit$statRLs$Estimate)
    names(statRLs)[2] <- "StatEstimate"
    statMarg <- merge(statRLs, margRLs, by = "RP")
    statMarg$RPnum <- as.numeric(word(statMarg$RP, 1, sep = fixed("-")))
    statMarg <- statMarg[order(statMarg$RPnum), ]
    statMarg$EP <- 1 - (1 - 1/as.numeric(statMarg$RPnum))^N
    statMargData <- melt(statMarg[, which(!names(statMarg) %in% 
                                            c("RP", "RPnum"))], id.var = "EP")
    statMargData$variable2 <- as.character(statMargData$variable)
    statMargData$variable2[statMargData$variable2 %in% c("MargLCI", 
                                                         "MargUCI")] <- paste0(CI, "% confidence interval")
    statMargData$variable2 <- as.factor(statMargData$variable2)
    statMargData$variable2 <- factor(statMargData$variable2, 
                                     levels(statMargData$variable2)[c(3, 2, 1)])
    h <- ggplot() +
      geom_line(data = statMargData, aes(x = EP, y = value, size = variable, colour = variable2, linetype = variable2)) + 
      labs(x = paste0("Encounter probability over the ", N, "-year record period"),
           y = expression(paste("Flow (", m^3, "/s)"))) +
      geom_rug(data = FlowData, aes(y = Flow)) +
      scale_colour_manual(values = c("#000000", "#0072B2", "#0072B2"),
                          labels = c("Stationary flow estimates", "Integrated flow estimates (non-stationary)", paste0(CI, "% confidence interval for the non-stationary estimates"))) + 
      scale_size_manual(values = c(1.5, 1, 1.5, 1)) +
      theme(legend.position = "none") + 
      scale_linetype_manual(values = c("solid", "solid", "dashed"),
                            labels = c("Stationary flow estimates", "Integrated flow estimates (non-stationary)", paste0(CI, "% confidence interval for the non-stationary estimates"))) + 
      theme(legend.position = "bottom", legend.direction = "vertical", legend.title = element_blank()) +
      guides(size = "none") + 
      ggtitle(plot3Title) + 
      theme(plot.title = element_text(hjust = 0.5))
  }
  output <- list(f = f, g = g, h = h)
  return(output)
}

physCovModelFits <- fit.phys.cov(dist, folder, detrend.cov,
                                   option, best_fit_method)
# Obtain return levels based on one of the fitted models
for(gauge in c(25009, 27009, 27029, 27034, 28060, 33034, 39014, 39089, 47004)){
  gauge <- as.character(gauge)
  RPs <- c(2, 10, 20, 50, 100)
  res.phys.cov(folder, physCovModelFits, gauge, RPs, CI,
                 model_type = "Best", locFit = NULL, scaleFit = NULL,
                 present_day = TRUE)
}
# # End of complete example script to run all functions 