library(nonstat)

folder <- "P:/08458 CWI-EA 2019-21 Flood Review/Code/NonstationaryFFA/Data/NONST"
dist <- "GEV"
RPs <- c(2, 10, 20, 50, 100)
CI <- 90

KeyEvents <- readr::read_csv(
  "P:/08458 CWI-EA 2019-21 Flood Review/Data/KeyDetails/KeyDetails_Long.csv")

KeyDetails <- readr::read_csv("P:/08458 CWI-EA 2019-21 Flood Review/Data/MSL_post queries.csv")

KeyEvents <- KeyEvents %>% 
  dplyr::filter(str_starts(`Non-stationary probability analysis (physical covariates)`, "Y"))

KeyEvents$best_model <- NA
KeyEvents$stat_model <- NA
KeyEvents$rowNo <- 1:nrow(KeyEvents)
STN <- unique(KeyEvents$`Gauge ID`)

# extra_GLOFITS <- list()

library(texmex)

# Flood frequency analysis (physical covariates) ------------
# Run the fitting function
detrend.cov <- "ScaledWaterYearIndex"
option <- 2
best_fit_method <- "BIC"

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
  plot1Title <- ifelse(nchar(gaugeName) > 0, paste0("Gauging station ", 
                                                    gaugeNumber, " (", gaugeName, ") \n Correlations between flow and covariate data"), 
                       paste0("Gauging station ", gaugeNumber, " \n Correlations between flow and covariate data"))
  f <- ggplot(covDataReshaped, aes(x = Flow, y = value)) + 
    geom_point(aes(color = variable)) + facet_grid(variable ~ 
                                                     ., scales = "free_y") + theme(legend.position = "none") + 
    labs(x = expression(paste("Flow (", m^3, "/s)")), 
         y = "Value") + ggtitle(plot1Title) + theme(plot.title = element_text(hjust = 0.5))
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
    plot2Title <- ifelse(nchar(gaugeName) > 0, paste0("Gauging station ", 
                                                      gaugeNumber, " (", gaugeName, ") (", 
                                                      dist, ") \n Selected model: ", covsInfo, " \n Stationary flow estimates"), 
                         paste0("Gauging station ", gaugeNumber, " (", 
                                dist, ") \n Selected model: ", covsInfo, 
                                " \n Stationary flow estimates"))
    g <- ggplot() + geom_segment(data = all_data_raw, aes(x = WaterYear, 
                                                          y = Flow, yend = 0, xend = WaterYear), size = 2) + 
      geom_hline(data = statRLsData, aes(yintercept = RL, 
                                         color = RP), size = 1, linetype = "dashed") + 
      scale_colour_brewer(name = "Return Period", 
                          palette = "Dark2") + labs(x = "Water year", 
                                                    y = expression(paste("Flow (", m^3, "/s)"))) + 
      ggtitle(plot2Title) + theme(plot.title = element_text(hjust = 0.5))
  }
  else {
    CRLsData = fit$condRLs
    CRLsData <- CRLsData[, grep("CI", names(CRLsData), 
                                invert = TRUE)]
    CRLsData <- CRLsData[, grep("Year|year", names(CRLsData), 
                                value = TRUE)]
    CRLsData <- CRLsData[, grep("Index", names(CRLsData), 
                                value = TRUE, invert = TRUE)]
    condRLsData <- melt(CRLsData, id.var = "WaterYear")
    names(condRLsData)[2] <- "RP"
    condRLsData <- condRLsData[which(word(condRLsData$RP, 
                                          1, sep = "-") %in% ret_periods_sub), ]
    plot2Title <- ifelse(nchar(gaugeName) > 0, paste0("Gauging station ", 
                                                      gaugeNumber, " (", gaugeName, ") (", 
                                                      dist, ") \n Selected model: ", covsInfo, " \n Stationary and conditional flow estimates"), 
                         paste0("Gauging station ", gaugeNumber, " (", 
                                dist, ") \n Selected model: ", covsInfo, 
                                " \n Stationary and conditional flow estimates"))
    g <- ggplot() + geom_segment(data = fit$condRLs, aes(x = WaterYear, 
                                                         y = Flow, yend = 0, xend = WaterYear), size = 2) + 
      geom_point(data = condRLsData, aes(x = WaterYear, 
                                         y = value, color = RP)) + geom_hline(data = statRLsData, 
                                                                              aes(yintercept = RL, color = RP), size = 1, linetype = "dashed") + 
      scale_colour_brewer(name = "Return Period", 
                          palette = "Dark2") + labs(x = "Water year", 
                                                    y = expression(paste("Flow (", m^3, "/s)"))) + 
      ggtitle(plot2Title) + theme(plot.title = element_text(hjust = 0.5))
  }
  FlowData <- data.frame(Dummy = rep(0, length(fit$model$map$data$y)), 
                         Flow = fit$model$map$data$y)
  N <- length(fit$model$map$data$y)
  plot3Title <- ifelse(nchar(gaugeName) > 0, paste0("Gauging station ", 
                                                    gaugeNumber, " (", gaugeName, ") (", dist, 
                                                    ") \n Selected model: ", covsInfo, " \n Encounter probability plot"), 
                       paste0("Gauging station ", gaugeNumber, " (", 
                              dist, ") \n Selected model: ", covsInfo, " \n Encounter probability plot"))
  if (fit$margRLs[1, 1] == "The selected model is stationary. Marginal return levels are not available for stationary models.") {
    statRLs = fit$statRLs
    names(statRLs)[2:4] <- c("StatLCI", "StatEstimate", 
                             "StatUCI")
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
    h <- ggplot() + geom_line(data = statRLsData, aes(x = EP, 
                                                      y = value, size = variable, linetype = variable2)) + 
      labs(x = paste0("Encounter probability over the ", 
                      N, "-year record period"), y = expression(paste("Flow (", 
                                                                      m^3, "/s)"))) + geom_rug(data = FlowData, 
                                                                                               aes(y = Flow)) + scale_size_manual(values = c(1, 
                                                                                                                                             1.5, 1)) + theme(legend.position = "none") + 
      scale_linetype_manual(values = c("solid", "dashed"), 
                            labels = c("Stationary flow estimates", 
                                       paste0(CI, "% confidence interval for the stationary estimates"))) + 
      theme(legend.position = "bottom", legend.direction = "vertical", 
            legend.title = element_blank()) + guides(size = "none") + 
      ggtitle(plot3Title) + theme(plot.title = element_text(hjust = 0.5))
  }
  else {
    margRLs <- fit$margRLs
    names(margRLs)[2:4] <- c("MargLCI", "MargEstimate", 
                             "MargUCI")
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
    h <- ggplot() + geom_line(data = statMargData, aes(x = EP, 
                                                       y = value, size = variable, colour = variable2, linetype = variable2)) + 
      labs(x = paste0("Encounter probability over the ", 
                      N, "-year record period"), y = expression(paste("Flow (", 
                                                                      m^3, "/s)"))) + geom_rug(data = FlowData, 
                                                                                               aes(y = Flow)) + scale_colour_manual(values = c("#000000", 
                                                                                                                                               "#0072B2", "#0072B2"), labels = c("Stationary flow estimates", 
                                                                                                                                                                                 "Integrated flow estimates (non-stationary)", 
                                                                                                                                                                                 paste0(CI, "% confidence interval for the non-stationary estimates"))) + 
      scale_size_manual(values = c(1.5, 1, 1.5, 1)) + theme(legend.position = "none") + 
      scale_linetype_manual(values = c("solid", "solid", 
                                       "dashed"), labels = c("Stationary flow estimates", 
                                                             "Integrated flow estimates (non-stationary)", 
                                                             paste0(CI, "% confidence interval for the non-stationary estimates"))) + 
      theme(legend.position = "bottom", legend.direction = "vertical", 
            legend.title = element_blank()) + guides(size = "none") + 
      ggtitle(plot3Title) + theme(plot.title = element_text(hjust = 0.5))
  }
  output <- list(f = f, g = g, h = h)
  return(output)
}

physCovModelFits <- fit.phys.cov(dist="GLO", folder, detrend.cov,
                                   option, best_fit_method)
STN <- c(25009, 27009, 27029, 27034, 28060, 33034, 39014, 39089, 47004, 53004, 68001, 69023, 70004, 71009, 72005, 73005, 73009)
# Obtain return levels based on one of the fitted models
for(gauge in STN){
  gauge <- as.character(gauge)
  RPs <- c(2, 10, 20, 50, 100)
  res.phys.cov(folder, physCovModelFits, gauge, RPs, CI,
                 model_type = "Best", locFit = NULL, scaleFit = NULL,
                 present_day = TRUE)
}

for (k in seq_along(STN)) {
  #print(paste(k, STN[k]))
  ke_1stn <- KeyEvents %>% dplyr::filter(`NRFA ID` == STN[k])
  n_id <- KeyDetails$`NRFA ID`[KeyDetails$`NRFA ID` == STN[k]]
  w <- which(names(physCovModelFits) == n_id)
  if(length(w) < 1){next}
  DAT <- physCovModelFits[[w]]$all_data_raw
  for (fit in c("best_model", "stat_model")) {
    #print(paste("***", fit))
    coeff <- physCovModelFits[[w]]$model_fits[[fit]]$res$map$coefficients
    q_fn <- physCovModelFits[[w]]$model_fits[[fit]]$res$map$family$prob
    covar <- physCovModelFits[[w]]$model_fits[[fit]]$res$map$data$D
    
    mu <- covar$mu %*% coeff[str_starts(names(coeff), "mu")]
    sigma <- covar$phi %*% coeff[str_starts(names(coeff), "phi")]
    xi <- covar$xi %*% coeff[str_starts(names(coeff), "xi")]
    coeff_table <- data.frame(mu=mu, sigma=sigma, xi=xi)
    
    y <- sapply(year(ke_1stn$EventDate), 
                \(z){max(which(DAT$WaterYear ==z))})
    if(any(y<0)){
      print(paste(k,fit))
    }
    Q_T <- q_fn(
      x = ke_1stn$EventQ, coeff_table[y, , drop=F], model = "GLO")
    #if (sum(!is.na(Q_T))==nrow(ke_1stn)) 
    KeyEvents[ke_1stn$rowNo, fit] <- 1-Q_T
  }
}

write_csv(KeyEvents, "./Outputs/KeyEvents_PhysCov_AEP_GLO.csv")
# # End of complete example script to run all functions 