
#### FUNCTIONS ####
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

dfilter <- dplyr::filter