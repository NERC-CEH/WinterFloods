### giaves 2023-10-25
# EA project 35752: Hydrological analysis of the 2019-2021 flooding

# Version 1.0: 2024-07-22. Final version for wider distribution.

# function file for 4-point spatial interpolation of return levels.


# x_ra = duration or return period
# y_ra = depth

# x_intp_r = requested value

flogi <- function(xx_r) {
  #logit transform
  1/(1 + exp(-xx_r))
}

flogim <- function(xx_r) {
  #inverse logit transform
  log(xx_r/(1 - xx_r))
}

# Scale x and y so both on a range of 0 to 1

I4PLA <- function(x_ra, y_ra, x_intp_r) {
  # 4-point interpolation of input data given depth, duration and grid to interpolate
  # x_ra      vector of duration or return period at bounding corners
  # y_ra      vector of depth (mm) at bounding corners
  # x_intp_r  requested value (return level) to interpolate to
  
  x_range_r <- x_ra[4] - x_ra[1]
  y_range_r <- y_ra[4] - y_ra[1]

  #scale to 0-1 scale
  x_s_ra <- 0
  y_s_ra <- 0
  for (i in 1:4) {
    x_s_ra[i] <- (x_ra[i] - x_ra[1])/x_range_r
    y_s_ra[i] <- (y_ra[i] - y_ra[1])/y_range_r
  }

  x_s_intp_r <- (x_intp_r - x_ra[1])/x_range_r

  # Interpolate to -inf to inf scale
  flm_x2_r <- flogim(x_s_ra[2])
  flm_y2_r <- flogim(y_s_ra[2])
  flm_x3_r <- flogim(x_s_ra[3])
  flm_y3_r <- flogim(y_s_ra[3])
  flm_x3m2_r <- flm_x3_r - flm_x2_r

  disk_r <- (flm_y3_r - flm_y2_r)/flm_x3m2_r
  disa_r <- (flm_x3_r*flm_y2_r - flm_x2_r*flm_y3_r)/flm_x3m2_r

  #final value with inverse logit transform
  y_range_r*flogi(disa_r + disk_r*flogim(x_s_intp_r)) + y_ra[1]
}