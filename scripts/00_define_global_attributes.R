# define global attributes

library(tidyverse)
library(ggpubr)
library(cowplot)
library(plotly)
library(abind)
library(terra)
library(modelr)
library(gt)

# source('../../analysis/lidar-processing/scripts/voxrs/voxrs_helper_fns.R')
source('../../analysis/disdrometer/scripts/00_source_functions.R')
perc_err_fltr <- 100 # percent error above which is unnacceptible to apply after accumulation period

scl_names_dict <- data.frame(
  name = c('sparse_forest', 'medium_density_forest', 'dense_forest'),
  scl_names_new = factor(c('Sparse', 'Mixed', 'Closed'), levels = c(c('Sparse', 'Mixed', 'Closed')))
)

calg_mag_declination <- 13.5 # deg + east in 2020 https://www.ngdc.noaa.gov/geomag/magfield-wist/

# theme_bw(base_size = 14)
options(ggplot2.discrete.colour= palette.colors(palette = "R4"))
options(ggplot2.discrete.fill= palette.colors(palette = "R4")[2:6])
cc_colours <- c("#9E9E9E",  "#F5C710", "#61D04F")

# [1] "#000000" "#DF536B" "#61D04F" "#2297E6" "#28E2E5" "#CD0BBC" "#F5C710"
# [8] "#9E9E9E"

fig_width <- 5.5
fig_height <- 4

wnd_bin_ax_lab <- "Wind Speed (m/s)"
temp_bin_ax_lab <- 'Air Temperature (°C)'
ip_y_ax_lab <- 'Interception Efficiency (-)'
temp_ax_lab <- 'Air Temperature (°C)'
wind_ax_lab <- 'Wind Speed (m/s)'
diam_ax_lab <- 'Hydrometeor Diameter (mm)'
vel_ax_lab <- 'Hydrometeor Velocity (m/s)'
le_ax_lab <- 'Leaf Area Index (-)'
cc_ax_lab <- 'Canopy Coverage (-)'
w_tree_ax_lab <- "Initial Canopy Snow Load (mm)"

label_bin_fn <- function(bins){
  (bins[-1] + bins[-length(bins)]) / 2
}

give.n <- function(x){
  return(c(y = median(x)*1.05, label = length(x)))
  # experiment with the multiplier to find the perfect position
}

to_long <- function(from, to, event_id){
  datetime <- seq(from, to, 900)

  out <- data.frame(datetime, event_id)

  return(out)
}

to_long_one_minute <- function(from, to, event_id){
  datetime <- seq(from, to, 60)

  out <- data.frame(datetime, event_id)

  return(out)
}

get_traj_time <- function(file, fin = T){
  origin <- as.POSIXct('1980-01-06 00:00:00', tz = 'UTC')
  traj <- read.csv(file)

  if(fin){ # i.e. grab tail
    traj_time <- tail(traj$Time.s., n = 1)+1e9
  } else { # grab head
    traj_time <- head(traj$Time.s., n = 1)+1e9
  }

  time_out <- as.POSIXct(traj_time, origin = origin, tz = 'UTC')
  time_out <- format(time_out, tz = 'Etc/GMT+6') |> as.POSIXct(tz = 'Etc/GMT+6')

  return(time_out)
}


# based on the idea that the increase in snow-leaf contact area is equal to the
# void space multiplied by the increase in leaf contact area with snowfall
# trajectory angle represented by sin(theta)
# see /home/alex/local-usask/working-papers/snow-int-paper/figs/examples/contact_area_reasoning.qmd for reasoning behind this model
sine_fn <- function(traj_angle, b,  cc){
  # traj_angle is the inclination angle from zenith of snowfall (degrees)
  # cc is the canopy coverage viewed from nadir (-)
  theta <- traj_angle * (pi/180)

  # 1-cc sets the max potential increase based on the initial void space
  # sin(theta)^2 is the increase in surface area snow can contact
  cp_inc <- b*((1-cc)*sin(theta)^2)#*1/(cos(theta))
  return(cp_inc)
}

traj_angle_deg <- function(wind_speed, velocity){
  slope <- wind_speed/velocity
  angle_deg <- atan(slope) * 180 / pi

  return(angle_deg)
}

# rearrange to return wind speed given angle and velocity
wind_speed <- function(traj_angle_deg, velocity){
  angle_rad <- traj_angle_deg * pi / 180
  wind_speed <- velocity * tan(angle_rad)

  return(wind_speed)
}

prop_err_ratio <- function(ratio, x, y, sigma_x, sigma_y){
  sigma_ratio <- ratio*sqrt((sigma_x/x)^2+(sigma_y/y)^2)
  return(sigma_ratio)
}

prop_err_sum <- function(sigma_x, sigma_y){
  sigma_sum <- sqrt(sigma_x^2 + sigma_y^2)
  return(sigma_sum)
}
