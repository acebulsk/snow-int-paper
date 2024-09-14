# define global attributes

library(tidyverse)
library(ggpubr)
library(cowplot)
library(plotly)
library(abind)
library(terra)
library(modelr)
library(gt)

source('../../analysis/lidar-processing/scripts/voxrs/voxrs_helper_fns.R')
source('../../analysis/disdrometer/scripts/00_source_functions.R')

scl_names_dict <- data.frame(
  name = c('sparse_forest', 'medium_density_forest', 'dense_forest'),
  scl_names_new = factor(c('Sparse', 'Mixed', 'Closed'), levels = c(c('Sparse', 'Mixed', 'Closed')))
)

met_intercept <- readRDS('../../analysis/interception/data/storm_analysis/continuous_throughfall_data_binned_met_select_events.rds')  |>
  filter(q_sf > 0,
         d_tf > 0.01,
    # u <= 2,
    q_sf > q_tf) |> # if troughs > q_sf may be some unloading
  mutate(
    q_int = q_sf - q_tf,
    IP = q_int / q_sf) |>
  filter(IP < 1) |>
  left_join(scl_names_dict)

calg_mag_declination <- 13.5 # deg + east in 2020 https://www.ngdc.noaa.gov/geomag/magfield-wist/

parsivel <- readRDS('../../analysis/disdrometer/data/disdro_spectrum_processed_agg_15_min.RDS')

ffr_met <- readRDS('../../analysis/met-data-processing/data/ffr_crhm_modelling_obs.rds')
ffr_met_wnd <- readRDS('../../analysis/met-data-processing/data/ffr_t_rh_u_qaqc_fill.rds')
# not enough ec wind obs over the event
ffr_ec <- readRDS('../../analysis/eddy-cov/data/high-tower/ec_high_tower_30_min_2021_2023_qc_rough.rds') |>
  mutate(ec_wind_dir = wind_dir_mag - calg_mag_declination) |>
  select(datetime, ec_wind_speed = wind_speed, ec_wind_dir)
# ffr_ec <- readRDS('../../analysis/eddy-cov/data/low-tower/low_tower_15min_2021_2023_qc_rough.rds') |>
#   mutate(ec_wind_dir = wind_dir_mag - calg_mag_declination) |>
#   select(datetime, ec_wind_speed = wind_speed, ec_wind_dir)
pwl_sf <- readRDS('../../analysis/met-data-processing/data/pluvio-qaqc/pwl_pluvio_15_min_qaqc_undercatch_corr_ac.rds')
pwl_wind <- readRDS('../../analysis/met-data-processing/data/pwl_met_qaqc.rds') |>
  select(
    datetime,
    wind_speed = WindSpeed_S_WVT,
    wind_dir_true = WindDir_D1_WVT # keir confirmed junction box is pointed at 180 deg south (true) as per spec
    # sd_wind_dir = WindDir_SD1_WVT
  )

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
w_tree_ax_lab <- "Initial Canopy Snow Load (kg m⁻²)"

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

