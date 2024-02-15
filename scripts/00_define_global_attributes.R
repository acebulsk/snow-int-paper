# define global attributes

library(tidyverse)
library(ggpubr)
library(cowplot)
library(plotly)

met_intercept <- readRDS('../../analysis/interception/data/storm_analysis/continuous_throughfall_data_binned_met_select_events.rds')  |>
  filter(q_sf > 0,
    # u <= 2,
    q_sf > q_tf) |> # if troughs > q_sf may be some unloading
  mutate(
    q_int = q_sf - q_tf,
    IP = q_int / q_sf) |>
  filter(IP < 1)

parsivel <- readRDS('../../analysis/disdrometer/data/disdro_spectrum_processed_agg_15_min_202310.RDS')

ffr_met <- readRDS('../../analysis/met-data-processing/data/ffr_crhm_modelling_obs.rds')

# theme_bw(base_size = 14)
options(ggplot2.discrete.colour= palette.colors(palette = "R4"))

# "#000000" "#DF536B" "#61D04F" "#2297E6" "#28E2E5" "#CD0BBC" "#F5C710" "#9E9E9E"

# theory_colours <- c("#30123BFF", "#1AE4B6FF", "#FABA39FF", "#7A0403FF") # viridis::turbo(4)

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
w_ax_lab <- 'Initial Canopy Snow Load (mm)'

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
