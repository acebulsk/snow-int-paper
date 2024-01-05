# define global attributes

library(tidyverse)
library(ggpubr)
library(cowplot)
library(plotly)

# theme_bw(base_size = 14)
options(ggplot2.discrete.colour= palette.colors(palette = "R4"))

theory_colours <- c("#30123BFF", "#1AE4B6FF", "#FABA39FF", "#7A0403FF") # viridis::turbo(4)

fig_width <- 5.5
fig_height <- 4

wnd_bin_ax_lab <- "Wind Speed bin (m/s)"
temp_bin_ax_lab <- 'Air Temperature bin (°C)'
ip_y_ax_lab <- 'Interception Efficiency (-)'
temp_ax_lab <- 'Air Temperature (°C)'
wind_ax_lab <- 'Wind Speed (m/s)'
diam_ax_lab <- 'Hydrometeor Diameter (mm)'
vel_ax_lab <- 'Hydrometeor Velocity (m/s)'

label_bin_fn <- function(bins){
  (bins[-1] + bins[-length(bins)]) / 2
}

give.n <- function(x){
  return(c(y = median(x)*1.05, label = length(x)))
  # experiment with the multiplier to find the perfect position
}
