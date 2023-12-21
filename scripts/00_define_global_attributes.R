# define global attributes

library(tidyverse)
library(ggpubr)
library(cowplot)
library(plotly)

# theme_bw(base_size = 14)
options(ggplot2.discrete.colour= c("black", "#56B4E9", "#E69F00"))

theory_colours <- c("#30123BFF", "#1AE4B6FF", "#FABA39FF", "#7A0403FF") # viridis::turbo(4)

fig_width <- 5.5
fig_height <- 4

wnd_bin_ax_lab <- "Wind Speed bin (m/s)"
temp_bin_ax_lab <- 'Air Temperature bin (°C)'
ip_y_ax_lab <- 'Interception Efficiency (-)'
temp_ax_lab <- 'Air Temperature (°C)'
wind_ax_lab <- 'Wind Speed (m/s)'
