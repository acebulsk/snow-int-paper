# combine the met plots with the fsd forest structure plots

source('scripts/00_define_global_attributes.R')
source('scripts/01_plot_binned_met_trough_IP.R')
# source('scripts/01_plot_forest_snow_survey_IP.R')
source('scripts/01_plot_parsivel_IP.R')


cowplot::plot_grid(
  # le_ip,
  at_ip,
  rh_ip,
  ws_ip,
  w_ip,
  diam_ip,
  vel_ip,
  nrow = 3,
  ncol = 2,
  labels = 'AUTO'
)

ggsave('figs/automated_snowfall_event_periods/troughs_met_vs_IP_bin.png', device = png, width = 8.5, height = 11)
