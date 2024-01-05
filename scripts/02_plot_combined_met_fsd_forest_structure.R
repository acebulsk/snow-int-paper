# combine the met plots with the fsd forest structure plots

cowplot::plot_grid(
  lai_ip_boxplot,
  at_ip,
  ws_ip,
  w_ip,
  # ip_vs_hydro_type,
  nrow = 4,
  labels = 'AUTO'
)

ggsave('figs/interception/troughs_met_LAI_vs_IP_bin.png', device = png, width = 8.5, height = 11)
