# plot wind rose for each fsd event period

source('scripts/lidar_snow_survey/01_plot_lidar_met_time_series.R')

# wind rose plots

for (event in as.character(lidar_event_periods$event_id)) {
  ffr_met_wnd_event <- ffr_met_wnd_lidar_events[as.character(ffr_met_wnd_lidar_events$event_id) == event, ]

  p <- weatherdash::wind_rose(ffr_met_wnd_event,
                              'datetime',
                              'wind_speed',
                              'wind_dir',
                              dir_res = 30,
                              ws_res = 1,
                              ws_max = 5,
                              plot_title = event
  )

  p

  # reticulate::py_install('kaleido')
  # reticulate::py_install('plotly')
  plotly::save_image(p, paste0('figs/interception/lidar_snow_survey/wind_rose/png/wind_rose_event_', event, '.png'))
  htmltools::save_html(p, paste0('figs/interception/lidar_snow_survey/wind_rose/interactive/wind_rose_event_', event, '.html'))
}
#
# ggwindRose(ffr_met_wnd_event, 'wind_speed', 'wind_dir')
#
# # alex function
# wind_rose(ffr_met_wnd_event,
#                        'datetime',
#                        'wind_speed',
#                        'wind_dir',
#                        plot_title = 'test'
# )
#
# # La función cut me da los intervalos en orden decresciente, pero
# # los necesito en orden cresciente.
# ReverseCut <- function(x, ...) {
#   f <- cut(x, ...)
#   n <- length(levels(f))
#   levels(f) <- levels(f)[n:1]
#   f
# }
#
# # Truquito: mover el dominio de la dirección a (-22.5/2, 360 - 22.5/2).
# ffr_met_wnd_event$wind_dir_new <- ifelse(ffr_met_wnd_event$wind_dir > 360 - 22.5/2, ffr_met_wnd_event$wind_dir - 360, ffr_met_wnd_event$wind_dir)
#
# # Me quedo con 1000 al azar para que no tarde tanto en plotear.
#
# ggplot(ffr_met_wnd_event, aes(wind_dir_new, fill = ReverseCut(wind_speed, breaks = 5))) +
#   geom_histogram(binwidth = 22.5, center = 0, aes(y = ..density..*100)) +
#   coord_polar(start = -22.5/2*pi/180) +    # para que el norte quede arriba
#   scale_fill_viridis_d(name = "Velocidad", direction = -1) +
#   scale_y_continuous(name = "Frecuencia",
#                      limits = c(-0.50, NA), labels = NULL) +
#   annotate(geom = "rect", ymin = -0.5, ymax = 0, xmin = 0-22.5/2, xmax =360-22.5/2,
#            fill = "white") +    # círculo blanco del centro
#   scale_x_continuous(name = "", limits = c(0 - 22.5/2, 360 - 22.5/2),
#                      breaks = seq(0, 360 - 22.5, by = 22.5),
#                      minor_breaks = NULL,
#                      labels = c("N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE",
#                                 "S", "SSO", "SO", "OSO", "O", "ONO", "NO", "NNO")) +
#   geom_text(data = data.frame(x = 0, y = seq(0, 12, by = 3)), aes(x = x, y = y, label = y),
#             inherit.aes = F) +
#   theme_minimal()
