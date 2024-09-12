# plot wind rose for each fsd event period

source('scripts/lidar_snow_survey/01_plot_lidar_met_time_series.R')

# wind rose plots

met_w_snowfall <- ffr_met_wnd_lidar_events |>
  filter(ppt > 0.1)

marmet <- ffr_met_wnd_lidar_events |>
  filter(event_id == '2023-03-14') |>
  pivot_longer(c(ft_wind_speed, pwl_wind_speed))
ggplot(marmet, aes(datetime, value, colour = name)) + geom_line()

ffr_met_wnd_lidar_events_snowing <- met_w_snowfall |>
  group_by(event_id) |>
  mutate(event_cml_sf = cumsum(ppt),
         ppt = ppt*4) |>
  group_by(event_id) |>
  summarise(
    `Air Temp. (°C)` = mean(air_temp),
    `RH (%)` = mean(rh),
    `med FT Wind Dir. (°)` = median(ft_wind_dir),
    `med FT Wind Speed (m/s)` = median(ft_wind_speed),
    `med PWL Wind Dir. (°)` = median(pwl_wind_dir),
    `med PWL Wind Speed (m/s)` = median(pwl_wind_speed),
    `mean FT Wind Dir. (°)` = mean(ft_wind_dir),
    `mean FT Wind Speed (m/s)` = mean(ft_wind_speed),
    `mean PWL Wind Dir. (°)` = mean(pwl_wind_dir),
    `mean PWL Wind Speed (m/s)` = mean(pwl_wind_speed))

saveRDS(ffr_met_wnd_lidar_events_snowing, 'data/event_met/lidar_events_met_avgs_snowing.rds')

for (event in as.character(scan_dates$event_id)) {
  ffr_met_wnd_event <- met_w_snowfall[as.character(met_w_snowfall$event_id) == event, ]

  p <- weatherdash::wind_rose(ffr_met_wnd_event,
                              'datetime',
                              'ft_wind_speed',
                              'ft_wind_dir',
                              dir_res = 30,
                              ws_res = .5,
                              ws_max = 5,
                              # plot_title = 'FT',
                              spd_unit = 'm/s'
  )

  p

  plotly::save_image(p, paste0('figs/lidar_periods/wind_rose/png/ft_wind_rose_event_snowing', event, '.png'))

  p <- weatherdash::wind_rose(ffr_met_wnd_event,
                              'datetime',
                              'pwl_wind_speed',
                              'pwl_wind_dir',
                              dir_res = 30,
                              ws_res = .5,
                              ws_max = 5,
                              # plot_title = 'PWL',
                              spd_unit = 'm/s'
  )

  p

  plotly::save_image(p, paste0('figs/lidar_periods/wind_rose/png/pwl_wind_rose_event_snowing', event, '.png'))
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
