# script to average met data over snow survey periods
source('scripts/00_define_global_attributes.R')
source('scripts/fsd_snow_survey/00_load_snow_survey_data.R')

# average over events
ffr_met_avg_event <- met_intercept |>
  group_by(storm_id) |>
  mutate(event_cml_sf = cumsum(p)) |>
  summarise(
    del_sf = sum(p),
    del_i = sum(q_int)/4,
    mean_u = mean(u),
    med_u = median(u),
    stdev_u = sd(u),
    initial_canopy_snow_load = weighed_tree_canopy_load_mm[which.min(event_cml_sf)],
    across(c(t:p, part_diam, part_vel, IP), \(x) mean(x,
                                                                na.rm = TRUE))
  )

# plot the average I/P over the event vs various met conditions ----

## can do this using the cumulative i over cumulative p

ffr_met_avg_event |>
  filter(initial_canopy_snow_load < 5,
         u < 3,
         t < -3) |>
  ggplot(aes(del_sf, del_i/del_sf, colour = t)) +
  geom_point() +
  scale_color_viridis_c(option = 'magma', name = 'Air Temperature (°C)')

## can use the average i over cumulative p (shows similar results )

ffr_met_avg_event |>
  filter(initial_canopy_snow_load < 5,
         u < 3,
         t < -3) |>
ggplot(aes(del_sf, IP, colour = t)) +
  geom_point() +
  scale_color_viridis_c(option = 'magma', name = 'Air Temperature (°C)')

ffr_met_avg_event |>
  filter(initial_canopy_snow_load < 5,
         u < 2,
         t < -3) |>
  ggplot(aes(del_sf, IP, colour = u)) +
  geom_point() +
  scale_color_viridis_c(option = 'magma', name = 'Wind Speed (m/s)')

ffr_met_avg_event |>
  filter(initial_canopy_snow_load < 5,
         u < 2,
         t < -3) |>
  ggplot(aes(del_sf, del_i/del_sf)) +
  geom_point() +
  ylab('Interception Efficiency (-)') +
  xlab('Event Snowfall (mm)')

ggsave('figs/automated_snowfall_event_periods/event_total_snowfall_vs_IP.png', width = 4, height = 4)


ffr_met_avg_event |>
  filter(initial_canopy_snow_load < 5,
         u < 2) |>
  ggplot(aes(t, del_i/del_sf)) +
  geom_point() +
  ylab('Interception Efficiency (-)') +
  xlab('Air Temperature (°C)')

ggsave('figs/automated_snowfall_event_periods/event_avg_temp_vs_IP.png', width = 4, height = 4)


ffr_met_avg_event |>
  filter(initial_canopy_snow_load < 5,
         t < -5) |>
  ggplot(aes(u, IP)) +
  geom_point() +
  ylab('Interception Efficiency (-)') +
  xlab('Wind Speed (m/s)')

ggsave('figs/automated_snowfall_event_periods/event_avg_wind_vs_IP.png', width = 4, height = 4)

