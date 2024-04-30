# met data from takeoff on pre flight to landing on post flight
# may also want to just look at from snowfall start to landing on post flight

source('scripts/lidar_snow_survey/00_load_lidar_meta.R')

ffr_met_wnd_lidar_events <- lidar_events_long_dt |>
  left_join(ffr_met_wnd)  |>
  left_join(parsivel, by = 'datetime') |>
  left_join(pwl_sf) |>
  filter(event_id |> is.na() == F)

# time series over each event
plot_vars <- c('air_temp', 'rh', 'wind_speed', 'wind_dir', 'event_cml_sf', 'ppt')

ffr_met_wnd_lidar_events_fltr <- ffr_met_wnd_lidar_events |>
  group_by(event_id) |>
  mutate(event_cml_sf = cumsum(ppt),
         ppt = ppt*4) |>
  ungroup() |>
  select(datetime, event_id, all_of(plot_vars)) |>
  rename(
    `Air Temp. (°C)` = air_temp,
    `Cuml. Snowfall (mm)` = event_cml_sf,
    `Snowfall (mm/hr)` = ppt,
    `RH (%)` = rh,
    `Wind Dir. (°)` = wind_dir,
    `Wind Speed (m/s)` = wind_speed
  )

saveRDS(ffr_met_wnd_lidar_events_fltr, 'data/event_met/lidar_events_met.rds')

ffr_met_lidr_events_avg <- ffr_met_wnd_lidar_events_fltr |>
  group_by(event_id) |>
  summarise(`Air Temp. (°C)` = mean(`Air Temp. (°C)`),
            `RH (%)` = mean(`RH (%)`),
            `Air Temp. (°C)` = mean(`Air Temp. (°C)`),
            `Wind Dir. (°)` = median(`Wind Dir. (°)`),
            `Wind Speed (m/s)` = median(`Wind Speed (m/s)`),
            `Cuml. Snowfall (mm)` = sum(`Snowfall (mm/hr)`)/4,
            )

saveRDS(ffr_met_lidr_events_avg, 'data/event_met/lidar_events_met_avg.rds')

ffr_met_wnd_lidar_events_long <- ffr_met_wnd_lidar_events_fltr |>
  pivot_longer(!c(datetime, event_id))



for (event in as.character(scan_dates$event_id)) {

  ffr_met_wnd_lidar_events_long |>
    filter(event_id == event) |>
    ggplot(aes(datetime, value, colour = name)) +
    geom_line() +
    facet_grid(name~event_id, scales = 'free') +
    ylab(element_blank()) +
    xlab(element_blank()) +
    theme(legend.position = 'none')

  ggsave(paste0('figs/lidar_periods/met_time_series/met_time_series_', event, '.png'),
         width = 6, height = 8.75)

}
