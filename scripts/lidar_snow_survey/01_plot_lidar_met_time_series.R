# met data from takeoff on pre flight to landing on post flight
# may also want to just look at from snowfall start to landing on post flight

source('scripts/lidar_snow_survey/00_load_lidar_meta.R')

ffr_met_wnd_lidar_events <- lidar_events_long_dt |>
  left_join(ffr_met_wnd)  |>
  left_join(parsivel, by = 'datetime') |>
  left_join(pwl_sf) |>
  filter(event_id |> is.na() == F)

# time series over each event
plot_vars <- c('air_temp', 'rh', 'wind_speed', 'wind_dir', 'event_cml_sf')

ffr_met_wnd_lidar_events_long <- ffr_met_wnd_lidar_events |>
  group_by(event_id) |>
  mutate(event_cml_sf = cumsum(ppt)) |>
  ungroup() |>
  select(datetime, event_id, all_of(plot_vars)) |>
  pivot_longer(!c(datetime, event_id))

for (event in as.character(lidar_event_periods$event_id)) {

  ffr_met_wnd_lidar_events_long |>
    filter(event_id == event) |>
    ggplot(aes(datetime, value, colour = name)) +
    geom_line() +
    facet_grid(name~event_id, scales = 'free')

  ggsave(paste0('figs/interception/lidar_snow_survey/met_time_series/met_time_series_', event, '.png'),
         width = 8, height = 6)

}
