# met data from takeoff on pre flight to landing on post flight
# may also want to just look at from snowfall start to landing on post flight

source('scripts/lidar_snow_survey/00_load_lidar_meta.R')

ffr_met_wnd_lidar_events <- lidar_events_long_dt |>
  left_join(ffr_met_wnd, by = 'datetime')  |>
  left_join(parsivel, by = 'datetime') |>
  left_join(pwl_sf, by = 'datetime') |>
  left_join(pwl_wind, by = 'datetime') |>
  filter(event_id |> is.na() == F) |>
  rename(
    ft_wind_speed = wind_speed.x,
    pwl_wind_speed = wind_speed.y,
    ft_wind_dir = wind_dir,
    pwl_wind_dir = wind_dir_true
  )

# time series over each event

var_name_dict <- data.frame(
  var_lab = c(
    'Air Temp. (°C)',
    'Cuml. Snowfall (mm)',
    'Snowfall (mm/hr)',
    'RH (%)',
    'Wind Dir. (°)',
    'Wind Dir. (°)',
    'Wind Speed (m/s)',
    'Wind Speed (m/s)'
  ),
  var_stn = c(
    'FT',
    'PWL',
    'PWL',
    'FT',
    'FT',
    'PWL',
    'FT',
    'PWL'
  ),
  name = c(
    'air_temp',
    'event_cml_sf',
    'ppt',
    'rh',
    'ft_wind_dir',
    'pwl_wind_dir',
    'ft_wind_speed',
    'pwl_wind_speed'
  )
)

ffr_met_wnd_lidar_events_fltr <- ffr_met_wnd_lidar_events |>
  group_by(event_id) |>
  mutate(event_cml_sf = cumsum(ppt),
         ppt = ppt*4) |>
  ungroup() |>
  select(datetime, event_id, all_of(var_name_dict$name))

saveRDS(ffr_met_wnd_lidar_events_fltr, 'data/event_met/lidar_events_met.rds')

ffr_met_lidr_events_avg <- ffr_met_wnd_lidar_events_fltr |>
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
    `mean PWL Wind Speed (m/s)` = mean(pwl_wind_speed),
    `Cuml. Snowfall (mm)` = sum(ppt)/4
  )

saveRDS(ffr_met_lidr_events_avg, 'data/event_met/lidar_events_met_avg.rds')

ffr_met_wnd_lidar_events_long <- ffr_met_wnd_lidar_events_fltr |>
  pivot_longer(!c(datetime, event_id)) |>
  left_join(var_name_dict, by = 'name') |>
  mutate(var_lab = factor(var_lab, levels = c(
    'Air Temp. (°C)',
    'RH (%)',
    'Cuml. Snowfall (mm)',
    'Snowfall (mm/hr)',
    'Wind Speed (m/s)',
    'Wind Dir. (°)'
  )))

for (event in as.character(scan_dates$event_id)) {

  ffr_met_wnd_lidar_events_long |>
    filter(event_id == event) |>
    ggplot(aes(datetime, value, colour = var_stn)) +
    geom_line() +
    facet_grid(rows = vars(var_lab), scales = 'free') +
    ylab(element_blank()) +
    xlab(element_blank()) +
    theme(legend.position = 'bottom') +
    scale_color_manual(values = c('salmon', 'dodgerblue'), name = 'Station: ')

  ggsave(paste0('figs/lidar_periods/met_time_series/met_time_series_', event, '.png'),
         width = 6, height = 8.95)

}
