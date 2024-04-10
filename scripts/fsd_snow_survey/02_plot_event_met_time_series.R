# plot met over the fsd survey periods
source('scripts/fsd_snow_survey/01_avg_met_fsd_periods.R')

ffr_met_wnd_events <- storms_long_datetime |>
  left_join(ffr_met_wnd)  |>
  left_join(parsivel, by = 'datetime') |>
  left_join(pwl_sf) |>
  filter(event_id |> is.na() == F)

# time series over each event
plot_vars <- c('air_temp', 'rh', 'wind_speed', 'wind_dir', 'event_cml_sf')

ffr_met_wnd_events_long <- ffr_met_wnd_events |>
  group_by(event_id) |>
  mutate(event_cml_sf = cumsum(ppt)) |>
  ungroup() |>
  select(datetime, event_id, all_of(plot_vars)) |>
  pivot_longer(!c(datetime, event_id))

for (event in as.character(fsd_periods_wide$event_id)) {

  ffr_met_wnd_events_long |>
    filter(event_id == event) |>
    ggplot(aes(datetime, value, colour = name)) +
    geom_line() +
    facet_grid(name~event_id, scales = 'free')

  ggsave(paste0('figs/snow_survey_periods/met_time_series/met_time_series_', event, '.png'),
         width = 8, height = 6)

}
