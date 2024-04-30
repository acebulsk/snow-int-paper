# plot time series of scl data and met for each selected interception event

events <- met_intercept$storm_id |> unique()

alldfs_indiv_troughs <- met_intercept |>
  select(datetime, storm_id, trough_name = name, t, u, q_sf, q_tf, tree_mm = weighed_tree_canopy_load_mm) |>
  pivot_longer(t:tree_mm) |>
  mutate(group = ifelse(name %in% c('q_sf', 'q_tf'), 'rates', name))

alldfs_avg_trough <- alldfs_indiv_troughs |>
  group_by(datetime, storm_id, name) |>
  summarise(
    value = mean(value, na.rm = T),
    group = first(group))



for (i in 1:length(events)) {

  alldfs_indiv_troughs |>
    mutate(name = ifelse(name == 'q_tf', trough_name, name)) |>
    filter(storm_id == events[i]) |>
    ggplot(aes(datetime, value, colour = name)) +
    geom_line() +
    facet_grid(group~storm_id, scales = 'free')

  ggsave(paste0('figs/supplement/scl-timeseries/', events[i], '_met_data.png'),
         width = 8, height = 6)

}

for (i in 1:length(storm_dates_wide$storm_id)) {

  alldfs_avg_trough |>
    filter(storm_id == events[i]) |>
    ggplot(aes(datetime, value, colour = name)) +
    geom_line() +
    facet_grid(group~storm_id, scales = 'free')

  ggsave(paste0('figs/supplement/scl-timeseries-avgs/', events[i], '_met_data.png'),
         width = 8, height = 6)

}
