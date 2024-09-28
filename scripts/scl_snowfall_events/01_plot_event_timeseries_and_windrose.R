# plot time series of scl data and met for each selected interception event and
# wind rose is below

events <- met_intercept$storm_id |> unique()

# plot event averages ----

avg_event <- met_intercept |>
  group_by(storm_id, name) |>
  summarise(
    t_min = min(t),
    t_mean = mean(t),
    t_max = max(t),
    u_min = min(u),
    u_mean = min(u),
    u_max = max(u),
    del_sf = sum(q_sf / 4),
    del_tf = sum(q_tf / 4)) |>
  mutate(
    del_i = del_sf - del_tf,
    ip = del_i/del_sf
  )

ggplot(avg_event, aes(t_mean, ip)) + geom_point() + facet_grid(~name)
ggsave('figs/supplement/event-avgs/event_avg_temp_vs_ip.png', width = 8, height = 4)
ggplot(avg_event, aes(u_mean, ip)) + geom_point() + facet_grid(~name)
ggsave('figs/supplement/event-avgs/event_avg_u_vs_ip.png', width = 8, height = 4)
ggplot(avg_event, aes(del_sf, ip)) + geom_point() + facet_grid(~name)
ggsave('figs/supplement/event-avgs/event_del_sf_vs_ip.png', width = 8, height = 4)

# plot event time series ----

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

for (i in 1:length(events)) {

  alldfs_avg_trough |>
    filter(storm_id == events[i]) |>
    ggplot(aes(datetime, value, colour = name)) +
    geom_line() +
    facet_grid(group~storm_id, scales = 'free')

  ggsave(paste0('figs/supplement/scl-timeseries-avgs/', events[i], '_met_data.png'),
         width = 8, height = 6)

}

# plot wind rose over all snowfall periods, main purpose here is to show
# dominant wind direction influencing the diff relationship of wind speed with
# IP for the different troughs

p <- weatherdash::wind_rose(ffr_met_wnd,
                            'datetime',
                            'wind_speed',
                            'wind_dir',
                            dir_res = 30,
                            ws_res = 1,
                            ws_max = 5,
                            # plot_title = 'FT',
                            spd_unit = 'm/s'
)

p

plotly::save_image(p, paste0('figs/automated_snowfall_event_periods/ft_wind_rose_allevents_snowing.png'), width = 2, height = 5)
