# script to plot continuous measurements of snowfall and interception
# data is from the pluvio (q_sf) and weighed tree (W)

wind_threshold <- 999 # m/s

# get periods updated to not include times where troughs are unloading

storm_dates_wide <- read.csv('../../analysis/interception/data/select_storms_datetime_wide_independent_snow_surveys.csv', skip = 1) |>
  filter(quality < 4,
         event_starts_late == F) |>
  mutate(
    from = as.POSIXct(from, tz = 'Etc/GMT+6'),
    to = as.POSIXct(to, tz = 'Etc/GMT+6'),
    storm_id = format(from, "%Y-%m-%d_%H")) |>
  select(from, to, w_tree_event, storm_id, bad_troughs)

to_long <- function(from, to, w_tree_event, storm_id, bad_troughs){
  datetime <- seq(from, to, 900)

  out <- data.frame(datetime, w_tree_event, storm_id, bad_troughs)

  return(out)
}

storm_dates_long <- purrr::pmap_dfr(storm_dates_wide, to_long)

met_df <- readRDS('../../analysis/met-data-processing/data/ffr_crhm_modelling_obs.rds')

load_df <- readRDS('../../analysis/interception/data/loadcell/treefort_load_main_cal_plv_fsd_mm.rds')

# ggplot(load_df, aes(datetime, value)) +
#   geom_line() +
#   facet_grid(rows = vars(name), scales = 'free')
#
# plotly::ggplotly()

q_int_tree <- load_df |>
  pivot_wider() |> select(datetime, tree_mm) |>
  mutate(q_int_tree = ifelse(is.na(lag(tree_mm)), 0, tree_mm - lag(tree_mm)),
         q_int_tree = ifelse(q_int_tree<0, 0, q_int_tree))

q_tf_scl <- load_df |>
  filter(name != 'tree_mm')  |>
  group_by(name) |>
  mutate(q_tf = ifelse(is.na(lag(value)), 0, value - lag(value)),
         q_tf = ifelse(q_tf<0, 0, q_tf)) |>
  select(datetime, name, q_tf) |>
  left_join(storm_dates_long) |>
  mutate(row_flag = mapply(grepl, name, bad_troughs)) |> # remove troughs where only one is unloading, could also jsut use more reliable medium trough but would have to remove some events (see notes)
  ungroup() |>
  filter(row_flag == F,
         is.na(q_tf) == F) |>
  select(datetime, trough_name = name, q_tf) |>
  ungroup()

q_tf_scl_avg <- q_tf_scl |>
  group_by(datetime) |>
  summarise(q_tf = mean(q_tf, na.rm = T))

# bad_events <- c('2022-03-23', '2022-02-14') |> as.Date()

# good_events <- c('2022-06-13',
#                  '2022-01-02',
#                  '2022-01-31',
#                  '2022-02-19',
#                  # '2022-03-01',
#                  # '2022-05-19',
#                  # '2023-01-27',
#                  # '2023-02-19',
#                  '2023-02-26',
#                  '2023-03-13')

event_df <- storm_dates_long |>
  left_join(met_df) |>
  left_join(q_int_tree) |>
  left_join(q_tf_scl_avg) |>
  group_by(w_tree_event) |>
  filter(p > 0,
         p > q_tf,
         q_tf > 0) |>
  mutate(cuml_snow = cumsum(p),
         q_int_troughs = p - q_tf,
         cuml_int_troughs = cumsum(q_int_troughs),
         cuml_int_tree = cumsum(q_int_tree),
         IP_tree = q_int_tree/p,
         IP_troughs = q_int_troughs/p,
         start_time = min(datetime),
         elapsed_hours = as.numeric(difftime(datetime, start_time, units = "hours"))
         )# |> filter(w_tree_event %in% good_events)

pretty_names_vect <- c(
  temp_ax_lab,
  "Relative Humidity (%)",
  wind_ax_lab,
  "Snowfall Rate (mm/hr)",
  "Canopy Load (mm)",
  "Interception Efficiency (-)"
)

var_name_dict <-
  data.frame(
    name = c('t', 'rh', 'u', 'p', 'cuml_int_troughs', 'IP_troughs'),
    pretty_name = pretty_names_vect
  )

event_df |>
  mutate(p = p * 4) |>  # mm/15min to mm/hr
  pivot_longer(c(t:u, p, cuml_int_troughs, IP_troughs)) |>
  left_join(var_name_dict, by = 'name') |>
  mutate(pretty_name = factor(pretty_name, levels = c(pretty_names_vect))) |>
  ggplot(aes(x = value)) +
  geom_histogram(color = 'black', fill = 'darkgray') +
  facet_wrap(~pretty_name, scales = 'free') +
  xlab(element_blank()) +
  ylab('Count (-)')

ggsave('figs/automated_snowfall_event_periods/histogram_met_ip_snowfall_periods.png', width = 6, height = 4, device = png)

event_avgs <- event_df |>
  group_by(w_tree_event) |>
  summarise(
    median_u = median(u),
    across(c(t:Qsi, IP_tree, IP_troughs), mean),
            peak_canopy_load_tree = max(cuml_int_tree),
            peak_canopy_load_scl = max(cuml_int_troughs),
            total_snowfall = sum(p))

saveRDS(event_avgs, 'data/event_avgs.rds')

# plot cuml canopy load using the avg of the troughs

event_df |>
  ggplot(aes(cuml_snow, cuml_int_troughs, group =w_tree_event)) +
  geom_line(alpha = 0.7) +
  # geom_smooth(method = 'lm', formula = y ~ x - 1, se = F, color = 'blue', linetype = 'dashed') +
  # geom_ribbon(aes(ymin = lm_tree - (lm_tree*0.3),
  #                 ymax = lm_tree + (lm_tree*0.3)), alpha = 0.3) +
  # facet_grid(~name) +
  # annotate("text", label = paste("y =", round(coef(lm)[[1]], 2), "*x"), color = "blue", parse = TRUE) +
  ylab('Canopy Storage (mm)') +
  xlab('Snowfall (mm)') +
  labs(colour = temp_ax_lab) +
  theme(legend.position = 'bottom')

ggsave('figs/automated_snowfall_event_periods/cuml_event_snowfall_canopy_storage_scl.png',
       width = 4, height = 4)

# as above but separate out the troughs
scl_lai_cc <- read.csv('~/local-usask/analysis/interception/data/lai/scl_canopy_metrics.csv') |>
  arrange(cc) |>
  mutate(cc = factor(round(cc, 2), levels = sort(unique(round(cc, 2)))))

scl_lai_cc$trough_name <- c('sparse_forest', 'medium_density_forest', 'dense_forest')

event_df_sep_troughs <- storm_dates_long |>
  left_join(met_df) |>
  left_join(q_tf_scl) |>
  left_join(scl_lai_cc) |>
  group_by(w_tree_event, trough_name) |>
  filter(p > 0,
         p > q_tf,
         q_tf > 0) |>
  mutate(cuml_snow = cumsum(p),
         q_int_troughs = p - q_tf,
         cuml_int_troughs = cumsum(q_int_troughs),
         IP_troughs = q_int_troughs/p,
         start_time = min(datetime),
         elapsed_hours = as.numeric(difftime(datetime, start_time, units = "hours")))

event_df_sep_troughs |>
  ggplot(aes(cuml_snow, cuml_int_troughs, colour = cc, group = interaction(w_tree_event, cc))) +
  geom_line(alpha = 0.7) +
  # geom_smooth(method = 'lm', formula = y ~ x - 1, se = F, color = 'blue', linetype = 'dashed') +
  # geom_ribbon(aes(ymin = lm_tree - (lm_tree*0.3),
  #                 ymax = lm_tree + (lm_tree*0.3)), alpha = 0.3) +
  # facet_grid(~name) +
  # annotate("text", label = paste("y =", round(coef(lm)[[1]], 2), "*x"), color = "blue", parse = TRUE) +
  ylab('Canopy Storage (mm)') +
  xlab('Event Cumulative Snowfall (mm)') +
  labs(colour = 'Lysimeter Canopy Coverage (-)') +
  theme(legend.position = 'none') +
  scale_color_manual(values = cc_colours)

ggsave('figs/automated_snowfall_event_periods/cuml_event_snowfall_canopy_storage_sep_scl.png',
       width = 4, height = 4)

event_df_sep_troughs_avg <- event_df_sep_troughs |>
  group_by(w_tree_event, trough_name) |>
  summarise(event_del_sf = max(cuml_snow),
            event_del_i = cuml_int_troughs[which.max(cuml_snow)],
            IP = event_del_i/event_del_sf,
            cc = first(cc)) |>
  group_by(trough_name) |>
  mutate(trough_avg_IP = mean(IP))


sf_breaks <- seq(
  0,
  50,
  5)

sf_labs <- label_bin_fn(sf_breaks)

event_df_sep_troughs_avg$event_del_sf_bin <-
  cut(event_df_sep_troughs_avg$event_del_sf,
      sf_breaks,
      include.lowest = T)

event_df_sep_troughs_avg$event_del_sf_bin_lab <-
  cut(event_df_sep_troughs_avg$event_del_sf,
      sf_breaks,
      labels = sf_labs,
      include.lowest = T) |> as.character() |> as.numeric()

event_df_sep_troughs_smry <- event_df_sep_troughs_avg |>
  group_by(event_del_sf_bin_lab, event_del_sf_bin) |>
  # filter(weighed_tree_canopy_load_mm <= 5) |>
  summarise(IP_avg = mean(IP, na.rm = T),
            sd = sd(IP, na.rm = T),
            sd_low = IP_avg - sd,
            sd_hi = IP_avg + sd,
            ci_low = quantile(IP,0.05),
            ci_hi = quantile(IP, 0.95),
            n = n()) |>
  filter(n >= 3)

event_df_sep_troughs_avg |>
  ggplot() +
  geom_point(aes(event_del_sf, IP, colour = cc, group = cc)) +
  geom_errorbar(data = event_df_sep_troughs_smry,
                aes(x = event_del_sf_bin_lab, ymax = sd_hi, ymin = sd_low),
                width = .5)  +
  geom_point(
    data = event_df_sep_troughs_smry,
    aes(x = event_del_sf_bin_lab, y = IP_avg),
    shape = 1,
    size = 4
  ) +
  geom_hline(aes(yintercept = trough_avg_IP, colour = cc), linetype = "dashed") +
  ylab("Interception Efficiency (-)") +
  xlab('Event Total Snowfall (mm)') +
  labs(colour = 'Lysimeter\nCanopy\nCoverage (-)') +
  theme(legend.position = 'right') +
  scale_color_manual(values = cc_colours)

ggsave('figs/automated_snowfall_event_periods/event_total_snowfall_vs_IP_colour_troughs.png',
       width = 5, height = 4)


# show the avg temp of the event

event_df_sep_troughs |>
  # pivot_longer(c(SCL)) |>
  select(-t) |>
  left_join(event_avgs |> select(w_tree_event, t)) |>
  # filter(w_tree_event %in% low_wind_events) |>
  ggplot(aes(cuml_snow, cuml_int_troughs, colour = t, group = t)) +
  geom_line() + scale_color_viridis_c(option = 'magma', end = .90) +
  # facet_grid(~name) +
  ylab('Canopy Storage (mm)') +
  xlab('Snowfall (mm)') +
  labs(colour = temp_ax_lab) +
  theme(legend.position = 'bottom')

ggsave('figs/automated_snowfall_event_periods/cuml_event_snowfall_canopy_storage_scl_colour_temp.png',
       width = 4, height = 6)

event_df_sep_troughs |>
  # pivot_longer(c(SCL)) |>
  select(-u) |>
  left_join(event_avgs |> select(w_tree_event, median_u)) |>
  # filter(w_tree_event %in% low_wind_events) |>
  ggplot(aes(cuml_snow, cuml_int_troughs, colour = median_u, group = interaction(median_u, w_tree_event))) +
  geom_line() + scale_color_viridis_c(option = 'viridis', end = .90) +
  # facet_grid(~name) +
  ylab('Canopy Storage (mm)') +
  xlab('Snowfall (mm)') +
  labs(colour = wind_ax_lab) +
  theme(legend.position = 'bottom')

ggsave('figs/automated_snowfall_event_periods/cuml_event_snowfall_canopy_storage_scl_colour_wind.png',
       width = 4, height = 6)

event_df |>
  pivot_longer(c(cuml_int_troughs, t, rh, u, cuml_snow)) |>
  mutate(name = factor(name, ordered = T,
                       levels = c('cuml_snow', 'Tree', 't', 'rh', 'u'),
                       labels = c('Snowfall (mm)',
                                  'Canopy Snow Load (mm)',
                                  'Air Temperature (°C)',
                                  'Relative Humidity (%)',
                                  "Wind Speed (m/s)"))) |>
  # left_join(event_avgs |> select(w_tree_event, t)) |>
  # filter(w_tree_event %in% low_wind_events) |>
  ggplot(aes(elapsed_hours, value)) +
  geom_line() + scale_color_viridis_c(option = 'magma', end = .90) +
  facet_grid(rows = vars(name), scales = 'free', cols = vars(w_tree_event)) +
  ylab('') +
  xlab('Event time elapsed (hours)') +
  labs(colour = temp_ax_lab)

ggsave('figs/automated_snowfall_event_periods/select_event_cuml_snowfall_canopy_storage_scl_w_met.png', width = 8, height = 9)

# plotly::ggplotly()

# sel_ip <- event_df_sep_troughs |>
#   select(w_tree_event, datetime, Tree = IP_tree, SCL = IP_troughs) |>
#   pivot_longer(c(Tree, SCL), names_to = 'inst', values_to = 'IP')
#
# event_df_sep_troughs |>
#   rename(Tree = cuml_int_tree, SCL = cuml_int_troughs) |>
#   pivot_longer(c(Tree, SCL)) |>
#   select(-t) |>
#   left_join(event_avgs |> select(w_tree_event, t)) |>
#   filter(w_tree_event %in% low_wind_events) |>
#   ggplot(aes(cuml_snow, value, colour = t, group = t)) +
#   geom_line() + scale_color_viridis_c(option = 'magma', end = .90) +
#   facet_grid(~name) +
#   ylab('Canopy Storage (mm)') +
#   xlab('Snowfall (mm)') +
#   labs(colour = temp_ax_lab)sel_W <- event_df_sep_troughs |>
#   ungroup() |>
#   select(datetime, Tree = cuml_int_tree, SCL = cuml_int_troughs) |>
#   pivot_longer(c(Tree, SCL), names_to = 'inst', values_to = 'W')

# left_join(sel_ip, sel_W, by = c('datetime', 'inst')) |>
#   left_join(event_avgs |> select(w_tree_event, t)) |>
#   filter(w_tree_event %in% low_wind_events) |>
#   ggplot(aes(W, IP, colour = t, group = t)) +
#   geom_line() + scale_color_viridis_c(option = 'magma', end = .90) +
#   facet_grid(~inst) +
#   ylab('Interception Efficiency (-)') +
#   xlab('Canopy Storage (mm)') +
#   labs(colour = temp_ax_lab)
#
# ggsave('figs/interception/canopy_storage_VS_IP_scl_tree.png', width = 7, height = 3)

# plotly::ggplotly()
