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
  mutate(q_tf = ifelse(is.na(lag(value)), 0, value - lag(value))) |>
  select(datetime, name, q_tf) |>
  left_join(storm_dates_long) |>
  mutate(row_flag = mapply(grepl, name, bad_troughs)) |> # remove troughs where only one is unloading, could also jsut use more reliable medium trough but would have to remove some events (see notes)
  ungroup() |>
  filter(row_flag == F,
         is.na(q_tf) == F) |>
  select(datetime, q_tf) |>
  group_by(datetime) |>
  summarise(q_tf = mean(q_tf, na.rm = T))

bad_events <- c('2022-03-23', '2022-02-14') |> as.Date()

event_df <- storm_dates_long |>
  left_join(met_df) |>
  left_join(q_int_tree) |>
  left_join(q_tf_scl) |>
  group_by(w_tree_event) |>
  # filter(p > 0,
  #        p > q_tf) |>
  mutate(cuml_snow = cumsum(p),
         q_int_troughs = p - q_tf,
         cuml_int_troughs = cumsum(q_int_troughs),
         cuml_int_tree = cumsum(q_int_tree)) |> filter(!w_tree_event %in% bad_events)

class_event_met <- event_df |>
  group_by(w_tree_event) |>
  summarise(across(c(t:Qsi), mean),
            peak_canopy_load_tree = max(cuml_int_tree),
            peak_canopy_load_scl = max(cuml_int_troughs),
            event_ip_tree = peak_canopy_load_tree/max(cuml_snow),
            event_ip_scl = peak_canopy_load_scl/max(cuml_snow))


class_event_met |>
  rename(Tree = peak_canopy_load_tree, SCL = peak_canopy_load_scl) |>
  pivot_longer(c(Tree, SCL)) |>
  ggplot(aes(t, value, colour = name)) +
  geom_smooth(method = 'lm', se = F) +
  stat_cor(aes(colour = name,label = after_stat(rr.label)), geom = "label", show.legend = F) +
  geom_point() +
  ylab('Maximum Canopy Storage (mm)') +
  xlab(temp_ax_lab) +
  labs(colour = 'Measurement:')

ggsave('figs/interception/air_temp_w_max_scl_tree.png', width = 5, height = 4, device = png)

# class_event_met |>
#   pivot_longer(c(event_ip_tree, event_ip_scl)) |>
#   ggplot(aes(t, value, colour = name)) +
#   geom_point()

low_wind_events <- class_event_met$w_tree_event[class_event_met$u < wind_threshold]

# is the w tree and troughs showing the same thing?
# event_df |>
#   filter(w_tree_event %in% low_wind_events) |>
#   pivot_longer(c(cuml_int_troughs, cuml_int_tree)) |>
#   ggplot(aes(cuml_snow, value, colour = name)) +
#   geom_line() +
#   facet_wrap(~w_tree_event, scales = 'free')

# look at all events colour by event id
# event_df |>
#   filter(w_tree_event %in% low_wind_events) |>
#   ggplot(aes(cuml_snow, cuml_int_tree, colour = as.factor(w_tree_event))) +
#   geom_line()

# plotly::ggplotly()

# show the change in temp over the event

# event_df |>
#   filter(w_tree_event %in% low_wind_events) |>
#   ggplot(aes(cuml_snow, tree_mm_zeroed, colour = t)) +
#   geom_point() + scale_color_viridis_c()

# show the avg temp of the event

event_df |>
  rename(Tree = cuml_int_tree, SCL = cuml_int_troughs) |>
  pivot_longer(c(Tree, SCL)) |>
  select(-t) |>
  left_join(class_event_met |> select(w_tree_event, t)) |>
  filter(w_tree_event %in% low_wind_events) |>
  ggplot(aes(cuml_snow, value, colour = t, group = t)) +
  geom_line() + scale_color_viridis_c(option = 'magma', end = .90) +
  facet_grid(~name) +
  ylab('Canopy Storage (mm)') +
  xlab('Snowfall (mm)') +
  labs(colour = temp_ax_lab)

ggsave('figs/interception/cuml_event_snowfall_canopy_storage_scl_tree.png', width = 7, height = 3)

# plotly::ggplotly()
