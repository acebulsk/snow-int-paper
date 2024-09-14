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
  "Interception Efficiency (-)",
  "Cumulative Snowfall (kg m⁻²)"
)

var_name_dict <-
  data.frame(
    name = c('t', 'rh', 'u', 'p', 'cuml_int_troughs', 'IP_troughs', 'event_del_sf'),
    pretty_name = pretty_names_vect
  )

event_df_long <- event_df |>
  mutate(p = p * 4) |>  # mm/15min to mm/hr
  pivot_longer(c(t:u, p, cuml_int_troughs, IP_troughs)) |>
  left_join(var_name_dict, by = 'name') |>
  mutate(pretty_name = factor(pretty_name, levels = c(pretty_names_vect))) |>
  group_by(pretty_name)

# histogram of 15-min met data ----
event_df_long |>
  mutate(mean_value = mean(value, na.rm = TRUE)) |>  # Calculate the mean per group
  ggplot(aes(x = value)) +
  geom_histogram(color = 'black', fill = 'darkgray') +
  geom_vline(aes(xintercept = mean_value), color = 'red', linetype = 'dashed') +  # Add vertical line at mean
  facet_wrap(~pretty_name, scales = 'free') +
  xlab(element_blank()) +
  ylab('Count (-)')

ggsave('figs/automated_snowfall_event_periods/histogram_met_ip_snowfall_periods.png', width = 6, height = 4, device = png)

# boxplot of event met stats ----
event_df_long |>
  filter(name %in% c('t', 'rh', 'u', 'IP_troughs')) |>
ggplot(aes(x = w_tree_event, y = value)) +
  geom_boxplot(fill = 'lightblue', outlier.color = 'black') +  # Boxplot for distribution
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "red") +  # Add mean points
  # stat_summary(fun = max, geom = "text", aes(label = round(..y.., 1)), vjust = -1.5, color = "darkgreen") +  # Max
  # stat_summary(fun = min, geom = "text", aes(label = round(..y.., 1)), vjust = 1.5, color = "darkred") +  # Min
  facet_wrap(~pretty_name, scales = 'free') +  # Facet by snowfall event if necessary
  theme_minimal() +
  xlab(element_blank()) +
  ylab(element_blank()) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))  # Rotate x-axis labels

event_avgs <- event_df |>
  group_by(w_tree_event) |>
  summarise(
    duration = max(elapsed_hours),
    t = mean(t, na.rm = TRUE),
    rh = mean(rh, na.rm = TRUE),
    u = mean(u, na.rm = TRUE),
    IP_troughs = mean(IP_troughs, na.rm = TRUE),
    total_snowfall = sum(p, na.rm = TRUE)
  )

saveRDS(event_avgs, 'data/event_avgs.rds')

event_avgs_maxmin <- event_df |>
  group_by(w_tree_event) |>
  summarise(
    duration = max(elapsed_hours),
    min_t = min(t, na.rm = TRUE),
    mean_t = mean(t, na.rm = TRUE),
    max_t = max(t, na.rm = TRUE),

    min_u = min(u, na.rm = TRUE),
    mean_u = mean(u, na.rm = TRUE),
    max_u = max(u, na.rm = TRUE),

    min_IP_troughs = min(IP_troughs, na.rm = TRUE),
    mean_IP_troughs = mean(IP_troughs, na.rm = TRUE),
    max_IP_troughs = max(IP_troughs, na.rm = TRUE),

    total_snowfall = sum(p, na.rm = TRUE)
  )

saveRDS(event_avgs_maxmin, 'data/event_avgs_maxmin.rds')

# pretty table of event met stats ----
pretty_table <- event_avgs_maxmin |>
  # select(w_tree_event, variable, min, mean, max, total_snowfall) |>
  gt() |>
  tab_spanner(
    label = "Air Temperature (°C)",
    columns = ends_with("_t"),
  ) |>
  tab_spanner(
    label = "Wind Speed (m/s)",
    columns = ends_with("_u")
  ) |>
  tab_spanner(
    label = "Interception Efficiency (-)",
    columns = ends_with("_IP_troughs")
  ) |>
  cols_label(
    w_tree_event = "Start Date",
    duration = 'Duration (Hrs)',
    min_t = "Min",
    mean_t = "Mean",
    max_t = "Max",
    min_u = "Min",
    mean_u = "Mean",
    max_u = "Max",
    min_IP_troughs = "Min",
    mean_IP_troughs = "Mean",
    max_IP_troughs = "Max",
    total_snowfall = "Total Snowfall (mm)"
  ) |>
  cols_align(align = "center")  |>
  cols_width(
    w_tree_event ~ px(95),           # Set width for 'Start Date' column
    duration ~ px(70),               # Set width for 'Duration (Hrs)' column
    ends_with("_t") ~ px(50),         # Set equal width for Air Temperature columns
    ends_with("_u") ~ px(50),         # Set equal width for Wind Speed columns
    ends_with("_IP_troughs") ~ px(50),# Set equal width for Interception Efficiency columns
    total_snowfall ~ px(70)          # Set width for 'Total Snowfall' column
  ) |>
  fmt_number(
    columns = c(starts_with("min_"), starts_with("mean_"), starts_with("max_"), total_snowfall),
    decimals = 1
  )

pretty_table

saveRDS(pretty_table, 'data/event_avgs_maxmin_pretty_gt_table.rds')

gt::gtsave(pretty_table, 'figs/event_avgs_maxmin_pretty_gt_table.pdf')

# plot cuml canopy load using the avg of the troughs ----

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

# as above but separate out the troughs ----
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
  labs(
    y = expression("Canopy Snow Load (kg m"^"-2"~")"),  # y-axis label with units in kg m^-2
    x = expression("Event Cumulative Snowfall (kg m"^"-2"~")")  # x-axis label with units in kg m^-2
  ) +
  labs(colour = 'SCL\nCanopy\nCoverage (-)') +
  # theme(legend.position = 'none') +
  scale_color_manual(values = cc_colours)

ggsave('figs/automated_snowfall_event_periods/cuml_event_snowfall_canopy_storage_sep_scl.png',
       width = 5, height = 4, device = png)

# look at trends in met avgs ----
event_df_sep_troughs_avg <- event_df_sep_troughs |>
  group_by(w_tree_event, trough_name) |>
  summarise(event_del_sf = max(cuml_snow),
            event_del_i = cuml_int_troughs[which.max(cuml_snow)],
            IP = event_del_i/event_del_sf,
            t = mean(t),
            u = mean(u),
            cc = first(cc)) |>
  group_by(trough_name) |>
  pivot_longer(c(event_del_sf, t, u)) |>
  left_join(var_name_dict)

# Add R-squared values to the dataset
y_col <- 'IP'
x_col <- 'value'
lm_nest <- event_df_sep_troughs_avg |>
  group_by(pretty_name, cc) |>
  nest() |>
  mutate(model = map(data, ~lm(as.formula(paste(y_col, "~", x_col)), data = .x)),
         resids = map2(data, model, add_residuals),
         preds = map2(data, model, add_predictions),
         glance = map(model, broom::glance))
model_summaries <- lm_nest |>
  unnest(glance) |>
  select(pretty_name, cc, adj.r.squared, p.value) |>
  mutate(
    offset_y = case_when(
      cc == 0.73 ~ 0,
      cc == 0.78  ~ .04,
      cc == 0.82  ~ .08
    )
  )

event_df_sep_troughs_avg |>
  ggplot() +
  geom_point(aes(value, IP, colour = cc, group = cc)) +
  geom_smooth(aes(value, IP, colour = cc, group = cc), method = 'lm', se = F, linetype = 'solid')+
  ylab("Interception Efficiency (-)") +
  # xlab('Event Total Snowfall (mm)') +
  labs(colour = 'SCL\nCanopy\nCoverage (-)') +
  theme(legend.position = 'right') +
  scale_color_manual(values = cc_colours) +
  facet_wrap(~pretty_name, nrow = 3, scales = 'free') +
  xlab(element_blank()) +
  geom_text(data = model_summaries,
            aes(x = -Inf, y = .85 - offset_y,
                label = sprintf("R² = %.3f%s",
                                adj.r.squared,
                                ifelse(p.value < 0.05, "*", "")),
                colour = cc),
            hjust = -0.1, vjust = 1.1, size = 3, fontface = 'bold')

ggsave('figs/automated_snowfall_event_periods/event_avg_temp_wind_cuml_snow_vs_IP_colour_troughs.png',
       width = 5, height = 7)

# plotly::ggplotly()

# use this method if you want to try binning

# sf_breaks <- seq(
#   0,
#   50,
#   5)
#
# sf_labs <- label_bin_fn(sf_breaks)
#
# event_df_sep_troughs_avg$event_del_sf_bin <-
#   cut(event_df_sep_troughs_avg$event_del_sf,
#       sf_breaks,
#       include.lowest = T)
#
# event_df_sep_troughs_avg$event_del_sf_bin_lab <-
#   cut(event_df_sep_troughs_avg$event_del_sf,
#       sf_breaks,
#       labels = sf_labs,
#       include.lowest = T) |> as.character() |> as.numeric()
#
# event_df_sep_troughs_smry <- event_df_sep_troughs_avg |>
#   group_by(event_del_sf_bin_lab, event_del_sf_bin) |>
#   # filter(weighed_tree_canopy_load_mm <= 5) |>
#   summarise(IP_avg = mean(IP, na.rm = T),
#             sd = sd(IP, na.rm = T),
#             sd_low = IP_avg - sd,
#             sd_hi = IP_avg + sd,
#             ci_low = quantile(IP,0.05),
#             ci_hi = quantile(IP, 0.95),
#             n = n()) |>
#   filter(n >= 3)

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
