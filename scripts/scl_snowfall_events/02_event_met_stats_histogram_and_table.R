# script to met stats over the lysimeter snow interception periods

wind_threshold <- 999 # m/s

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

event_df_avg_troughs <- throughfall_periods_long |>
  left_join(ffr_met) |>
  left_join(q_tf_scl_avg) |>
  group_by(w_tree_event) |>
  filter(q_sf > 0,
         q_sf > q_tf,
         q_tf > 0) |>
  mutate(cuml_snow = cumsum(q_sf/4),
         q_int_troughs = q_sf - q_tf,
         cuml_int_troughs = cumsum(q_int_troughs/4),
         # cuml_int_tree = cumsum(q_int_tree), # if adding tree back in need to consider tree cal matching
         # IP_tree = q_int_tree/q_sf,
         IP_troughs = q_int_troughs/q_sf,
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
  "Cumulative Snowfall (mm)",
  "Initial Canopy Snow Load (mm)"
)

var_name_dict <-
  data.frame(
    name = c('t', 'rh', 'u', 'p', 'cuml_int_troughs', 'IP_troughs', 'event_del_sf', 'weighed_tree_canopy_load_mm'),
    pretty_name = pretty_names_vect
  )

event_df_long <- event_df_avg_troughs |>
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

event_avgs <- event_df_avg_troughs |>
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

event_avgs_maxmin <- event_df_avg_troughs |>
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
  select(
    w_tree_event,
    starts_with('min'),
    starts_with('mean'),
    starts_with('max'),
    total_snowfall
  ) |>
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
  tab_spanner(
    label = md("Snowfall (mm)"),
    columns = total_snowfall
  ) |>
  # tab_options(
  #   table.font.size = "10px"
  # ) |>
  cols_label(
    w_tree_event = "Start Date",
    # duration = 'Duration (Hrs)',
    min_t = "Min",
    mean_t = "Mean",
    max_t = "Max",
    min_u = "Min",
    mean_u = "Mean",
    max_u = "Max",
    min_IP_troughs = "Min",
    mean_IP_troughs = "Mean",
    max_IP_troughs = "Max",
    total_snowfall = "Total"
  ) |>
  cols_align(align = "center")  |>
  cols_width(
    w_tree_event ~ px(90),           # Set width for 'Start Date' column
    # duration ~ px(60),               # Set width for 'Duration (Hrs)' column
    ends_with("_t") ~ px(50),         # Set equal width for Air Temperature columns
    ends_with("_u") ~ px(50),         # Set equal width for Wind Speed columns
    ends_with("_IP_troughs") ~ px(50),# Set equal width for Interception Efficiency columns
    total_snowfall ~ px(80)          # Set width for 'Total Snowfall' column
  ) |>
  fmt_number(
    columns = c(starts_with("min_"), starts_with("mean_"), starts_with("max_"), total_snowfall),
    decimals = 1
  )

pretty_table

saveRDS(pretty_table, 'data/event_avgs_maxmin_pretty_gt_table.rds')

# gt::gtsave(pretty_table, 'figs/event_avgs_maxmin_pretty_gt_table.pdf')
