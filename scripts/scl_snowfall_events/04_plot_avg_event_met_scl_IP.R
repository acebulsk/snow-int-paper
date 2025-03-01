# Script to plot the average met and SCL IP over each event

# look at trends in met avgs ----

# avg over each event
scl_rel_accuracy <- 0.02/100 # rel accuracy is 0.02% as stated in the strain gauge manual
pluvio_rel_accuracy <- 0.2/100 # relative accuracy, or 0.1 mm if absolute error is less than 0.1 mm so use ifelse to control for this, pluvio measures at 0.1 mm resolution

avg_int <- 'event'
error_th <- 100
fig_tag <- paste0('_', avg_int, '_', err_th, 'perc')

tf_periods_scl_pcp_15min <- throughfall_periods_long |>
  left_join(ffr_met |> select(datetime, p, t, u)) |>
  left_join(pwl_pluvio_raw |> select(datetime, pluvio_raw_mm = value)) |>
  left_join(q_tf_scl) |>
  left_join(scl_lai_cc_fltr) |>
  left_join(scl_raw_kg, by = c('datetime', 'trough_name')) |>
  left_join(scl_meta |> select(trough_name, surface_area), by = 'trough_name') |>
  select(-c(storm_id, bad_troughs))

tf_periods_scl_pcp_event <- tf_periods_scl_pcp_15min |>
  filter(p > 0,
         p > d_tf,
         d_tf > 0
  ) |>
  group_by(w_tree_event, trough_name, cc) |>
  summarise(
    datetime = NA,
    event_del_sf = sum(p),
    t = mean(t),
    u = mean(u),
    pluvio_raw_mm = last(pluvio_raw_mm), # take the last measurement of the interval
    event_del_tf = sum(d_tf),
    scl_raw_kg = last(scl_raw_kg),# take the last measurement of the interval
    surface_area = mean(surface_area))|>
  mutate(
    # SCL calcs
    scl_raw_kg = scl_raw_kg + 15, # raw_kg does not include weight of SCL so add estimate here based on diff of when the troughs were taken down in the spring/summer
    scl_abs_accuracy = scl_raw_kg * scl_rel_accuracy,
    d_tf_kg = event_del_tf*surface_area,
    # Pluvio calcs
    pluvio_abs_accuracy = pluvio_raw_mm * pluvio_rel_accuracy,
    pluvio_abs_accuracy = ifelse(pluvio_abs_accuracy < 0.1, 0.1, pluvio_abs_accuracy),
    scl_rel_perc_error = (scl_abs_accuracy/d_tf_kg)*100,
    pluvio_rel_perc_error = (pluvio_abs_accuracy/event_del_sf)*100,
    scl_flag = ifelse(scl_rel_perc_error > error_th, T, F),
    pluvio_flag = ifelse(pluvio_rel_perc_error > error_th, T, F),
    event_del_i = event_del_sf - event_del_tf,
         event_IP = event_del_i/event_del_sf,
         sigma_event_del_i = prop_err_sum(scl_abs_accuracy, pluvio_abs_accuracy),
         sigma_event_IP = prop_err_ratio(event_IP, event_del_i, event_del_i, sigma_event_del_i, pluvio_abs_accuracy),
         sigma_event_IP_hi = event_IP+sigma_event_IP,
         sigma_event_IP_hi = ifelse(sigma_event_IP_hi > 1, 1, sigma_event_IP_hi),
         sigma_event_IP_lo = event_IP-sigma_event_IP,
         sigma_event_IP_lo = ifelse(sigma_event_IP_lo<0,0,sigma_event_IP_lo)
         )  |>
  # filter(pluvio_rel_perc_error < error_th,
  #        scl_rel_perc_error < error_th) |>
  select(datetime,
         w_tree_event,
         trough_name,
         cc,
         event_IP,
         sigma_event_IP_hi,
         sigma_event_IP_lo,
         event_del_sf,
         t,
         u) |>
  pivot_longer(c(event_del_sf, t, u)) |>
  left_join(var_name_dict) |>
  ungroup()

# avg over all three troughs for each event
tf_periods_scl_pcp_event_mean_scl <- tf_periods_scl_pcp_event |>
  mutate(trough_name = 'scl_mean',
         cc = mean(scl_lai_cc_fltr$cc)) |>
  group_by(datetime, w_tree_event, trough_name, name, cc, pretty_name) |>
  summarise(
    event_IP = mean(event_IP),
    value = mean(value),
    sigma_event_IP_hi = mean(sigma_event_IP_hi),
    sigma_event_IP_lo = mean(sigma_event_IP_lo)
    ) |>
  select(names(tf_periods_scl_pcp_event))

event_avg_bind <- rbind(tf_periods_scl_pcp_event, tf_periods_scl_pcp_event_mean_scl) |>
  mutate(cc = factor(round(cc, 2), levels = sort(unique(round(cc, 2)))))

# Add R-squared values to the dataset
y_col <- 'event_IP'
x_col <- 'value'

lm_nest <- event_avg_bind  |>
  group_by(pretty_name, cc, trough_name) |>
  nest() |>
  mutate(model = map(data, ~lm(as.formula(paste(y_col, "~", x_col)), data = .x)),
         resids = map2(data, model, add_residuals),
         preds = map2(data, model, add_predictions),
         glance = map(model, broom::glance))

model_summaries <- lm_nest |>
  unnest(glance) |>
  ungroup() |>
  # left_join(scl_names_dict, by = c('trough_name' = 'name')) |>
  select(pretty_name, trough_name, cc, r.squared,adj.r.squared, p.value, df.residual) |>
  mutate(
    offset_y = case_when(
      cc == 0.64 ~ 0,
      cc == 0.75  ~ .05,
      cc == 0.79  ~ .1
    ),
    n = df.residual + 2
  ) |>
  select(-df.residual)

saveRDS(model_summaries, 'data/lysimeter-data/processed/lysimter_event_avg_regression_stats.rds')

tf_periods_scl_pcp_event |>
  filter(trough_name != 'scl_mean') |>
  mutate(cc = factor(round(cc, 2), levels = sort(unique(round(cc, 2))))) |>
  ggplot() +
  geom_point(aes(value, event_IP, colour = cc, group = cc)) +
  geom_errorbar(
    aes(x = value, ymax = sigma_event_IP_hi, ymin = sigma_event_IP_lo),
    width = 0,
    alpha = 0.2
  )  +
  # Conditionally add the smooth line where p.value < 0.05
  geom_smooth(
    data = event_df_sep_troughs_avg |>
      mutate(cc = factor(round(cc, 2), levels = sort(unique(round(cc, 2))))) |>
      left_join(model_summaries, by = c('pretty_name', 'cc', 'trough_name')) |>  # Join the summaries
      filter(p.value < 0.05),                   # Filter where p-value < 0.05
    aes(value, IP, colour = cc, group = interaction(trough_name, cc)),
    method = 'lm', se = F, linetype = 'solid'
  ) +

  ylab("Interception Efficiency (-)") +
  # xlab('Event Total Snowfall (mm)') +
  labs(colour = 'SCL\nCanopy\nCoverage (-)') +
  theme(legend.position = 'right') +
  scale_color_manual(values = cc_colours) +
  facet_wrap(~pretty_name+trough_name, nrow = 3, scales = 'free') +
  xlab(element_blank()) #+
  # ylim(c(NA, 1)) #+
  # show above model R2 and significance on the graph, decided to move to table
  # geom_text(data = model_summaries,
  #           aes(x = -Inf, y = (1 - offset_y),
  #               label = sprintf("R² = %.3f%s",
  #                               adj.r.squared,
  #                               ifelse(p.value < 0.05, "*", "")),
  #               colour = cc),
  #           hjust = -.1, vjust = 1.1, size = 3, fontface = 'bold')

ggsave(
  paste0(
    'figs/automated_snowfall_event_periods/event_avg_temp_wind_cuml_snow_vs_IP_colour_troughs',
    fig_tag,
    '.png'
  ),
  width = 5, height = 7)

# event_df_sep_troughs_avg_wide <- event_df_sep_troughs |>
#   group_by(w_tree_event, trough_name) |>
#   summarise(event_del_sf = max(cuml_snow),
#             event_del_i = cuml_int[which.max(cuml_snow)],
#             IP = event_del_i/event_del_sf,
#             t = mean(t),
#             u = mean(u),
#             cc = first(cc))
#
# event_df_sep_troughs_avg_wide |>
#   ggplot() +
#   geom_point(aes(t, u, colour = IP)) +
#   facet_wrap(~cc)

# plot wide for pptx

# event_df_sep_troughs_avg |>
#   ggplot() +
#   geom_point(aes(value, IP, colour = cc, group = cc)) +
#
#   # Conditionally add the smooth line where p.value < 0.05
#   geom_smooth(
#     data = event_df_sep_troughs_avg |>
#       left_join(model_summaries, by = c('pretty_name', 'cc')) |>  # Join the summaries
#       filter(p.value < 0.05),                   # Filter where p-value < 0.05
#     aes(value, IP, colour = cc, group = cc),
#     method = 'lm', se = F, linetype = 'solid'
#   ) +
#
#   ylab("Interception Efficiency (-)") +
#   # xlab('Event Total Snowfall (mm)') +
#   labs(colour = 'SCL\nCanopy\nCoverage (-)') +
#   theme(legend.position = 'right') +
#   scale_color_manual(values = cc_colours) +
#   facet_wrap(~pretty_name, ncol = 3, scales = 'free') +
#   xlab(element_blank()) #+
# # ylim(c(NA, 1)) #+
# # show above model R2 and significance on the graph, decided to move to table
# # geom_text(data = model_summaries,
# #           aes(x = -Inf, y = (1 - offset_y),
# #               label = sprintf("R² = %.3f%s",
# #                               adj.r.squared,
# #                               ifelse(p.value < 0.05, "*", "")),
# #               colour = cc),
# #           hjust = -.1, vjust = 1.1, size = 3, fontface = 'bold')
#
# ggsave('figs/automated_snowfall_event_periods/event_avg_temp_wind_cuml_snow_vs_IP_colour_troughs_wide.png',
#        width = 8.5, height = 3)

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
