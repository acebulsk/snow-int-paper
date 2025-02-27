# Script to plot the average met and SCL IP over each event

# look at trends in met avgs ----

# avg over each event
event_df_sep_troughs_avg <- throughfall_periods_long |>
  left_join(ffr_met) |>
  left_join(q_tf_scl) |>
  left_join(scl_lai_cc_fltr) |>
  group_by(w_tree_event, trough_name) |>
  filter(p > 0,
         q_sf > q_tf,
         q_tf > 0) |>
  group_by(w_tree_event, trough_name) |>
  summarise(event_del_sf = sum(p),
            event_del_tf = sum(d_tf),
            event_del_i = event_del_sf - event_del_tf,
            IP = event_del_i/event_del_sf,
            t = mean(t),
            u = mean(u),
            cc = first(cc)) |>
  group_by(trough_name) |>
  pivot_longer(c(event_del_sf, t, u)) |>
  left_join(var_name_dict) |>
  select(-event_del_tf)

# avg over all three troughs for each event
event_df_avg_troughs_avg <- event_df_sep_troughs_avg |>
  mutate(trough_name = 'scl_mean',
         cc = mean(scl_lai_cc_fltr$cc)) |>
  group_by(w_tree_event, trough_name, name, cc, pretty_name) |>
  summarise(
    event_del_i = mean(event_del_i),
    IP = mean(IP),
    value = mean(value)) |>
  select(names(event_df_sep_troughs_avg))

event_avg_bind <- rbind(event_df_sep_troughs_avg, event_df_avg_troughs_avg) |>
  mutate(cc = factor(round(cc, 2), levels = sort(unique(round(cc, 2)))))

# Add R-squared values to the dataset
y_col <- 'IP'
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

event_df_sep_troughs_avg |>
  filter(trough_name != 'scl_mean') |>
  mutate(cc = factor(round(cc, 2), levels = sort(unique(round(cc, 2))))) |>
  ggplot() +
  geom_point(aes(value, IP, colour = cc, group = cc)) +

  # Conditionally add the smooth line where p.value < 0.05
  geom_smooth(
    data = event_df_sep_troughs_avg |>
      mutate(cc = factor(round(cc, 2), levels = sort(unique(round(cc, 2))))) |>
      left_join(model_summaries, by = c('pretty_name', 'cc')) |>  # Join the summaries
      filter(p.value < 0.05),                   # Filter where p-value < 0.05
    aes(value, IP, colour = cc, group = cc),
    method = 'lm', se = F, linetype = 'solid'
  ) +

  ylab("Interception Efficiency (-)") +
  # xlab('Event Total Snowfall (mm)') +
  labs(colour = 'SCL\nCanopy\nCoverage (-)') +
  theme(legend.position = 'right') +
  scale_color_manual(values = rev(cc_colours)) +
  facet_wrap(~pretty_name, nrow = 3, scales = 'free') +
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

ggsave('figs/automated_snowfall_event_periods/event_avg_temp_wind_cuml_snow_vs_IP_colour_troughs.png',
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
