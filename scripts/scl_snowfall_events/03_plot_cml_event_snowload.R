# Plot cumulative canopy load over the snowfall periods using the SCLs

# these dont start at 0 because we accumulate starting when precip/throughfall begins

# plot cuml canopy load using the avg of the troughs ----

event_df_avg_troughs |>
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

event_df_sep_troughs <- throughfall_periods_long |>
  left_join(ffr_met) |>
  left_join(q_tf_scl) |>
  left_join(scl_lai_cc_fltr) |>
  group_by(w_tree_event, trough_name) |>
  filter(p > 0,
         q_sf > q_tf,
         q_tf > 0) |>
  mutate(
          cc = factor(round(cc, 2), levels = sort(unique(round(cc, 2)))),
         cuml_snow = cumsum(p),
         q_int_troughs = (q_sf - q_tf)/4,
         q_int_troughs = ifelse(q_int_troughs < 0, 0, q_int_troughs),
         cuml_int = cumsum(q_int_troughs),
         IP_troughs = q_int_troughs/p,
         start_time = min(datetime),
         elapsed_hours = as.numeric(difftime(datetime, start_time, units = "hours")),
         inst_type = 'SCL') |>
  select(datetime, w_tree_event, cuml_snow, cuml_int, cc, inst_type)

event_df_sep_troughs |>
  # filter(w_tree_event == '2021-12-23') |>
  ggplot(aes(cuml_snow, cuml_int, colour = cc, group = interaction(w_tree_event, cc))) +
  geom_line(alpha = 0.7) +
  # geom_smooth(method = 'lm', formula = y ~ x - 1, se = F, color = 'blue', linetype = 'dashed') +
  # geom_ribbon(aes(ymin = lm_tree - (lm_tree*0.3),
  #                 ymax = lm_tree + (lm_tree*0.3)), alpha = 0.3) +
  # facet_grid(~name) +
  # annotate("text", label = paste("y =", round(coef(lm)[[1]], 2), "*x"), color = "blue", parse = TRUE) +
  labs(
    # y = expression("Canopy Snow Load (kg m"^"-2"~")"),  # y-axis label with units in kg m^-2
    # x = expression("Event Cumulative Snowfall (kg m"^"-2"~")")  # x-axis label with units in kg m^-2
    y = "Canopy Snow Load (mm)",  # y-axis label with units in kg m^-2
    x = "Event Cumulative Snowfall (mm)"  # x-axis label with units in kg m^-2
  ) +
  labs(colour = 'SCL\nCanopy\nClosure (-)') +
  # theme(legend.position = 'none') +
  scale_color_manual(values = cc_colours)

ggsave('figs/automated_snowfall_event_periods/cuml_event_snowfall_canopy_storage_sep_scl.png',
       width = 5, height = 4, device = png)

# plot cuml canopy load using the weighed tree ----

event_df_w_tree <- throughfall_periods_long |>
  left_join(ffr_met) |>
  left_join(q_int_tree) |>
  group_by(trough_name, w_tree_event) |>
  filter(p > 0,
        # p > q_int_tree,
         q_int > 0) |>
  mutate(cuml_snow = cumsum(p),
         cuml_int = cumsum(d_int),
         IP_tree = d_int/p,
         start_time = min(datetime),
         cc = factor(round(tree_cal_cc, 2), levels = sort(unique(round(tree_cal_cc, 2)))),
         elapsed_hours = as.numeric(difftime(datetime, start_time, units = "hours")),
         inst_type = 'Weighed Tree Lysimeter') |>
  select(datetime, w_tree_event, cuml_snow, cuml_int, cc, inst_type)

event_df_w_tree |>
  ggplot(aes(cuml_snow, cuml_int, colour = cc, group = interaction(w_tree_event, cc))) +
  geom_line(alpha = 0.7) +
  # geom_smooth(method = 'lm', formula = y ~ x - 1, se = F, color = 'blue', linetype = 'dashed') +
  # geom_ribbon(aes(ymin = lm_tree - (lm_tree*0.3),
  #                 ymax = lm_tree + (lm_tree*0.3)), alpha = 0.3) +
  # facet_grid(~name) +
  # annotate("text", label = paste("y =", round(coef(lm)[[1]], 2), "*x"), color = "blue", parse = TRUE) +
  ylab('Canopy Snow Load (mm)') +
  xlab('Snowfall (mm)') +
  labs(colour = 'SCL\nCanopy\nClosure (-)') +
  theme(legend.position = 'bottom')

ggsave('figs/automated_snowfall_event_periods/cuml_event_snowfall_canopy_storage_w_tree_cc_avg_of_snow_surveys.png',
       width = 4, height = 4)

# plot sep troughs alongside the tree ----

rbind(event_df_sep_troughs, event_df_w_tree) |>
  ggplot(aes(cuml_snow, cuml_int, colour = cc, group = interaction(w_tree_event, cc))) +
  geom_line(alpha = 0.7) +
  # geom_smooth(method = 'lm', formula = y ~ x - 1, se = F, color = 'blue', linetype = 'dashed') +
  # geom_ribbon(aes(ymin = lm_tree - (lm_tree*0.3),
  #                 ymax = lm_tree + (lm_tree*0.3)), alpha = 0.3) +
  # facet_grid(~name) +
  # annotate("text", label = paste("y =", round(coef(lm)[[1]], 2), "*x"), color = "blue", parse = TRUE) +
  ylab('Canopy Snow Load (mm)') +
  xlab('Snowfall (mm)') +
  labs(colour = 'Canopy\nClosure (-)') +
  theme(legend.position = 'bottom') +
  facet_wrap(~inst_type) +
  # theme(legend.position = 'none') +
  scale_color_manual(values = cc_colours)

# plot avg troughs alongside the tree ----

event_df_avg_troughs_here <- event_df_avg_troughs |>
  mutate(inst_type = 'Subcanopy Lysimeter Mean')|>
  select(datetime, w_tree_event, cuml_snow, cuml_int = cuml_int_troughs, inst_type)

rbind(event_df_avg_troughs_here, event_df_w_tree |> select(-cc)) |>
  group_by(datetime, w_tree_event, inst_type) |>
  summarise(cuml_int = mean(cuml_int, na.rm = T),
            cuml_snow = mean(cuml_snow, na.rm = T)) |>
  ggplot(aes(cuml_snow, cuml_int, group = w_tree_event)) +
  geom_line(alpha = 0.7) +
  # geom_smooth(method = 'lm', formula = y ~ x - 1, se = F, color = 'blue', linetype = 'dashed') +
  # geom_ribbon(aes(ymin = lm_tree - (lm_tree*0.3),
  #                 ymax = lm_tree + (lm_tree*0.3)), alpha = 0.3) +
  # facet_grid(~name) +
  # annotate("text", label = paste("y =", round(coef(lm)[[1]], 2), "*x"), color = "blue", parse = TRUE) +
  ylab('Canopy Snow Load (mm)') +
  xlab('Snowfall (mm)') +
  labs(colour = 'SCL\nCanopy\nClosure (-)') +
  theme(legend.position = 'bottom') +
  facet_wrap(~inst_type) +
  # theme(legend.position = 'none') +
  scale_color_manual(values = cc_colours)

ggsave('figs/automated_snowfall_event_periods/cuml_event_snowfall_canopy_storage_avg_of_scls_and_w_tree.png',
       width = 8, height = 4)
