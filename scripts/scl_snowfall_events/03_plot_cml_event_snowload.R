# Plot cumulative canopy load over the snowfall periods using the SCLs

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
scl_lai_cc_fltr <- scl_lai_cc |>
  arrange(cc) |>
  mutate(cc = factor(round(cc, 2), levels = sort(unique(round(cc, 2)))))

scl_lai_cc_fltr$trough_name <- c('sparse_forest', 'medium_density_forest', 'dense_forest')

event_df_sep_troughs <- storm_dates_long |>
  left_join(met_df) |>
  left_join(q_tf_scl) |>
  left_join(scl_lai_cc_fltr) |>
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
