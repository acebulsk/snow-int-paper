# plot 15 minute interval binned met data and IP

ip_y_lims <- c(0.05, 1)

mean_ip_by_trough <- met_intercept |>
  group_by(scl_names_new) |>
  summarise(IP = mean(IP))

saveRDS(mean_ip_by_trough, 'data/mean_ip_by_trough.rds')

# thresholds based on what was used in the stats script

t_th <- -6
u_th <- 1
w_th <- 10

# lysimeter data ----

## group the data ----

# could show the boxplots on top but currently we reference the error bar/mean  dots so not going through with this for now
met_intercept$t_group <- factor(ifelse(met_intercept$t < t_th, 'cold', 'warm'), levels = c('cold', 'warm'))
met_intercept$u_group <- ifelse(met_intercept$u < u_th, 'calm', 'windy')
met_intercept$w_group <- ifelse(met_intercept$weighed_tree_canopy_load_mm < w_th, 'low', 'high')

## plot select variables (air temp, wind, snow load) ----

### air temp ----

# Define x and y column names
x_col <- 't'
y_col <- 'IP'

# Add R-squared values to the dataset
lm_nest <- met_intercept |>
  group_by(scl_names_new) |>
  nest() |>
  mutate(model = map(data, ~lm(as.formula(paste(y_col, "~", x_col)), data = .x)),
         resids = map2(data, model, add_residuals),
         preds = map2(data, model, add_predictions),
         glance = map(model, broom::glance))

at_model_summaries <- lm_nest |>
  unnest(glance) |>
  mutate(n = df.residual + 2,
         name = x_col) |>
  select(scl_names_new,
         name,
         r.squared,
         adj.r.squared,
         p.value,
         n)

at_ip_smry <- met_intercept |>
  group_by(temp_labs, scl_names_new) |>
  # filter(weighed_tree_canopy_load_mm <= 5) |>
  summarise(IP_avg = mean(IP, na.rm = T),
            sd = sd(IP, na.rm = T),
            sd_low = IP_avg - sd,
            sd_hi = IP_avg + sd,
            ci_low = quantile(IP,0.05),
            ci_hi = quantile(IP, 0.95),
            n = n()) |>
  filter(n > 10)

at_ip <- met_intercept |>
  # filter(weighed_tree_canopy_load_mm <= 5) |>
  ggplot() +
  xlab(temp_bin_ax_lab) +
  geom_point(aes(x = .data[[x_col]], y = .data[[y_col]]), colour = 'dodgerblue',  alpha = 0.5, size = 0.5) +
  geom_errorbar(data = at_ip_smry, aes(x = temp_labs, ymax = sd_hi, ymin = sd_low), width = .5)  +
  geom_point(data = at_ip_smry, aes(x = temp_labs, y = IP_avg), shape = 1, size = 4) +
  ylab(ip_y_ax_lab) +
  # scale_fill_viridis_c(option = 'magma')+
  # xlim(-17, 1) +
  # ylim(ip_y_lims) +
  theme(legend.position = 'none',
        plot.margin = margin(0.5, 0.5, 0.5, .75, "cm")) +
  # geom_text(data = model_summaries,
  #           aes(x = -Inf, y = Inf,
  #               label = sprintf("R² = %.3f\np = %.3e", adj.r.squared, p.value)),
  #           hjust = -0.1, vjust = 1.1, size = 3) +
  # stat_smooth(aes(x = .data[[x_col]], y = .data[[y_col]]), method = "lm", colour = 'red', linetype = 'dashed', se = F) +  # Add regression line
  facet_grid(~scl_names_new) #+
  # geom_text(data = at_model_summaries,
  #           aes(x = -Inf, y = Inf,
  #               label = sprintf("R² = %.3f%s",
  #                               adj.r.squared,
  #                               ifelse(p.value < 0.05, "*", ""))),
  #           hjust = -0.1, vjust = 1.1, size = 3)
at_ip

### wind speed ----

# Define x and y column names
x_col <- 'u'
y_col <- 'IP'

# Add R-squared values to the dataset
lm_nest <- met_intercept |>
  group_by(scl_names_new) |>
  nest() |>
  mutate(model = map(data, ~lm(as.formula(paste(y_col, "~", x_col)), data = .x)),
         resids = map2(data, model, add_residuals),
         preds = map2(data, model, add_predictions),
         glance = map(model, broom::glance))

u_model_summaries <- lm_nest |>
  unnest(glance) |>
  mutate(n = df.residual + 2, name = x_col) |>
  select(scl_names_new, name, r.squared, adj.r.squared, p.value, n)

ws_ip_smry <- met_intercept |>
  group_by(wind_labs, scl_names_new) |>
  # filter(weighed_tree_canopy_load_mm <= 5) |>
  summarise(IP_avg = mean(IP, na.rm = T),
            sd = sd(IP, na.rm = T),
            sd_low = IP_avg - sd,
            sd_hi = IP_avg + sd,
            ci_low = quantile(IP,0.05),
            ci_hi = quantile(IP, 0.95),
            n = n()) |> filter(n > 10)

ws_ip <- met_intercept |>
  # filter(weighed_tree_canopy_load_mm <= 5) |>
  ggplot() +
  geom_point(aes(x = u, y = IP), colour = 'dodgerblue', alpha = 0.5, size = 0.5)+
  geom_errorbar(data = ws_ip_smry, aes(x = wind_labs, ymax = sd_hi, ymin = sd_low), width = .1)  +
  geom_point(data = ws_ip_smry, aes(x = wind_labs, y = IP_avg), shape = 1, size = 4) +
  ylab(ip_y_ax_lab) +
  xlab(wnd_bin_ax_lab)+
  ylim(ip_y_lims)+
  theme(plot.margin = margin(0.5, 0.5, 0.5, .75, "cm")) +
  facet_grid(~scl_names_new) #+
  # stat_smooth(aes(x = .data[[x_col]], y = .data[[y_col]]), method = "lm", colour = 'red', linetype = 'dashed', se = F) +  # Add regression line
  # geom_text(data = model_summaries,
  #           aes(x = -Inf, y = Inf,
  #               label = sprintf("R² = %.3f\np = %.3e", adj.r.squared, p.value)),
  #           hjust = -0.1, vjust = 1.1, size = 3)
  # geom_text(data = model_summaries,
  #           aes(x = -Inf, y = Inf,
  #               label = sprintf("R² = %.3f%s",
  #                               adj.r.squared,
  #                               ifelse(p.value < 0.05, "*", ""))),
  #           hjust = -0.1, vjust = 1.1, size = 3)
ws_ip

# canopy snow load ----

# Define x and y column names
x_col <- 'weighed_tree_canopy_load_mm'
y_col <- 'IP'

# Add R-squared values to the dataset
lm_nest <- met_intercept |>
  group_by(scl_names_new) |>
  nest() |>
  mutate(model = map(data, ~lm(as.formula(paste(y_col, "~", x_col)), data = .x)),
         resids = map2(data, model, add_residuals),
         preds = map2(data, model, add_predictions),
         glance = map(model, broom::glance))
w_model_summaries <- lm_nest |>
  unnest(glance)  |>
  mutate(n = df.residual + 2, name = x_col) |>
  select(scl_names_new, name, r.squared, adj.r.squared, p.value, n)

w_ip_smry <- met_intercept |>
  group_by(tree_labs, scl_names_new) |>
  # filter(u <= 2) |>
  summarise(IP_avg = mean(IP, na.rm = T),
            sd = sd(IP, na.rm = T),
            sd_low = IP_avg - sd,
            sd_hi = IP_avg + sd,
            ci_low = quantile(IP,0.05),
            ci_hi = quantile(IP, 0.95),
            n = n()) |> filter(n > 10)

w_ip <- met_intercept |>
  ggplot() +
  geom_point(aes(x = weighed_tree_canopy_load_mm, y = IP), colour = 'dodgerblue', alpha = 0.5, size = 0.5) +
  geom_errorbar(data = w_ip_smry, aes(x = tree_labs, ymax = sd_hi, ymin = sd_low), width = .5)  +
  geom_point(data = w_ip_smry, aes(x = tree_labs, y = IP_avg), colour = 'black', shape = 1, size = 4) +
  ylab(ip_y_ax_lab) +
  xlab(expression("Initial Canopy Snow Load (kg" ~ m^{-2} * ")"))+
  ylim(ip_y_lims)+
  theme(plot.margin = margin(0.5, 0.5, 0.5, .75, "cm")) +
  facet_grid(~scl_names_new) #+
  # stat_smooth(aes(x = .data[[x_col]], y = .data[[y_col]]), method = "lm", colour = 'red', linetype = 'dashed', se = F) +  # Add regression line
# geom_text(data = model_summaries,
#           aes(x = -Inf, y = Inf,
#               label = sprintf("R² = %.3f%s",
#                               adj.r.squared,
#                               ifelse(p.value < 0.05, "*", ""))),
#           hjust = -0.1, vjust = 1.1, size = 3)
  # geom_text(data = model_summaries,
  #           aes(x = -Inf, y = Inf,
  #               label = sprintf("R² = %.3f%s",
  #                               adj.r.squared,
  #                               ifelse(p.value < 0.05, "*", ""))),
  #           hjust = -0.1, vjust = 1.1, size = 3)

w_ip
plotly::ggplotly()
cowplot::plot_grid(at_ip, ws_ip, w_ip, nrow = 3, labels = 'AUTO')

ggsave('figs/automated_snowfall_event_periods/troughs_met_vs_IP_bin.png', device = png, width = 8.5, height = 8.5)

# write out regression stats table

model_summaries <- rbind(
  at_model_summaries,
  u_model_summaries,
  w_model_summaries
) |> left_join(var_name_dict) |>
  left_join(scl_lai_cc, by = c('scl_names_new' = 'Name')) |>
  select(pretty_name,
         scl_names_new,
         cc,
         r.squared,
         adj.r.squared, p.value, n)

saveRDS(model_summaries,
        'data/lysimeter-data/lysimter_15min_avg_regression_stats.rds')

# other plots not used currently ----
#
# sf_ip_smry <- met_intercept |>
#   group_by(q_sf_labs, scl_names_new) |>
#   # filter(weighed_tree_canopy_load_mm <= 5) |>
#   summarise(IP_avg = mean(IP, na.rm = T),
#             sd = sd(IP, na.rm = T),
#             sd_low = IP_avg - sd,
#             sd_hi = IP_avg + sd,
#             ci_low = quantile(IP,0.05),
#             ci_hi = quantile(IP, 0.95),
#             n = n())
#
# sf_ip <- met_intercept |>
#   # filter(weighed_tree_canopy_load_mm <= 5) |>
#   ggplot() +
#   geom_point(aes(x = q_sf, y = IP), colour = 'dodgerblue',  alpha = 0.5, size = 0.5)+
#   geom_errorbar(data = sf_ip_smry, aes(x = q_sf_labs, ymax = sd_hi, ymin = sd_low), width = .5)  +
#   geom_point(data = sf_ip_smry, aes(x = q_sf_labs, y = IP_avg), shape = 1, size = 4) +
#   ylab(ip_y_ax_lab) +
#   xlab('Snowfall Rate (mm/hr)') +
#   # scale_fill_viridis_c(option = 'magma')+
#   # xlim(NA, 0) +
#   # ylim(ip_y_lims) +
#   theme(legend.position = 'none',
#         plot.margin = margin(0.5, 0.5, 0.5, .75, "cm")) +
#   facet_grid(~scl_names_new)
# sf_ip
#
#
#
# tice_ip_smry <- met_intercept |>
#   group_by(t_ice_labs, scl_names_new) |>
#   # filter(weighed_tree_canopy_load_mm <= 5) |>
#   summarise(IP_avg = mean(IP, na.rm = T),
#             sd = sd(IP, na.rm = T),
#             sd_low = IP_avg - sd,
#             sd_hi = IP_avg + sd,
#             ci_low = quantile(IP,0.05),
#             ci_hi = quantile(IP, 0.95),
#             n = n())
#
# tice_ip <- met_intercept |>
#   # filter(weighed_tree_canopy_load_mm <= 5) |>
#   ggplot() +
#   geom_point(aes(x = t_ice_bulb, y = IP), colour = 'dodgerblue',  alpha = 0.5, size = 0.5)+
#   geom_errorbar(data = tice_ip_smry, aes(x = t_ice_labs, ymax = sd_hi, ymin = sd_low), width = .5)  +
#   geom_point(data = tice_ip_smry, aes(x = t_ice_labs, y = IP_avg), shape = 1, size = 4) +
#   ylab(ip_y_ax_lab) +
#   xlab("") +
#   # scale_fill_viridis_c(option = 'magma')+
#   xlim(NA, 0) +
#   # ylim(ip_y_lims) +
#   theme(legend.position = 'none',
#         plot.margin = margin(0.5, 0.5, 0.5, .75, "cm")) +
#   facet_grid(~scl_names_new)
# tice_ip
#
# tice_dep_ip_smry <- met_intercept |>
#   group_by(t_ice_dep_labs, scl_names_new) |>
#   # filter(weighed_tree_canopy_load_mm <= 5) |>
#   summarise(IP_avg = mean(IP, na.rm = T),
#             sd = sd(IP, na.rm = T),
#             sd_low = IP_avg - sd,
#             sd_hi = IP_avg + sd,
#             ci_low = quantile(IP,0.05),
#             ci_hi = quantile(IP, 0.95),
#             n = n())
#
# tice_ip <- met_intercept |>
#   # filter(weighed_tree_canopy_load_mm <= 5) |>
#   ggplot() +
#   geom_point(aes(x = t_ice_dep, y = IP), colour = 'dodgerblue',  alpha = 0.5, size = 0.5)+
#   geom_errorbar(data = tice_dep_ip_smry, aes(x = t_ice_dep_labs, ymax = sd_hi, ymin = sd_low), width = .5)  +
#   geom_point(data = tice_dep_ip_smry, aes(x = t_ice_dep_labs, y = IP_avg), shape = 1, size = 4) +
#   ylab(ip_y_ax_lab) +
#   xlab("") +
#   # scale_fill_viridis_c(option = 'magma')+
#   # xlim(NA, 0) +
#   # ylim(ip_y_lims) +
#   theme(legend.position = 'none',
#         plot.margin = margin(0.5, 0.5, 0.5, .75, "cm")) +
#   facet_grid(~scl_names_new)
# tice_ip
#
# rh_ip_smry <- met_intercept |>
#   group_by(rh_labs, scl_names_new) |>
#   # filter(weighed_tree_canopy_load_mm <= 5) |>
#   summarise(IP_avg = mean(IP, na.rm = T),
#             sd = sd(IP, na.rm = T),
#             sd_low = IP_avg - sd,
#             sd_hi = IP_avg + sd,
#             ci_low = quantile(IP,0.05),
#             ci_hi = quantile(IP, 0.95),
#             n = n()) |>
#   filter(rh_labs > 65)
#
# rh_ip <- met_intercept |>
#   # filter(weighed_tree_canopy_load_mm <= 5) |>
#   ggplot() +
#   geom_point(aes(x = rh, y = IP), colour = 'dodgerblue',  alpha = 0.5, size = 0.5)+
#   geom_errorbar(data = rh_ip_smry, aes(x = rh_labs, ymax = sd_hi, ymin = sd_low), width = .5)  +
#   geom_point(data = rh_ip_smry, aes(x = rh_labs, y = IP_avg), shape = 1, size = 4) +
#   ylab(ip_y_ax_lab) +
#   xlab("Relative Humidity (%)") +
#   # scale_fill_viridis_c(option = 'magma')+
#   # xlim(NA, 0) +
#   # ylim(ip_y_lims) +
#   theme(legend.position = 'none',
#         plot.margin = margin(0.5, 0.5, 0.5, .75, "cm")) +
#   facet_grid(~scl_names_new)
# rh_ip
#
# Qsi_ip_smry <- met_intercept |>
#   group_by(Qsi_labs, scl_names_new) |>
#   # filter(weighed_tree_canopy_load_mm <= 5) |>
#   summarise(IP_avg = mean(IP, na.rm = T),
#             sd = sd(IP, na.rm = T),
#             sd_low = IP_avg - sd,
#             sd_hi = IP_avg + sd,
#             ci_low = quantile(IP,0.05),
#             ci_hi = quantile(IP, 0.95),
#             n = n()) |>
#   filter(Qsi_labs > 60)
#
# Qsi_ip <- met_intercept |>
#   # filter(weighed_tree_canopy_load_mm <= 5) |>
#   ggplot() +
#   geom_point(aes(x = Qsi, y = IP), colour = 'dodgerblue',  alpha = 0.5, size = 0.5)+
#   geom_errorbar(data = Qsi_ip_smry, aes(x = Qsi_labs, ymax = sd_hi, ymin = sd_low), width = .5)  +
#   geom_point(data = Qsi_ip_smry, aes(x = Qsi_labs, y = IP_avg), shape = 1, size = 4) +
#   ylab(ip_y_ax_lab) +
#   xlab("Solar Radiation (W m-2)") +
#   # scale_fill_viridis_c(option = 'magma')+
#   # xlim(NA, 0) +
#   # ylim(ip_y_lims) +
#   theme(legend.position = 'none',
#         plot.margin = margin(0.5, 0.5, 0.5, .75, "cm")) +
#   facet_grid(~scl_names_new)
# Qsi_ip
