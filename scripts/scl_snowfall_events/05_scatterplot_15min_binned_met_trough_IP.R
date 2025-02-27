# plot 15 minute interval binned met data and IP

ip_y_lims <- c(0.05, 1)

mean_ip_by_trough <- met_intercept |>
  group_by(trough_name) |>
  summarise(IP = mean(IP))

saveRDS(mean_ip_by_trough, 'data/mean_ip_by_trough.rds')

met_intercept$trough_name <- paste0(toupper(substr(met_intercept$trough_name, 1, 1)), substr(met_intercept$trough_name, 2, nchar(met_intercept$trough_name)))
met_intercept$trough_name <- factor(met_intercept$trough_name, levels = c('Sparse', 'Mixed', 'Closed'))
scl_lai_cc_fltr$trough_name <- paste0(toupper(substr(scl_lai_cc_fltr$trough_name, 1, 1)), substr(scl_lai_cc_fltr$trough_name, 2, nchar(scl_lai_cc_fltr$trough_name)))
# lysimeter data ----

## group the data ----



## plot select variables (air temp, wind, snow load) ----

### air temp ----

# Define x and y column names
x_col <- 't'
y_col <- 'IP'

# Add R-squared values to the dataset
lm_nest <- met_intercept |>
  group_by(trough_name) |>
  nest() |>
  mutate(model = map(data, ~lm(as.formula(paste(y_col, "~", x_col)), data = .x)),
         resids = map2(data, model, add_residuals),
         preds = map2(data, model, add_predictions),
         glance = map(model, broom::glance))

at_model_summaries <- lm_nest |>
  unnest(glance) |>
  mutate(n = df.residual + 2,
         name = x_col) |>
  select(trough_name,
         name,
         r.squared,
         adj.r.squared,
         p.value,
         n)

at_ip_smry <- met_intercept |>
  group_by(temp_labs, trough_name) |>
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
  facet_grid(~trough_name) #+
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
  group_by(trough_name) |>
  nest() |>
  mutate(model = map(data, ~lm(as.formula(paste(y_col, "~", x_col)), data = .x)),
         resids = map2(data, model, add_residuals),
         preds = map2(data, model, add_predictions),
         glance = map(model, broom::glance))

u_model_summaries <- lm_nest |>
  unnest(glance) |>
  mutate(n = df.residual + 2, name = x_col) |>
  select(trough_name, name, r.squared, adj.r.squared, p.value, n)

ws_ip_smry <- met_intercept |>
  group_by(wind_labs, trough_name) |>
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
  facet_grid(~trough_name) #+
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
  group_by(trough_name) |>
  nest() |>
  mutate(model = map(data, ~lm(as.formula(paste(y_col, "~", x_col)), data = .x)),
         resids = map2(data, model, add_residuals),
         preds = map2(data, model, add_predictions),
         glance = map(model, broom::glance))
w_model_summaries <- lm_nest |>
  unnest(glance)  |>
  mutate(n = df.residual + 2, name = x_col) |>
  select(trough_name, name, r.squared, adj.r.squared, p.value, n)

w_ip_smry <- met_intercept |>
  group_by(tree_labs, trough_name) |>
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
  xlab("Initial Canopy Snow Load (mm)")+
  # xlab(expression("Initial Canopy Snow Load (kg" ~ m^{-2} * ")"))+
  ylim(ip_y_lims)+
  theme(plot.margin = margin(0.5, 0.5, 0.5, .75, "cm")) +
  facet_grid(~trough_name) #+
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
cowplot::plot_grid(at_ip, ws_ip, w_ip, nrow = 3, labels = c('a', 'b', 'c'))

ggsave('figs/automated_snowfall_event_periods/troughs_met_vs_IP_bin.png', device = png, width = 8.5, height = 8.5)

# write out regression stats table

model_summaries <- rbind(
  at_model_summaries,
  u_model_summaries,
  w_model_summaries
) |> left_join(var_name_dict) |>
  left_join(scl_lai_cc_fltr) |>
  select(pretty_name,
         trough_name,
         cc,
         r.squared,
         adj.r.squared, p.value, n)

saveRDS(model_summaries,
        'data/lysimeter-data/processed/lysimter_15min_avg_regression_stats.rds')

# check interaction of variables ----

ggplot(met_intercept, aes(x = wind_labs, y = temp_labs, fill = IP)) +
  geom_tile() +
  labs(fill='I/P') +
  scale_fill_viridis_c(option = "A") +
  facet_grid(~trough_name)

ggplot(met_intercept, aes(x = tree_labs, y = temp_labs, fill = IP)) +
  geom_tile() +
  labs(fill='I/P') +
  scale_fill_viridis_c(option = "A") +
  facet_grid(~trough_name)

ggplot(met_intercept, aes(x = tree_labs, y = wind_labs, fill = IP)) +
  geom_tile() +
  labs(fill='I/P') +
  scale_fill_viridis_c(option = "A") +
  facet_grid(~trough_name)

# run stats on interaction ----
# based on above showing higher IP for low temps and low wind or high temps and high wind
# Define x and y column names
x1_col <- 't'
x2_col <- 'u'
y_col <- 'IP'

library(broom)
lm_nest <- met_intercept |>
  group_by(trough_name) |>
  nest() |>
  mutate(
    # The formula below creates an interaction model: IP ~ t * wind. The formula
    # t * wind in R expands to t + wind + t:wind, which includes both main
    # effects and their interaction.
    model = map(data, ~ lm(as.formula(
      paste(y_col, "~", paste0(x1_col, "*", x2_col))
    ), data = .x)),
    resids = map2(data, model, add_residuals),
    preds = map2(data, model, add_predictions),
    glance = map(model, broom::glance)
  )

at_model_summaries <- lm_nest |>
  unnest(glance) |>
  mutate(n = df.residual + 2,
         name = x_col) |>
  select(trough_name,
         name,
         r.squared,
         adj.r.squared,
         p.value,
         n)

# Assuming lm_nest_interaction is your nested data frame with models including t * wind
interaction_summary <- lm_nest %>%
  mutate(tidy_model = map(model, broom::tidy)) %>%
  unnest(tidy_model) %>%
  filter(term == "t:u") %>%  # adjust the term name if your predictors have different names
  select(trough_name, term, estimate, std.error, statistic, p.value)

print(interaction_summary)

# interaction term is not significant above !!!

# Kruskal Wallis / Pairwise Wilcox ----
# since or independent variables are not normally distributed maybe a
# non-parameteric test is more appropriate

# thresholds based on what was used in the stats script

t_th <- -6
u_th <- 2
w_th <- 10

met_intercept$t_group <- factor(ifelse(met_intercept$t < t_th, 'cold', 'warm'), levels = c('cold', 'warm'))
met_intercept$u_group <- ifelse(met_intercept$u < u_th, 'calm', 'windy')
met_intercept$w_group <- ifelse(met_intercept$weighed_tree_canopy_load_mm < w_th, 'low', 'high')

met_intercept <- met_intercept %>%
  mutate(
    interaction_group = interaction(t_group, u_group)
  )

median_IPs <- met_intercept %>%
  group_by(interaction_group, trough_name) %>%
  summarise(median_IP = median(IP, na.rm = TRUE))

print(median_IPs)

## sparse trough ----
sparse_data <- met_intercept |> filter(trough_name == 'Sparse')
kruskal.test(IP ~ interaction_group, data = sparse_data)

pairwise.wilcox.test(sparse_data$IP, sparse_data$interaction_group,
                     p.adjust.method = "bonferroni")
# above shows significantly greater I/P for cold calm compared to warm calm
# storms and significantly greater IP for warm windy compared to cold calm
# no difference between cold windy and warm windy
median_IPs |> filter(trough_name == 'Sparse')

## mixed trough ----
mixed_data <- met_intercept |> filter(trough_name == 'Mixed')
kruskal.test(IP ~ interaction_group, data = mixed_data)

pairwise.wilcox.test(mixed_data$IP, mixed_data$interaction_group,
                     p.adjust.method = "bonferroni")
# above shows significantly greater I/P for cold calm compared to all types
median_IPs |> filter(trough_name == 'Mixed')

## closed trough ----
closed_data <- met_intercept |> filter(trough_name == 'Closed') |>
  filter(interaction_group != 'cold.windy') # not enough obs for cold windy
kruskal.test(IP ~ interaction_group, data = closed_data)

pairwise.wilcox.test(closed_data$IP, closed_data$interaction_group,
                     p.adjust.method = "bonferroni")
# above shows significantly greater I/P for both warm windy compared to cold calm and warm calm
median_IPs |> filter(trough_name == 'Closed')

#
# sf_ip_smry <- met_intercept |>
#   group_by(q_sf_labs, trough_name) |>
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
#   facet_grid(~trough_name)
# sf_ip
#
#
#
# tice_ip_smry <- met_intercept |>
#   group_by(t_ice_labs, trough_name) |>
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
#   facet_grid(~trough_name)
# tice_ip
#
# tice_dep_ip_smry <- met_intercept |>
#   group_by(t_ice_dep_labs, trough_name) |>
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
#   facet_grid(~trough_name)
# tice_ip
#
# rh_ip_smry <- met_intercept |>
#   group_by(rh_labs, trough_name) |>
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
#   facet_grid(~trough_name)
# rh_ip
#
# Qsi_ip_smry <- met_intercept |>
#   group_by(Qsi_labs, trough_name) |>
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
#   facet_grid(~trough_name)
# Qsi_ip
