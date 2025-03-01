# plot the interception efficienty for various met bin I/P is calculated for
# each bin using accumulated precip and throughfall over the bin which reduces
# the relative instrument error. The error bars below show the absolute
# instrument error which is higher when the accumulated precip/tf is lower OR
# the load measured in the pluvio/SCL is very high

ip_y_ax_lab <- 'Interception Efficiency (-)'

ip_y_lims <- c(0, 1)

# lysimeter data ----

## group the data ----

scl_lai_cc_fltr$trough_name <- paste0(toupper(substr(scl_lai_cc_fltr$trough_name, 1, 1)), substr(scl_lai_cc_fltr$trough_name, 2, nchar(scl_lai_cc_fltr$trough_name)))

## plot select variables (air temp, wind, snow load) ----

### air temp ----

# Define x and y column names
x_col <- 't'
y_col <- 'bin_IP'

at_ip_smry <- met_intercept |>
  group_by(temp_labs, trough_name) |>
  # filter(weighed_tree_canopy_load_mm <= 5) |>
  summarise(
    surface_area = mean(surface_area),
    bin_del_sf = sum(event_del_sf),
    bin_del_tf = sum(event_del_tf),
    scl_abs_accuracy = prop_err_sum(mean(scl_abs_accuracy), mean(scl_abs_accuracy)), # calculated as the difference over an interval, i.e. as if we filtered to each temperature bin and then differenced the start/end accumulated precip/SCL measurement
    pluvio_abs_accuracy = prop_err_sum(mean(pluvio_abs_accuracy), mean(pluvio_abs_accuracy))) |>
  # scl_abs_accuracy = sqrt(sum(scl_abs_accuracy^2)), # +/- kg # this is an over estimate of the uncertainty because the interval measurements could have been taken at a coarser resolution...
  # pluvio_abs_accuracy = sqrt(sum(pluvio_abs_accuracy^2))) |>  # +/- mm) |>
  mutate(
    bin_del_i = bin_del_sf - bin_del_tf,
    bin_IP = bin_del_i/bin_del_sf,
    sigma_bin_del_i = prop_err_sum(scl_abs_accuracy, pluvio_abs_accuracy),
    sigma_bin_IP = prop_err_ratio(bin_IP, bin_del_i, bin_del_i, sigma_bin_del_i, pluvio_abs_accuracy),
    sigma_bin_IP_hi = bin_IP+sigma_bin_IP,
    sigma_bin_IP_hi = ifelse(sigma_bin_IP_hi > 1, 1, sigma_bin_IP_hi),
    sigma_bin_IP_lo = bin_IP-sigma_bin_IP,
    sigma_bin_IP_lo = ifelse(sigma_bin_IP_lo<0,0,sigma_bin_IP_lo),
    scl_rel_perc_error = (scl_abs_accuracy/(bin_del_tf*surface_area))*100,
    pluvio_rel_perc_error = (pluvio_abs_accuracy/ bin_del_sf)*100,
    scl_flag = ifelse(scl_rel_perc_error > perc_err_fltr, T, F),
    pluvio_flag = ifelse(pluvio_rel_perc_error > perc_err_fltr, T, F)
  ) |>
  filter(scl_flag == F & pluvio_flag == F)

at_ip <- met_intercept |>
  # filter(weighed_tree_canopy_load_mm <= 5) |>
  ggplot() +
  xlab(temp_bin_ax_lab) +
  # geom_point(aes(x = .data[[x_col]], y = .data[[y_col]]), colour = 'dodgerblue',  alpha = 0.5, size = 0.5) +
  geom_errorbar(data = at_ip_smry, aes(x = temp_labs, ymax = sigma_bin_IP_hi, ymin = sigma_bin_IP_lo), width = 1, alpha = 0.5)  +
  geom_point(data = at_ip_smry, aes(x = temp_labs, y = bin_IP), size = 2) +
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

ws_ip_smry <- met_intercept |>
  group_by(wind_labs, trough_name) |>
  # filter(weighed_tree_canopy_load_mm <= 5) |>
  summarise(
    surface_area = mean(surface_area),
    bin_del_sf = sum(event_del_sf),
    bin_del_tf = sum(event_del_tf),
    scl_abs_accuracy = prop_err_sum(mean(scl_abs_accuracy), mean(scl_abs_accuracy)), # calculated as the difference over an interval
    pluvio_abs_accuracy = prop_err_sum(mean(pluvio_abs_accuracy), mean(pluvio_abs_accuracy))) |>
  # scl_abs_accuracy = sqrt(sum(scl_abs_accuracy^2)), # +/- kg # this is an over estimate of the uncertainty because the interval measurements could have been taken at a coarser resolution...
  # pluvio_abs_accuracy = sqrt(sum(pluvio_abs_accuracy^2))) |>  # +/- mm) |>
  mutate(
    bin_del_i = bin_del_sf - bin_del_tf,
    bin_IP = bin_del_i/bin_del_sf,
    sigma_bin_del_i = prop_err_sum(scl_abs_accuracy, pluvio_abs_accuracy),
    sigma_bin_IP = prop_err_ratio(bin_IP, bin_del_i, bin_del_i, sigma_bin_del_i, pluvio_abs_accuracy),
    sigma_bin_IP_hi = bin_IP+sigma_bin_IP,
    sigma_bin_IP_hi = ifelse(sigma_bin_IP_hi > 1, 1, sigma_bin_IP_hi),
    sigma_bin_IP_lo = bin_IP-sigma_bin_IP,
    sigma_bin_IP_lo = ifelse(sigma_bin_IP_lo<0,0,sigma_bin_IP_lo),
    scl_rel_perc_error = (scl_abs_accuracy/(bin_del_tf*surface_area))*100,
    pluvio_rel_perc_error = (pluvio_abs_accuracy/ bin_del_sf)*100,
    scl_flag = ifelse(scl_rel_perc_error > perc_err_fltr, T, F),
    pluvio_flag = ifelse(pluvio_rel_perc_error > perc_err_fltr, T, F)
  )|>
  filter(scl_flag == F & pluvio_flag == F)

ws_ip <- met_intercept |>
  # filter(weighed_tree_canopy_load_mm <= 5) |>
  ggplot() +
  # geom_point(aes(x = u, y = IP), colour = 'dodgerblue', alpha = 0.5, size = 0.5)+
  geom_errorbar(data = ws_ip_smry, aes(x = wind_labs, ymax = sigma_bin_IP_hi, ymin = sigma_bin_IP_lo), width = .255, alpha = 0.5)  +
  geom_point(data = ws_ip_smry, aes(x = wind_labs, y = bin_IP), size = 2) +
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

w_ip_smry <- met_intercept |>
  group_by(tree_labs, trough_name) |>
  # filter(weighed_tree_canopy_load_mm <= 5) |>
  summarise(
    surface_area = mean(surface_area),
    bin_del_sf = sum(event_del_sf),
    bin_del_tf = sum(event_del_tf),
    scl_abs_accuracy = prop_err_sum(mean(scl_abs_accuracy), mean(scl_abs_accuracy)), # calculated as the difference over an interval
    pluvio_abs_accuracy = prop_err_sum(mean(pluvio_abs_accuracy), mean(pluvio_abs_accuracy))) |>
  # scl_abs_accuracy = sqrt(sum(scl_abs_accuracy^2)), # +/- kg # this is an over estimate of the uncertainty because the interval measurements could have been taken at a coarser resolution...
  # pluvio_abs_accuracy = sqrt(sum(pluvio_abs_accuracy^2))) |>  # +/- mm) |>
  mutate(
    bin_del_i = bin_del_sf - bin_del_tf,
    bin_IP = bin_del_i/bin_del_sf,
    sigma_bin_del_i = prop_err_sum(scl_abs_accuracy, pluvio_abs_accuracy),
    sigma_bin_IP = prop_err_ratio(bin_IP, bin_del_i, bin_del_i, sigma_bin_del_i, pluvio_abs_accuracy),
    sigma_bin_IP_hi = bin_IP+sigma_bin_IP,
    sigma_bin_IP_hi = ifelse(sigma_bin_IP_hi > 1, 1, sigma_bin_IP_hi),
    sigma_bin_IP_lo = bin_IP-sigma_bin_IP,
    sigma_bin_IP_lo = ifelse(sigma_bin_IP_lo<0,0,sigma_bin_IP_lo),
    scl_rel_perc_error = (scl_abs_accuracy/(bin_del_tf*surface_area))*100,
    pluvio_rel_perc_error = (pluvio_abs_accuracy/ bin_del_sf)*100,
    scl_flag = ifelse(scl_rel_perc_error > perc_err_fltr, T, F),
    pluvio_flag = ifelse(pluvio_rel_perc_error > perc_err_fltr, T, F)
  ) |>
  filter(scl_flag == F & pluvio_flag == F)

w_ip <- met_intercept |>
  ggplot() +
  geom_errorbar(data = w_ip_smry, aes(x = tree_labs, ymax = sigma_bin_IP_hi, ymin = sigma_bin_IP_lo), width = 0.75, alpha = 0.5)  +
  geom_point(data = w_ip_smry, aes(x = tree_labs, y = bin_IP), size = 2) +
  ylab(ip_y_ax_lab) +
  xlab("Initial Canopy Snow Load (mm)")+
  # xlab(expression("Initial Canopy Snow Load (kg" ~ m^{-2} * ")"))+
  ylim(ip_y_lims)+
  theme(plot.margin = margin(0.5, 0.5, 0.5, .75, "cm")) +
  facet_grid(~trough_name) #+

w_ip
plotly::ggplotly()
cowplot::plot_grid(at_ip, ws_ip, w_ip, nrow = 3, labels = c('a', 'b', 'c'))

ggsave('figs/automated_snowfall_event_periods/troughs_met_vs_IP_accumulated_bin.png', device = png, width = 8.5, height = 8.5)


# check out other vars ----

# # Define x and y column names
# x_col <- 'rh_labs'
# y_col <- 'IP'
#
# ip_smry <- met_intercept |>
#   group_by(.data[[x_col]], trough_name) |>
#   # filter(weighed_tree_canopy_load_mm <= 5) |>
#   summarise(
#     surface_area = mean(surface_area),
#     bin_del_sf = sum(event_del_sf),
#     bin_del_tf = sum(event_del_tf),
#     scl_abs_accuracy = prop_err_sum(mean(scl_abs_accuracy), mean(scl_abs_accuracy)), # calculated as the difference over an interval
#     pluvio_abs_accuracy = prop_err_sum(mean(pluvio_abs_accuracy), mean(pluvio_abs_accuracy))) |>
#   # scl_abs_accuracy = sqrt(sum(scl_abs_accuracy^2)), # +/- kg # this is an over estimate of the uncertainty because the interval measurements could have been taken at a coarser resolution...
#   # pluvio_abs_accuracy = sqrt(sum(pluvio_abs_accuracy^2))) |>  # +/- mm) |>
#   mutate(
#     bin_del_i = bin_del_sf - bin_del_tf,
#     bin_IP = bin_del_i/bin_del_sf,
#     sigma_bin_del_i = prop_err_sum(scl_abs_accuracy, pluvio_abs_accuracy),
#     sigma_bin_IP = prop_err_ratio(bin_IP, bin_del_i, bin_del_i, sigma_bin_del_i, pluvio_abs_accuracy),
#     sigma_bin_IP_hi = bin_IP+sigma_bin_IP,
#     sigma_bin_IP_hi = ifelse(sigma_bin_IP_hi > 1, 1, sigma_bin_IP_hi),
#     sigma_bin_IP_lo = bin_IP-sigma_bin_IP,
#     sigma_bin_IP_lo = ifelse(sigma_bin_IP_lo<0,0,sigma_bin_IP_lo),
#     scl_rel_perc_error = (scl_abs_accuracy/(bin_del_tf*surface_area))*100,
#     pluvio_rel_perc_error = (pluvio_abs_accuracy/ bin_del_sf)*100,
#     scl_flag = ifelse(scl_rel_perc_error > perc_err_fltr, T, F),
#     pluvio_flag = ifelse(pluvio_rel_perc_error > perc_err_fltr, T, F)
#   )
#
# vel_ip <- ip_smry |>
#   ggplot() +
#   geom_errorbar(aes(x = .data[[x_col]], ymax = sigma_bin_IP_hi, ymin = sigma_bin_IP_lo), width = 0.75, alpha = 0.5)  +
#   geom_point(aes(x = .data[[x_col]], y = bin_IP), size = 2) +
#   ylab(ip_y_ax_lab) +
#   xlab("Initial Canopy Snow Load (mm)")+
#   # xlab(expression("Initial Canopy Snow Load (kg" ~ m^{-2} * ")"))+
#   ylim(ip_y_lims)+
#   theme(plot.margin = margin(0.5, 0.5, 0.5, .75, "cm")) +
#   facet_grid(~trough_name)
# vel_ip
#
# # Define x and y column names
# x_col <- 'part_vel_labs'
# y_col <- 'IP'
#
# ip_smry <- met_intercept |>
#   group_by(.data[[x_col]], trough_name) |>
#   # filter(weighed_tree_canopy_load_mm <= 5) |>
#   summarise(
#     surface_area = mean(surface_area),
#     bin_del_sf = sum(event_del_sf),
#     bin_del_tf = sum(event_del_tf),
#     scl_abs_accuracy = prop_err_sum(mean(scl_abs_accuracy), mean(scl_abs_accuracy)), # calculated as the difference over an interval
#     pluvio_abs_accuracy = prop_err_sum(mean(pluvio_abs_accuracy), mean(pluvio_abs_accuracy))) |>
#   # scl_abs_accuracy = sqrt(sum(scl_abs_accuracy^2)), # +/- kg # this is an over estimate of the uncertainty because the interval measurements could have been taken at a coarser resolution...
#   # pluvio_abs_accuracy = sqrt(sum(pluvio_abs_accuracy^2))) |>  # +/- mm) |>
#   mutate(
#     bin_del_i = bin_del_sf - bin_del_tf,
#     bin_IP = bin_del_i/bin_del_sf,
#     sigma_bin_del_i = prop_err_sum(scl_abs_accuracy, pluvio_abs_accuracy),
#     sigma_bin_IP = prop_err_ratio(bin_IP, bin_del_i, bin_del_i, sigma_bin_del_i, pluvio_abs_accuracy),
#     sigma_bin_IP_hi = bin_IP+sigma_bin_IP,
#     sigma_bin_IP_hi = ifelse(sigma_bin_IP_hi > 1, 1, sigma_bin_IP_hi),
#     sigma_bin_IP_lo = bin_IP-sigma_bin_IP,
#     sigma_bin_IP_lo = ifelse(sigma_bin_IP_lo<0,0,sigma_bin_IP_lo),
#     scl_rel_perc_error = (scl_abs_accuracy/(bin_del_tf*surface_area))*100,
#     pluvio_rel_perc_error = (pluvio_abs_accuracy/ bin_del_sf)*100,
#     scl_flag = ifelse(scl_rel_perc_error > perc_err_fltr, T, F),
#     pluvio_flag = ifelse(pluvio_rel_perc_error > perc_err_fltr, T, F)
#   )
#
# vel_ip <- ip_smry |>
#   ggplot() +
#   geom_errorbar(aes(x = .data[[x_col]], ymax = sigma_bin_IP_hi, ymin = sigma_bin_IP_lo), width = 0.75, alpha = 0.5)  +
#   geom_point(aes(x = .data[[x_col]], y = bin_IP), size = 2) +
#   ylab(ip_y_ax_lab) +
#   xlab("Initial Canopy Snow Load (mm)")+
#   # xlab(expression("Initial Canopy Snow Load (kg" ~ m^{-2} * ")"))+
#   ylim(ip_y_lims)+
#   theme(plot.margin = margin(0.5, 0.5, 0.5, .75, "cm")) +
#   facet_grid(~trough_name)
# vel_ip
#
# # Define x and y column names
# x_col <- 'part_diam_labs'
# y_col <- 'IP'
#
# ip_smry <- met_intercept |>
#   group_by(.data[[x_col]], trough_name) |>
#   # filter(weighed_tree_canopy_load_mm <= 5) |>
#   summarise(
#     surface_area = mean(surface_area),
#     bin_del_sf = sum(event_del_sf),
#     bin_del_tf = sum(event_del_tf),
#     scl_abs_accuracy = prop_err_sum(mean(scl_abs_accuracy), mean(scl_abs_accuracy)), # calculated as the difference over an interval
#     pluvio_abs_accuracy = prop_err_sum(mean(pluvio_abs_accuracy), mean(pluvio_abs_accuracy))) |>
#   # scl_abs_accuracy = sqrt(sum(scl_abs_accuracy^2)), # +/- kg # this is an over estimate of the uncertainty because the interval measurements could have been taken at a coarser resolution...
#   # pluvio_abs_accuracy = sqrt(sum(pluvio_abs_accuracy^2))) |>  # +/- mm) |>
#   mutate(
#     bin_del_i = bin_del_sf - bin_del_tf,
#     bin_IP = bin_del_i/bin_del_sf,
#     sigma_bin_del_i = prop_err_sum(scl_abs_accuracy, pluvio_abs_accuracy),
#     sigma_bin_IP = prop_err_ratio(bin_IP, bin_del_i, bin_del_i, sigma_bin_del_i, pluvio_abs_accuracy),
#     sigma_bin_IP_hi = bin_IP+sigma_bin_IP,
#     sigma_bin_IP_hi = ifelse(sigma_bin_IP_hi > 1, 1, sigma_bin_IP_hi),
#     sigma_bin_IP_lo = bin_IP-sigma_bin_IP,
#     sigma_bin_IP_lo = ifelse(sigma_bin_IP_lo<0,0,sigma_bin_IP_lo),
#     scl_rel_perc_error = (scl_abs_accuracy/(bin_del_tf*surface_area))*100,
#     pluvio_rel_perc_error = (pluvio_abs_accuracy/ bin_del_sf)*100,
#     scl_flag = ifelse(scl_rel_perc_error > perc_err_fltr, T, F),
#     pluvio_flag = ifelse(pluvio_rel_perc_error > perc_err_fltr, T, F)
#   )
#
# vel_ip <- ip_smry |>
#   ggplot() +
#   geom_errorbar(aes(x = .data[[x_col]], ymax = sigma_bin_IP_hi, ymin = sigma_bin_IP_lo), width = 0.75, alpha = 0.5)  +
#   geom_point(aes(x = .data[[x_col]], y = bin_IP), size = 2) +
#   ylab(ip_y_ax_lab) +
#   xlab("Initial Canopy Snow Load (mm)")+
#   # xlab(expression("Initial Canopy Snow Load (kg" ~ m^{-2} * ")"))+
#   ylim(ip_y_lims)+
#   theme(plot.margin = margin(0.5, 0.5, 0.5, .75, "cm")) +
#   facet_grid(~trough_name)
# vel_ip
