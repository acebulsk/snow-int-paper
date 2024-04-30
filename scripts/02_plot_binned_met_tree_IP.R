# plot binned met data and IP

library(tidyverse)
library(ggpubr)

ip_y_lims <- c(0.05, 1)

# calc tree interception efficiency

met_intercept_tree <- readRDS('../../analysis/interception/data/storm_analysis/continuous_throughfall_data_binned_met_select_events.rds') |>
  left_join(q_int_tree) |>
  mutate(q_int_tree = q_int_tree * 4,
         lag_weighed_tree_canopy_load_mm = lag(weighed_tree_canopy_load_mm),
         q_int_troughs = q_sf - q_tf) |>  # mm/15min to mm/hr
  filter(q_sf > 0,
         q_int_tree > 0) |>
  mutate(
    IP = q_int_tree / q_sf) |>
  filter(IP < 1)

ggplot(met_intercept_tree |> filter(q_int_troughs > 0), aes(q_int_tree, q_int_troughs, colour = name)) +
  geom_point() +
  geom_abline()

# tree data ----

at_ip_smry <- met_intercept_tree |>
  group_by(temp_labs) |>
  # filter(weighed_tree_canopy_load_mm <= 5) |>
  summarise(IP_avg = mean(IP, na.rm = T),
            sd = sd(IP, na.rm = T),
            sd_low = IP_avg - sd,
            sd_hi = IP_avg + sd,
            ci_low = quantile(IP,0.05),
            ci_hi = quantile(IP, 0.95),
            n = n())

at_ip <- met_intercept_tree |>
  # filter(weighed_tree_canopy_load_mm <= 5) |>
  ggplot() +
  geom_point(aes(x = t, y = IP), colour = 'dodgerblue',  alpha = 0.5, size = 0.5)+
  geom_errorbar(data = at_ip_smry, aes(x = temp_labs, ymax = sd_hi, ymin = sd_low), width = .5)  +
  geom_point(data = at_ip_smry, aes(x = temp_labs, y = IP_avg), shape = 1, size = 4) +
  ylab(ip_y_ax_lab) +
  xlab(temp_bin_ax_lab) +
  # scale_fill_viridis_c(option = 'magma')+
  xlim(NA, 0) +
  # ylim(ip_y_lims) +
  theme(legend.title = element_blank(),
        plot.margin = margin(0.5, 0.5, 0.5, .75, "cm"))
at_ip

ws_ip_smry <- met_intercept_tree |>
  group_by(wind_labs) |>
  # filter(weighed_tree_canopy_load_mm <= 5) |>
  summarise(IP_avg = mean(IP, na.rm = T),
            sd = sd(IP, na.rm = T),
            sd_low = IP_avg - sd,
            sd_hi = IP_avg + sd,
            ci_low = quantile(IP,0.05),
            ci_hi = quantile(IP, 0.95),
            n = n()) |> filter(n > 10)

ws_ip <- met_intercept_tree |>
  # filter(weighed_tree_canopy_load_mm <= 5) |>
  ggplot() +
  geom_point(aes(x = u, y = IP), colour = 'dodgerblue', alpha = 0.5, size = 0.5)+
  geom_errorbar(data = ws_ip_smry, aes(x = wind_labs, ymax = sd_hi, ymin = sd_low), width = .1)  +
  geom_point(data = ws_ip_smry, aes(x = wind_labs, y = IP_avg), shape = 1, size = 4) +
  ylab(ip_y_ax_lab) +
  xlab(wnd_bin_ax_lab)+
  ylim(ip_y_lims)+
  theme(plot.margin = margin(0.5, 0.5, 0.5, .75, "cm"))
ws_ip

w_ip_smry <- met_intercept_tree |>
  group_by(tree_labs) |>
  # filter(u <= 2) |>
  summarise(IP_avg = mean(IP, na.rm = T),
            sd = sd(IP, na.rm = T),
            sd_low = IP_avg - sd,
            sd_hi = IP_avg + sd,
            ci_low = quantile(IP,0.05),
            ci_hi = quantile(IP, 0.95),
            n = n()) |> filter(n > 10)

w_ip <- met_intercept_tree |>
  ggplot() +
  geom_point(aes(x = lag_weighed_tree_canopy_load_mm, y = IP), colour = 'dodgerblue', alpha = 0.5, size = 0.5) +
  geom_errorbar(data = w_ip_smry, aes(x = tree_labs, ymax = sd_hi, ymin = sd_low), width = .5)  +
  geom_point(data = w_ip_smry, aes(x = tree_labs, y = IP_avg), colour = 'black', shape = 1, size = 4) +
  ylab(ip_y_ax_lab) +
  xlab(w_ax_lab)+
  ylim(ip_y_lims)+
  theme(plot.margin = margin(0.5, 0.5, 0.5, .75, "cm"))

w_ip

cowplot::plot_grid(at_ip, ws_ip, w_ip, nrow = 3, labels = 'AUTO')

ggsave('figs/automated_snowfall_event_periods/tree_met_vs_IP_bin.png', device = png, width = 8.5, height = 7)
