# plot binned met data and IP

library(tidyverse)
library(ggpubr)

ip_y_lims <- c(0.05, 1)

met_intercept <- readRDS('../../analysis/interception/data/storm_analysis/continuous_throughfall_data_binned_met_select_events.rds')  |>
  filter(q_sf > 0,
         q_sf > q_tf) |>
  mutate(
    q_int = q_sf - q_tf,
    IP = q_int / q_sf) |>
  filter(IP < 1)

# lysimeter data ----

at_ip <- met_intercept |>
  # group_by(temp_labs) |>
  # filter(u <= 2) |>
  # summarise(IP_avg = mean(IP, na.rm = T),
  #           ci_low = quantile(IP,0.05),
  #           ci_hi = quantile(IP, 0.95),
  #           n = n()) |>
  # filter(n > 10) |>
  ggplot(aes(x = temp_labs, y = IP, group = temp_labs)) +
  # ggplot(aes(x = temp_labs, y = IP, group = temp_labs, fill = temp_labs)) +
  # geom_point(alpha=0.2, colour = "#56B4E9")+
  geom_boxplot(width = 1)+
  # geom_point() +
  # geom_errorbar(aes(ymax = ci_hi, ymin = ci_low))  +
  ylab(ip_y_ax_lab) +
  xlab(temp_bin_ax_lab) +
  # scale_fill_viridis_c(option = 'magma')+
  xlim(NA, 0.5) +
  ylim(ip_y_lims) +
  theme(legend.title = element_blank(),
        plot.margin = margin(0.5, 0.5, 0.5, .75, "cm"))

ws_ip <- met_intercept |>
  group_by(wind_labs) |>
  #   # filter(u <= 2) |>
  # summarise(IP_avg = mean(IP, na.rm = T),
  #           ci_low = quantile(IP,0.05),
  #           ci_hi = quantile(IP, 0.95),
  #           n = n()) |>
  mutate(n = n()) |>
  filter(n > 4) |>
  ggplot(aes(wind_labs, IP, group = wind_labs)) +
  # geom_point(alpha = 0.2, colour = "#56B4E9") +
  geom_boxplot()+
  # geom_errorbar(aes(ymax = ci_hi, ymin = ci_low))  +
  ylab(ip_y_ax_lab) +
  xlab(wnd_bin_ax_lab)+
  ylim(ip_y_lims)+
  theme(plot.margin = margin(0.5, 0.5, 0.5, .75, "cm"))

w_ip <- met_intercept |>
  group_by(tree_labs) |>
  # summarise(IP_avg = mean(IP, na.rm = T),
  #           ci_low = quantile(IP,0.05),
  #           ci_hi = quantile(IP, 0.95),
  #           n = n()) |>
  mutate(n = n()) |>
  # filter(n > 10) |>
  ggplot(aes(tree_labs, IP, group = tree_labs)) +
  # geom_point(alpha = 0.5, colour = "#56B4E9") +
  # geom_errorbar(aes(ymax = ci_hi, ymin = ci_low))  +
  geom_boxplot() +
  ylab(ip_y_ax_lab) +
  xlab('Canopy Snow Load (mm)')+
  ylim(ip_y_lims)+
  theme(plot.margin = margin(0.5, 0.5, 0.5, .75, "cm"))


cowplot::plot_grid(at_ip, ws_ip, w_ip, nrow = 3, labels = 'AUTO')

ggsave('figs/interception/troughs_met_vs_IP_bin.png', device = png, width = 8.5, height = 7)
