# plot binned met data and IP

library(tidyverse)
library(ggpubr)

ip_y_lims <- c(0.05, 1)

# lysimeter data ----

at_ip_smry <- met_intercept |>
  group_by(temp_labs) |>
  # filter(weighed_tree_canopy_load_mm <= 5) |>
  summarise(IP_avg = mean(IP, na.rm = T),
            sd = sd(IP, na.rm = T),
            sd_low = IP_avg - sd,
            sd_hi = IP_avg + sd,
            ci_low = quantile(IP,0.05),
            ci_hi = quantile(IP, 0.95),
            n = n())

at_ip <- met_intercept |>
  # filter(weighed_tree_canopy_load_mm <= 5) |>
  ggplot() +
  geom_point(aes(x = t, y = IP), colour = '#61D04F',  alpha = 0.5, size = 0.5)+
  geom_errorbar(data = at_ip_smry, aes(x = temp_labs, ymax = sd_hi, ymin = sd_low), width = .5)  +
  geom_point(data = at_ip_smry, aes(x = temp_labs, y = IP_avg), shape = 1, size = 4) +
  ylab(ip_y_ax_lab) +
  xlab(temp_bin_ax_lab) +
  # scale_fill_viridis_c(option = 'magma')+
  xlim(NA, 0) +
  # ylim(ip_y_lims) +
  theme(legend.position = 'none',
        plot.margin = margin(0.5, 0.5, 0.5, .75, "cm"))
at_ip

tice_ip_smry <- met_intercept |>
  group_by(t_ice_labs) |>
  # filter(weighed_tree_canopy_load_mm <= 5) |>
  summarise(IP_avg = mean(IP, na.rm = T),
            sd = sd(IP, na.rm = T),
            sd_low = IP_avg - sd,
            sd_hi = IP_avg + sd,
            ci_low = quantile(IP,0.05),
            ci_hi = quantile(IP, 0.95),
            n = n())

tice_ip <- met_intercept |>
  # filter(weighed_tree_canopy_load_mm <= 5) |>
  ggplot() +
  geom_point(aes(x = t_ice_bulb, y = IP), colour = '#61D04F',  alpha = 0.5, size = 0.5)+
  geom_errorbar(data = tice_ip_smry, aes(x = t_ice_labs, ymax = sd_hi, ymin = sd_low), width = .5)  +
  geom_point(data = tice_ip_smry, aes(x = t_ice_labs, y = IP_avg), shape = 1, size = 4) +
  ylab(ip_y_ax_lab) +
  xlab("") +
  # scale_fill_viridis_c(option = 'magma')+
  xlim(NA, 0) +
  # ylim(ip_y_lims) +
  theme(legend.position = 'none',
        plot.margin = margin(0.5, 0.5, 0.5, .75, "cm"))
tice_ip

tice_dep_ip_smry <- met_intercept |>
  group_by(t_ice_dep_labs) |>
  # filter(weighed_tree_canopy_load_mm <= 5) |>
  summarise(IP_avg = mean(IP, na.rm = T),
            sd = sd(IP, na.rm = T),
            sd_low = IP_avg - sd,
            sd_hi = IP_avg + sd,
            ci_low = quantile(IP,0.05),
            ci_hi = quantile(IP, 0.95),
            n = n())

tice_ip <- met_intercept |>
  # filter(weighed_tree_canopy_load_mm <= 5) |>
  ggplot() +
  geom_point(aes(x = t_ice_dep, y = IP), colour = '#61D04F',  alpha = 0.5, size = 0.5)+
  geom_errorbar(data = tice_dep_ip_smry, aes(x = t_ice_dep_labs, ymax = sd_hi, ymin = sd_low), width = .5)  +
  geom_point(data = tice_dep_ip_smry, aes(x = t_ice_dep_labs, y = IP_avg), shape = 1, size = 4) +
  ylab(ip_y_ax_lab) +
  xlab("") +
  # scale_fill_viridis_c(option = 'magma')+
  # xlim(NA, 0) +
  # ylim(ip_y_lims) +
  theme(legend.position = 'none',
        plot.margin = margin(0.5, 0.5, 0.5, .75, "cm"))
tice_ip

rh_ip_smry <- met_intercept |>
  group_by(rh_labs) |>
  # filter(weighed_tree_canopy_load_mm <= 5) |>
  summarise(IP_avg = mean(IP, na.rm = T),
            sd = sd(IP, na.rm = T),
            sd_low = IP_avg - sd,
            sd_hi = IP_avg + sd,
            ci_low = quantile(IP,0.05),
            ci_hi = quantile(IP, 0.95),
            n = n()) |>
  filter(rh_labs > 65)

rh_ip <- met_intercept |>
  # filter(weighed_tree_canopy_load_mm <= 5) |>
  ggplot() +
  geom_point(aes(x = rh, y = IP), colour = '#61D04F',  alpha = 0.5, size = 0.5)+
  geom_errorbar(data = rh_ip_smry, aes(x = rh_labs, ymax = sd_hi, ymin = sd_low), width = .5)  +
  geom_point(data = rh_ip_smry, aes(x = rh_labs, y = IP_avg), shape = 1, size = 4) +
  ylab(ip_y_ax_lab) +
  xlab("Relative Humidity (%)") +
  # scale_fill_viridis_c(option = 'magma')+
  # xlim(NA, 0) +
  # ylim(ip_y_lims) +
  theme(legend.position = 'none',
        plot.margin = margin(0.5, 0.5, 0.5, .75, "cm"))
rh_ip

Qsi_ip_smry <- met_intercept |>
  group_by(Qsi_labs) |>
  # filter(weighed_tree_canopy_load_mm <= 5) |>
  summarise(IP_avg = mean(IP, na.rm = T),
            sd = sd(IP, na.rm = T),
            sd_low = IP_avg - sd,
            sd_hi = IP_avg + sd,
            ci_low = quantile(IP,0.05),
            ci_hi = quantile(IP, 0.95),
            n = n()) |>
  filter(Qsi_labs > 60)

Qsi_ip <- met_intercept |>
  # filter(weighed_tree_canopy_load_mm <= 5) |>
  ggplot() +
  geom_point(aes(x = Qsi, y = IP), colour = '#61D04F',  alpha = 0.5, size = 0.5)+
  geom_errorbar(data = Qsi_ip_smry, aes(x = Qsi_labs, ymax = sd_hi, ymin = sd_low), width = .5)  +
  geom_point(data = Qsi_ip_smry, aes(x = Qsi_labs, y = IP_avg), shape = 1, size = 4) +
  ylab(ip_y_ax_lab) +
  xlab("Relative Humidity (%)") +
  # scale_fill_viridis_c(option = 'magma')+
  # xlim(NA, 0) +
  # ylim(ip_y_lims) +
  theme(legend.position = 'none',
        plot.margin = margin(0.5, 0.5, 0.5, .75, "cm"))
Qsi_ip

ws_ip_smry <- met_intercept |>
  group_by(wind_labs) |>
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
  geom_point(aes(x = u, y = IP), colour = '#61D04F', alpha = 0.5, size = 0.5)+
  geom_errorbar(data = ws_ip_smry, aes(x = wind_labs, ymax = sd_hi, ymin = sd_low), width = .1)  +
  geom_point(data = ws_ip_smry, aes(x = wind_labs, y = IP_avg), shape = 1, size = 4) +
  ylab(ip_y_ax_lab) +
  xlab(wnd_bin_ax_lab)+
  ylim(ip_y_lims)+
  theme(plot.margin = margin(0.5, 0.5, 0.5, .75, "cm"))
ws_ip

w_ip_smry <- met_intercept |>
  group_by(tree_labs) |>
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
  geom_point(aes(x = weighed_tree_canopy_load_mm, y = IP), colour = '#61D04F', alpha = 0.5, size = 0.5) +
  geom_errorbar(data = w_ip_smry, aes(x = tree_labs, ymax = sd_hi, ymin = sd_low), width = .5)  +
  geom_point(data = w_ip_smry, aes(x = tree_labs, y = IP_avg), colour = 'black', shape = 1, size = 4) +
  ylab(ip_y_ax_lab) +
  xlab(w_ax_lab)+
  ylim(ip_y_lims)+
  theme(plot.margin = margin(0.5, 0.5, 0.5, .75, "cm"))

w_ip

cowplot::plot_grid(at_ip, ws_ip, w_ip, nrow = 3, labels = 'AUTO')

ggsave('figs/interception/troughs_met_vs_IP_bin.png', device = png, width = 8.5, height = 7)
