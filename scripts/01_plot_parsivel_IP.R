# plot parsivel data with IP

parsivel_1min <- readRDS('../../analysis/disdrometer/data/disdro_spectrum_processed_202310.RDS')

# parsivel_1min |>
#   filter(is.na(precip_name) == F,
#          part_diam < 4) |>
#   ggplot(aes(part_diam, part_vel, colour = precip_name), size = 1) +
#   geom_point(alpha = 0.4) +
#   scale_color_viridis_d() +
#   ylab('Hydrometeor Velocity (m/s)') +
#   xlab('Hydrometeor Diameter (mm)') +
#   theme_bw()
#
# ggsave('figs/supplement/hydrometeor_classification_2021_2023.png', width = 5, height = 3)

ffr_met <- readRDS('../../analysis/met-data-processing/data/ffr_crhm_modelling_obs.rds')
parsivel_15min <- readRDS('../../analysis/disdrometer/data/disdro_spectrum_processed_agg_15_min_202310.RDS') |>
  left_join(ffr_met)

parsivel_15min |>
  filter(is.na(precip_name) == F,
         part_diam < 4) |>
  ggplot(aes(part_diam, part_vel, colour = precip_name), size = 1) +
  geom_point(alpha = 0.4) +
  scale_color_viridis_d() +
  ylab('Hydrometeor Velocity (m/s)') +
  xlab('Hydrometeor Diameter (mm)') +
  theme_bw()

ggsave('figs/supplement/hydrometeor_classification_2021_2023_15min_agg.png', width = 5, height = 3)

parsivel_15min |>
  ggplot(aes(precip_name, t)) + geom_boxplot(width = .1) +
  ylab("Air Temperature (°C)") +
  xlab('Hydrometeor Type')

# seems like the dry snow / graupel / wet snow classification is being messed up by turbulence at powerline
ggplot(met_intercept, aes(precip_name, IP)) + geom_boxplot()
ggplot(met_intercept, aes(precip_name, t)) + geom_boxplot()

# so well filter to lower wind speeds Garrett2014 suggest using turbulence
# calculated as ((max wind speed - avg wind speed)^2)/2 = turbulence (m2/s)
# Following Schreur and Geertsema [2008] could better define threshold using
# this metric but for now just using a wind speed threshold

u_th <- 1
wt_cl_th <- 25
met_intercept_pv <- met_intercept |>
  filter(#u < u_th,
         #weighed_tree_canopy_load_mm < wt_cl_th,
         #part_diam > 1,
        is.na(precip_name) == F)

# hydrometeor type ----

met_intercept_pv |>
  ggplot(aes(part_diam, part_vel, colour = precip_name), size = 1) +
  geom_point(alpha = 0.4) +
  scale_color_viridis_d() +
  ylab('Hydrometeor Velocity (m/s)') +
  xlab('Hydrometeor Diameter (mm)') +
  labs(colour = '')+
  theme_bw()

ggsave('figs/supplement/hydrometeor_classification_interception_periods.png', width = 5, height = 3)

# wet show should be warmer.. not trusting the classification scheme..
# maybe have some large flakes at warmer temperatures due to coehsion complicating this
met_intercept_pv |>
  ggplot(aes(precip_name, t)) + geom_boxplot(width = .1) +
  ylab("Air Temperature (°C)") +
  xlab('Hydrometeor Type')+
  theme(plot.margin = margin(0.5, 0.5, 0.5, .75, "cm"))

ggsave('figs/supplement/hydrometeor_class_vs_temperature_interception_periods.png', width = 5, height = 3)

ip_vs_hydro_type <- met_intercept_pv |>
  filter(u < u_th,
         weighed_tree_canopy_load_mm < wt_cl_th,
         precip_name %in% c('dry snow', 'wet snow')) |>
  ggplot(aes(precip_name, IP)) + geom_boxplot(width = .1) +
  ylab(ip_y_ax_lab) +
  xlab('Hydrometeor Type')+
  theme(plot.margin = margin(0.5, 0.5, 0.5, .75, "cm"))
ip_vs_hydro_type

met_intercept_pv |>
  filter(u < u_th,
         weighed_tree_canopy_load_mm < wt_cl_th,
         precip_name %in% c('dry snow', 'wet snow')) |>
  group_by(precip_name) |>
  summarise(IP = mean(IP, na.rm = T),
            count = n()) |>
  ggplot(aes(precip_name, IP)) + geom_point() +
  ylim(c(.2, .8))

# particle diameter ----

smry <- met_intercept_pv |>
  group_by(part_diam_labs) |>
  # filter(weighed_tree_canopy_load_mm <= 5) |>
  summarise(IP_avg = mean(IP, na.rm = T),
            sd = sd(IP, na.rm = T),
            sd_low = IP_avg - sd,
            sd_hi = IP_avg + sd,
            ci_low = quantile(IP,0.05),
            ci_hi = quantile(IP, 0.95),
            n = n()) |> filter(n > 3)

diam_ip <- met_intercept_pv |>
  ggplot() +
  geom_point(aes(x = part_diam, y = IP), colour = '#61D04F',  alpha = 0.5, size = 0.5)+
  geom_errorbar(data = smry, aes(x = part_diam_labs, ymax = sd_hi, ymin = sd_low), width = .025)  +
  geom_point(data = smry, aes(x = part_diam_labs, y = IP_avg), shape = 1, size = 4) +
  ylab(ip_y_ax_lab) +
  xlab(diam_ax_lab) +
  # ylim(ip_y_lims) +
  theme(legend.title = element_blank(),
        plot.margin = margin(0.5, 0.5, 0.5, .75, "cm"))
diam_ip


met_intercept_pv |>
  filter(u < u_th,
         weighed_tree_canopy_load_mm < wt_cl_th,
         precip_name %in% c('dry snow', 'wet snow')) |>
  mutate(part_diam_labs = as.numeric(as.character(part_diam_labs))) |>
  ggplot(aes(part_diam_labs, IP, group = part_diam_labs)) +
  geom_boxplot() +
  ylab(ip_y_ax_lab) +
  xlab('Hydrometeor Diameter (mm)')

ggsave('figs/supplement/IP_hydrometeor_diameter.png', width = 5, height = 3)

# particle velocity ----

smry <- met_intercept_pv |>
  group_by(part_vel_labs) |>
  # filter(weighed_tree_canopy_load_mm <= 5) |>
  summarise(IP_avg = mean(IP, na.rm = T),
            sd = sd(IP, na.rm = T),
            sd_low = IP_avg - sd,
            sd_hi = IP_avg + sd,
            ci_low = quantile(IP,0.05),
            ci_hi = quantile(IP, 0.95),
            n = n()) |> filter(part_vel_labs < 1.2)

vel_ip <- met_intercept_pv |>
  ggplot() +
  geom_point(aes(x = part_vel, y = IP), colour = '#61D04F',  alpha = 0.5, size = 0.5)+
  geom_errorbar(data = smry, aes(x = part_vel_labs, ymax = sd_hi, ymin = sd_low), width = .025)  +
  geom_point(data = smry, aes(x = part_vel_labs, y = IP_avg), shape = 1, size = 4) +
  ylab(ip_y_ax_lab) +
  xlab(vel_ax_lab) +
  ylim(ip_y_lims) +
  xlim(c(0.6, 1.3)) +
  theme(legend.title = element_blank(),
        plot.margin = margin(0.5, 0.5, 0.5, .75, "cm"))
vel_ip


met_intercept_pv |>
  mutate(part_vel_labs = as.numeric(as.character(part_vel_labs))) |>
  ggplot(aes(part_vel_labs, IP, group = part_vel_labs)) + geom_boxplot()+
  ylab(ip_y_ax_lab) +
  xlab('Hydrometeor Velocity (m/s)')

# ggsave('figs/supplement/IP_hydrometeor_velocity.png', width = 5, height = 3)

# fingerprints

met_intercept_pv |>
  ggplot(aes(x = part_diam, y = part_vel, colour = IP)) +
  geom_point() +
  scale_color_viridis_c()

avg_ip <- met_intercept_pv |>
  group_by(part_diam_labs, part_vel_labs) |>
  summarise(IP = mean(IP, na.rm = T),
            n = n()) |>
  filter(n > 4)



avg_ip |>
  ggplot(aes(x = part_diam_labs, y = part_vel_labs, fill = IP)) +
  geom_raster() +
  scale_fill_viridis_c()
