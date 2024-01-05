# plot parsivel data with IP

parsivel <- readRDS('../../analysis/disdrometer/data/disdro_spectrum_processed_202310.RDS')

parsivel |>
  filter(is.na(precip_name) == F,
         part_diam < 4) |>
  ggplot(aes(part_diam, part_vel, colour = precip_name), size = 1) +
  geom_point(alpha = 0.4) +
  scale_color_viridis_d() +
  ylab('Hydrometeor Velocity (m/s)') +
  xlab('Hydrometeor Diameter (mm)') +
  theme_bw()

ggsave('figs/supplement/hydrometeor_classification_2021_2023.png', width = 5, height = 3)

met_intercept <- readRDS('../../analysis/interception/data/storm_analysis/continuous_throughfall_data_binned_met_select_events.rds')  |>
  filter(q_sf > 0,
         q_sf > q_tf) |>
  mutate(
    q_int = q_sf - q_tf,
    IP = q_int / q_sf) |>
  filter(IP < 1)

# seems like the dry snow / graupel / wet snow classification is being messed up by turbulence at powerline
ggplot(met_intercept, aes(precip_name, IP)) + geom_boxplot()
ggplot(met_intercept, aes(precip_name, t)) + geom_boxplot()

# so well filter to lower wind speeds Garrett2014 suggest using turbulence
# calculated as ((max wind speed - avg wind speed)^2)/2 = turbulence (m2/s)
# Following Schreur and Geertsema [2008] could better define threshold using
# this metric but for now just using a wind speed threshold

u_th <- 1

# hydrometeor type ----

met_intercept |>
  filter(u < u_th,
         precip_name %in% c('dry snow', 'wet snow')) |>
  ggplot(aes(part_diam, part_vel, colour = precip_name), size = 1) +
  geom_point(alpha = 0.4) +
  scale_color_viridis_d() +
  ylab('Hydrometeor Velocity (m/s)') +
  xlab('Hydrometeor Diameter (mm)') +
  labs(colour = '')+
  theme_bw()

ggsave('figs/supplement/hydrometeor_classification_low_wind.png', width = 5, height = 3)

# wet show should be warmer.. not trusting the classification scheme..
met_intercept |>
  filter(u < u_th,
         precip_name %in% c('dry snow', 'wet snow')) |>
  ggplot(aes(precip_name, t)) + geom_boxplot(width = .1) +
  ylab(ip_y_ax_lab) +
  xlab('Hydrometeor Type')+
  theme(plot.margin = margin(0.5, 0.5, 0.5, .75, "cm"))

ip_vs_hydro_type <- met_intercept |>
  filter(u < u_th,
         precip_name %in% c('dry snow', 'wet snow')) |>
  ggplot(aes(precip_name, IP)) + geom_boxplot(width = .1) +
  ylab(ip_y_ax_lab) +
  xlab('Hydrometeor Type')+
  theme(plot.margin = margin(0.5, 0.5, 0.5, .75, "cm"))
ip_vs_hydro_type

met_intercept |>
  filter(u < u_th,
         precip_name %in% c('dry snow', 'wet snow')) |>
  group_by(precip_name) |>
  summarise(IP = mean(IP, na.rm = T),
            count = n()) |>
  ggplot(aes(precip_name, IP)) + geom_point() +
  ylim(c(.2, .8))



# particle diameter ----

met_intercept |>
  filter(u < u_th,
         precip_name %in% c('dry snow', 'wet snow')) |>
  mutate(part_diam_labs = as.numeric(as.character(part_diam_labs))) |>
  group_by(part_diam_labs) |>
  summarise(IP = mean(IP, na.rm = T),
            count = n()) |>
  filter(count > 3) |>
  ggplot(aes(part_diam_labs, IP)) + geom_point() +
  ylab(ip_y_ax_lab) +
  xlab('Hydrometeor Diameter (mm)')

met_intercept |>
  filter(u < u_th,
         precip_name %in% c('dry snow', 'wet snow')) |>
  mutate(part_diam_labs = as.numeric(as.character(part_diam_labs))) |>
  ggplot(aes(part_diam_labs, IP, group = part_diam_labs)) +
  geom_boxplot() +
  ylab(ip_y_ax_lab) +
  xlab('Hydrometeor Diameter (mm)')

ggsave('figs/supplement/IP_hydrometeor_diameter.png', width = 5, height = 3)

# particle velocity ----

met_intercept |>
  filter(u < u_th,
         precip_name %in% c('dry snow', 'wet snow')) |>
  mutate(part_vel_labs = as.numeric(as.character(part_vel_labs))) |>
  group_by(part_vel_labs) |>
  summarise(IP = mean(IP, na.rm = T),
            count = n(),
            u = mean(u)) |>
  filter(count > 3) |>
  ggplot(aes(part_vel_labs, IP)) + geom_point() +
  ylab(ip_y_ax_lab) +
  xlab('Hydrometeor Velocity (m/s)')

met_intercept |>
  filter(u < u_th,
         precip_name %in% c('dry snow', 'wet snow')) |>
  mutate(part_vel_labs = as.numeric(as.character(part_vel_labs))) |>
  ggplot(aes(part_vel_labs, IP, group = part_vel_labs)) + geom_boxplot()+
  ylab(ip_y_ax_lab) +
  xlab('Hydrometeor Velocity (m/s)')

ggsave('figs/supplement/IP_hydrometeor_velocity.png', width = 5, height = 3)

# fingerprints
met_intercept |>
  filter(u < u_th) |>
  ggplot(aes(x = part_diam, y = part_vel, colour = IP)) +
  geom_point() +
  scale_color_viridis_c()

met_intercept |>
  filter(u < u_th) |>
  ggplot(aes(x = part_diam_labs, y = part_vel_labs, fill = IP)) +
  geom_raster() +
  scale_fill_viridis_c()
