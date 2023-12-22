# plot fingerprint of velocity and particle shape

parsivel <- readRDS('../../analysis/disdrometer/data/disdro_spectrum_processed.RDS')

met_intercept <- readRDS('../../analysis/interception/data/storm_analysis/continuous_throughfall_data_binned_met_select_events.rds')  |>
  filter(q_sf > 0,
         q_sf > q_tf) |>
  mutate(
    q_int = q_sf - q_tf,
    IP = q_int / q_sf) |>
  filter(IP < 1)

ggplot(met_intercept, aes(x = part_diam, y = part_vel, colour = IP)) +
  geom_point() +
  scale_color_viridis_c()

ggplot(met_intercept, aes(x = part_diam_labs, y = part_vel_labs, fill = IP)) +
  geom_raster() +
  scale_fill_viridis_c()
