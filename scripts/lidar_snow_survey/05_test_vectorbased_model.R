# Script to test model of shifted trajectory angle
# Run 04 first for some of the objects below

# Calculate the event trajectory angle ----

paste0('Given the height of wind deemed important for PWL: ', pwl_wind_height_of_event |> round(2),
       ' and FT: ', ft_wind_height_of_event |> round(2), '. This was based on the zone of heighest correlation of the hemisphere and canopy contacts and I/P.')

# First need the wind speed at the specified heights above

heights <- seq(ft_wp_pars$d_0+ft_wp_pars$z_0m, 12, 0.1)
wind_speeds <- trbtransfeR::fit_neutral_wind_helpr(c(event_avg_ustar,
                                      ft_wp_pars$z_0m,
                                      ft_wp_pars$d_0),
                                      heights)

event_wind_profile <- data.frame(
  height = heights,
  wind_speed = wind_speeds,
  traj_angle = traj_angle_deg(wind_speeds, mean_vel)
)

ggplot(event_wind_profile, aes(wind_speed, height)) +
  geom_line() +
  labs(
    y = 'Height Above Snowpack (m)',
    x = 'Average Wind Speed (m/s)',
    title = 'March 13-14, 2023 Wind Profile'
  )

ggsave('figs/lidar_periods/wind_profile_20230313.png',width = 4, height = 3.5)

ggplot(event_wind_profile |>
         rename(
           `Simulated Trajectory Angle (°)` = traj_angle,
           `Simulated Wind Speed (m/s)` = wind_speed
         ) |>
         pivot_longer(c(`Simulated Trajectory Angle (°)`, `Simulated Wind Speed (m/s)`)), aes(value, height)) +
  geom_line() +
  labs(
    y = 'Height Above Snowpack (m)',
    x = 'Average Wind Speed (m/s)',
    # title = 'March 13-14, 2023 Wind Profile'
  ) +
  facet_wrap(~name, scales = 'free')

ggsave('figs/lidar_periods/wind_profile_w_trajectories_20230313.png',width = 8, height = 3.5)

ft_wind <- trbtransfeR::fit_neutral_wind_helpr(c(event_avg_ustar,
                                                         ft_wp_pars$z_0m,
                                                         ft_wp_pars$d_0),
                                               ft_wind_height_of_event)

ft_10m_example_wind <- trbtransfeR::fit_neutral_wind_helpr(c(event_avg_ustar,
                                                        ft_wp_pars$z_0m,
                                                        ft_wp_pars$d_0),
                                                      10)
ft_10m_example_ta <- traj_angle_deg(ft_10m_example_wind, mean_vel)

saveRDS(list(height = 10, wind = ft_10m_example_wind, traj_angle = ft_10m_example_ta),
        'data/event_met/ft_20230313_wind_speed_traj_angle_10m.rds')

ft_2m_example_wind <- trbtransfeR::fit_neutral_wind_helpr(c(event_avg_ustar,
                                                             ft_wp_pars$z_0m,
                                                             ft_wp_pars$d_0),
                                                           2)
ft_2m_example_ta <- traj_angle_deg(ft_2m_example_wind, mean_vel)

saveRDS(list(height = 2, wind = ft_2m_example_wind, traj_angle = ft_2m_example_ta),
        'data/event_met/ft_20230313_wind_speed_traj_angle_2m.rds')

ft_event_ta <- traj_angle_deg(ft_wind, mean_vel)

pwl_wind <- trbtransfeR::fit_neutral_wind_helpr(c(event_avg_ustar,
                                                 ft_wp_pars$z_0m,
                                                 ft_wp_pars$d_0),
                                               pwl_wind_height_of_event)

pwl_event_ta <- traj_angle_deg(pwl_wind, mean_vel)

# if we didnt have the u_star measurement to get the wind profile we should
# adjust wind speed from 3 m to 2 m using equation from [@McMahon2013]:
# $$
#   u_2 = u_z \frac{ln(\frac{2}{z_0})}{ln(\frac{z}{z_0})}
# $$
# where $u_2$ is the wind speed at a height of 2 m and $u_z$ is at the given
# height $z$, and $z_0$ is the roughness height.

adjust_wind <- function(wind_speed, z, z2, z_0m){
  u2 <- wind_speed * (log(z2/z_0m) / log(z/z_0m))
  return(u2)
}

ft_wind_estimate <- adjust_wind(event_avg_wind,
                                z = us_wind_height,
                                z2 = ft_wind_height_of_event,
                                z_0m = ft_wp_pars$z_0m)

ft_event_ta_estimate <- traj_angle_deg(ft_wind_estimate, mean_vel)

pwl_wind_estimate <- adjust_wind(event_avg_wind,
                                z = us_wind_height,
                                z2 = pwl_wind_height_of_event,
                                z_0m = ft_wp_pars$z_0m)
pwl_event_ta_estimate <- traj_angle_deg(pwl_wind_estimate, mean_vel)

# so if we used the basic wind profile estimate above we would be over
# estimating the traj angle by about 10 deg.

# get the average plot canopy coverage from nadir ----

# these data have canopy metrics even where we do not have snow depths...
# nadir_cc <-
#   readRDS('../../analysis/lidar-processing/data/hemi_stats/full_voxrs_not_filtered_to_swe_traj_angle_w_lca_23_072.rds')
#
# ft_cc_nadir <- nadir_cc |>
#   filter(plot_name == 'FT') |>
#   pull(lca_nadir) |>
#   unique()
# pwl_cc_nadir <- nadir_cc |>
#   filter(plot_name == 'PWL') |>
#   pull(lca_nadir) |>
#   unique()

# get the canopy metrics that match up to where we have snow observations we
# want to use this data set that does not have non-snow depth forest structure
# metrics otherwise we overestimate interception efficiency in out model
# because our measurements are inherently underestimates since we are missing a
# lot of subcanopy snow depths

mcn_df_smry <- readRDS(paste0(
  '../../analysis/lidar-processing/data/hemi_stats/avg_voxrs_canopy_metrics_over_nadir_and_upper_hemi_2.5thpercentile_FT_PWL.rds'
))

ft_cc_nadir <- mcn_df_smry |>
  filter(plot_name == 'FT',
         group == 'Nadir') |>
  pull(lca) |>
  mean(na.rm = T)

pwl_cc_nadir <- mcn_df_smry |>
  filter(plot_name == 'PWL',
         group == 'Nadir') |>
  pull(lca) |>
  mean(na.rm = T)

# Calculate the resulting increase in leaf contact area based on trajectory angle ----

ft_nls_coefs <-
  readRDS('../../analysis/lidar-processing/data/models/ta_vs_lca_nls_coefs_ft.rds')

ft_lca_inc <- logistic_origin(x = ft_event_ta,
                              Asym = ft_nls_coefs['Asym'],
                              xmid = ft_nls_coefs['xmid'],
                              scal = ft_nls_coefs['scal'])  |> as.numeric()

pwl_nls_coefs <-
  readRDS('../../analysis/lidar-processing/data/models/ta_vs_lca_nls_coefs_pwl.rds')

pwl_lca_inc <- logistic_origin(x = pwl_event_ta,
                              Asym = pwl_nls_coefs['Asym'],
                              xmid = pwl_nls_coefs['xmid'],
                              scal = pwl_nls_coefs['scal']) |> as.numeric()

# Calculate the leaf contact area for the event adjusted by trajectory angle

ft_lca <- ft_cc_nadir + ft_lca_inc

pwl_lca <- pwl_cc_nadir + pwl_lca_inc

# Calculate the interception efficiency for the event
ip_model_coefs <- readRDS('../../analysis/lidar-processing/data/models/lca_vs_ip_model_error_ft_pwl_nadir_adj.rds')

ft_a <- ip_model_coefs |>
  filter(`Plot Name` == 'FT',
         `Canopy Metrics` == 'Vector Based') |>
  pull(`Model Slope`)

pwl_a <- ip_model_coefs |>
  filter(`Plot Name` == 'PWL',
         `Canopy Metrics` == 'Vector Based') |>
  pull(`Model Slope`)

ft_ip_mod <- ft_lca * ft_a
pwl_ip_mod <- pwl_lca * pwl_a

# found these actually did pretty well but vector based still better
# ft_ip_mod <- ft_cc_nadir # could agrue IP == nadir canopy coverage as in rainfall params
# pwl_ip_mod <- pwl_cc_nadir # could agrue IP == nadir canopy coverage as in rainfall params

# Error Analysis
library(terra)

ft_ip_obs <-
  rast("../../analysis/lidar-processing/data/dsm_ip/23_072_23_073_v2.0.0_sa_FSR_S_ip_normalised_resample_0.25_crop_mask.tif") |>
  values() |>
  mean(na.rm = T)
pwl_ip_obs <-
  rast("../../analysis/lidar-processing/data/dsm_ip/23_072_23_073_v2.0.0_sa_PWL_E_ip_normalised_resample_0.25_crop_mask.tif") |>
  values() |>
  mean(na.rm = T)

# Vector Based
errors <- data.frame(
  ip = c(ft_ip_obs, pwl_ip_obs, ft_ip_mod, pwl_ip_mod,ft_cc_nadir, pwl_cc_nadir),
  group = c('obs','obs', 'vb', 'vb', 'nadir', 'nadir'),
  plot = c('FT', 'PWL', 'FT', 'PWL', 'FT', 'PWL')) |>
  mutate(tf = (1-ip)*event_precip) |>
  pivot_longer(c(ip, tf)) |>
  pivot_wider(names_from = c(group), values_from = value) |>
  pivot_longer(c(vb, nadir), names_to = 'mod_type', values_to = 'mod_vals') |>
  mutate(bias = obs - mod_vals,
         MAE = abs(bias),
         `Perc. Error` = (bias)/obs*100)
errors

saveRDS(errors, 'data/model_results/plot_scale_vb_model_error.rds')

# plot accumulated throughfall over the event for each model
tf_df <- errors |>
  filter(name == 'tf') |>
  select(plot:mod_vals,-name) |>
  pivot_wider(names_from = mod_type, values_from = mod_vals) |>
  pivot_longer(c(obs, vb, nadir))
ggplot(tf_df, aes(plot, value, fill = name)) +
  geom_bar(stat = "identity", position = 'dodge') +
  labs(
    y = '𝚫 Throughfall (kg m⁻²)',
    x = element_blank(),
    fill = 'Data Type'
  ) +
  ylim(c(0, 30)) +
  geom_hline(aes(yintercept = event_precip, linetype = "𝚫 SWEo \n(kg m⁻²)"), color = "black", show.legend = TRUE) +
  scale_linetype_manual(values = "dashed", name = NULL) +
  guides(
    fill = guide_legend(override.aes = list(linetype = "blank")),
    linetype = guide_legend(override.aes = list(color = "black"))
  )

ggsave(
  'figs/lidar_periods/20230314_event_throughfall_totals_obs_vs_vb_vs_nadir.png',
  width = 5,
  height = 3
)

error_summary <- errors |>
  group_by(plot, name, mod_type) |>
  summarise(
    bias = bias,
    MAE = MAE) |>
  arrange(name)

ggplot(error_summary, aes(mod_type, MAE, fill = plot)) +
  geom_bar(stat = "identity", position = 'dodge') +
  facet_wrap(~name, scales = 'free')
ggplot(error_summary, aes(mod_type, bias, fill = plot)) +
  geom_bar(stat = "identity", position = 'dodge') +
  facet_wrap(~name, scales = 'free')

error_summary_noplots <- errors |>
  group_by(name, mod_type) |>
  summarise(
    bias = mean(bias),
    MAE = mean(MAE)) |>
  arrange(name)

error_summary_noplots

# test at the 5 m grid scale ----

rsmpl_res <- 5
agg_fn <- 'average'
pwl_tf_025 <- rast("../../analysis/lidar-processing/data/dsm_swe/23_072_23_073_v2.0.0_sa_PWL_E_swe_normalised_resample_0.25_crop_mask.tif")
pwl_tf_5m <- resample_rast(pwl_tf_025, rsmpl_res, agg_fn)
names(pwl_tf_5m) <- 'tf_obs'
ft_tf_025 <- rast("../../analysis/lidar-processing/data/dsm_swe/23_072_23_073_v2.0.0_sa_FSR_S_swe_normalised_resample_0.25_crop_mask.tif")
ft_tf_5m <- resample_rast(ft_tf_025, rsmpl_res, agg_fn)
names(ft_tf_5m) <- 'tf_obs'

pwl_ip_5m <- rast("../../analysis/lidar-processing/data/dsm_ip/23_072_23_073_v2.0.0_sa_PWL_E_ip_normalised_resample_5m_crop_mask.tif")
names(pwl_ip_5m) <- 'ip_obs'
ft_ip_5m <- rast("../../analysis/lidar-processing/data/dsm_ip/23_072_23_073_v2.0.0_sa_FSR_S_ip_normalised_resample_5m_crop_mask.tif")
names(ft_ip_5m) <- 'ip_obs'

pwl_cc_nadir_5m <- rast('../../analysis/lidar-processing/data/dsm_cpy_metrics/23_072_23_073_v2.0.0_sa_PWL_E_nadir_lca_5m_crop_mask.tif')
ft_cc_nadir_5m <- rast('../../analysis/lidar-processing/data/dsm_cpy_metrics/23_072_23_073_v2.0.0_sa_FSR_S_nadir_lca_5m_crop_mask.tif')

# compute interception efficiency using the vector based model

pwl_ip_vb_5m <- (pwl_cc_nadir_5m + pwl_lca_inc)*pwl_a
names(pwl_ip_vb_5m) <- 'ip_mod'
ft_ip_vb_5m <- (ft_cc_nadir_5m + ft_lca_inc)*ft_a
names(ft_ip_vb_5m) <- 'ip_mod'

pwl_cp_vb_5m <- (pwl_cc_nadir_5m + pwl_lca_inc)
names(pwl_cp_vb_5m) <- 'C_p_mod'
ft_cp_vb_5m <- (ft_cc_nadir_5m + ft_lca_inc)
names(ft_cp_vb_5m) <- 'C_p_mod'

# Leaf Contact Area ----

pwl_cp_df <- c(pwl_ip_5m, pwl_cp_vb_5m) |>
  terra::as.points() |>
  as.data.frame(geom="XY") |>
  # pivot_longer(c(ip_obs, ip_mod)) |>
  mutate(plot_name = 'PWL')

ft_cp_df <- c(ft_ip_5m, ft_cp_vb_5m) |>
  terra::as.points() |>
  as.data.frame(geom="XY") |>
  # pivot_longer(c(ip_obs, ip_mod)) |>
  mutate(plot_name = 'FT')

cp_df_5m <- rbind(pwl_cp_df, ft_cp_df)

ggplot(cp_df_5m, aes(C_p_mod, ip_obs)) +
  geom_point() +
  geom_abline(aes(slope = 1, intercept = 0, linetype = "1:1 line"), alpha = 0.5) +
  facet_grid(~plot_name) +
  lims(
    y = c(0,NA),
    x = c(0,NA)
  )

# IP ----

## map 5m performance ----

plot(ft_ip_5m - ft_ip_vb_5m)

plot(pwl_ip_5m - pwl_ip_vb_5m)

## plot 5 m performance with vector based adjustment ----

pwl_ip_df <- c(pwl_ip_5m, pwl_ip_vb_5m) |>
  terra::as.points() |>
  as.data.frame(geom="XY") |>
  # pivot_longer(c(ip_obs, ip_mod)) |>
  mutate(plot_name = 'PWL')

ft_ip_df <- c(ft_ip_5m, ft_ip_vb_5m) |>
  terra::as.points() |>
  as.data.frame(geom="XY") |>
  # pivot_longer(c(ip_obs, ip_mod)) |>
  mutate(plot_name = 'FT')

ip_df_5m <- rbind(pwl_ip_df, ft_ip_df)

ggplot(ip_df_5m, aes(ip_obs, ip_mod)) +
  geom_point() +
  geom_abline(aes(slope = 1, intercept = 0, linetype = "1:1 line"), alpha = 0.5) +
  facet_grid(~plot_name)

error_tbl_vb <- ip_df_5m |>
  mutate(diff = ip_obs - ip_mod) |>
  group_by(plot_name) |>
  summarise(
    `Mean Bias` = mean(diff, na.rm = T),
    # `Max Error` = diff[which.max(abs(diff))],
    MAE = mean(abs(diff), na.rm = T),
    `RMS Error` = sqrt(mean(diff ^ 2, na.rm = T)))

## plot 5 m performance using nadir cc as ip proxy ----

pwl_ip_df <- c(pwl_ip_5m, pwl_cc_nadir_5m) |>
  terra::as.points() |>
  as.data.frame(geom="XY") |>
  # pivot_longer(c(ip_obs, ip_mod)) |>
  mutate(plot_name = 'PWL')

ft_ip_df <- c(ft_ip_5m, ft_cc_nadir_5m) |>
  terra::as.points() |>
  as.data.frame(geom="XY") |>
  # pivot_longer(c(ip_obs, ip_mod)) |>
  mutate(plot_name = 'FT')

ip_df_5m <- rbind(pwl_ip_df, ft_ip_df)

ggplot(ip_df_5m, aes(ip_obs, Nadir)) +
  geom_point() +
  geom_abline(aes(slope = 1, intercept = 0, linetype = "1:1 line"), alpha = 0.5) +
  facet_grid(~plot_name)

error_tbl_cc_nadir <- ip_df_5m |>
  mutate(diff = ip_obs - Nadir) |>
  group_by(plot_name) |>
  summarise(
    `Mean Bias` = mean(diff, na.rm = T),
    # `Max Error` = diff[which.max(abs(diff))],
    MAE = mean(abs(diff), na.rm = T),
    `RMS Error` = sqrt(mean(diff ^ 2, na.rm = T)))


# Throughfall ----

# calculate throughfall using vector based I/P

pwl_tf_vb_5m <- (1-pwl_ip_vb_5m) * event_precip
names(pwl_tf_vb_5m) <- 'tf_mod'
ft_tf_vb_5m <- (1-ft_ip_vb_5m) * event_precip
names(ft_tf_vb_5m) <- 'tf_mod'

plot(ft_tf_5m - ft_tf_vb_5m)

plot(pwl_tf_5m - pwl_tf_vb_5m)

pwl_tf_df <- c(pwl_tf_5m, pwl_tf_vb_5m) |>
  terra::as.points() |>
  as.data.frame(geom="XY") |>
  # pivot_longer(c(ip_obs, ip_mod)) |>
  mutate(plot_name = 'PWL')

ft_tf_df <- c(ft_tf_5m, ft_tf_vb_5m) |>
  terra::as.points() |>
  as.data.frame(geom="XY") |>
  # pivot_longer(c(ip_obs, ip_mod)) |>
  mutate(plot_name = 'FT')

tf_df_5m <- rbind(pwl_tf_df, ft_tf_df)

ggplot(tf_df_5m, aes(tf_obs, tf_mod)) +
  geom_point() +
  geom_abline(aes(slope = 1, intercept = 0, linetype = "1:1 line"), alpha = 0.5) +
  facet_grid(~plot_name)

error_tbl_tf <- tf_df_5m |>
  mutate(diff = tf_obs - tf_mod) |>
  group_by(plot_name) |>
  summarise(
    `Mean Bias` = mean(diff, na.rm = T),
    # `Max Error` = diff[which.max(abs(diff))],
    MAE = mean(abs(diff), na.rm = T),
    `RMS Error` = sqrt(mean(diff ^ 2, na.rm = T)))

# test at any grid scale ----

# i've found that the error is pretty good around 5/10/20 m and gets better as
# you get coarser/worse as you get finer. The differences between the two plots
# in terms of the error also get smaller at coarser resolutions.

rsmpl_res <- 20
agg_fn <- 'average'
pwl_tf_025 <- rast("../../analysis/lidar-processing/data/dsm_swe/23_072_23_073_v2.0.0_sa_PWL_E_swe_normalised_resample_0.25_crop_mask.tif")
pwl_tf_rsmpl <- resample_rast(pwl_tf_025, rsmpl_res, agg_fn)
names(pwl_tf_rsmpl) <- 'tf_obs'
ft_tf_025 <- rast("../../analysis/lidar-processing/data/dsm_swe/23_072_23_073_v2.0.0_sa_FSR_S_swe_normalised_resample_0.25_crop_mask.tif")
ft_tf_rsmpl <- resample_rast(ft_tf_025, rsmpl_res, agg_fn)
names(ft_tf_rsmpl) <- 'tf_obs'

pwl_ip_025 <- rast("../../analysis/lidar-processing/data/dsm_ip/23_072_23_073_v2.0.0_sa_PWL_E_ip_normalised_resample_0.25_crop_mask.tif")
pwl_ip_rsmpl <- resample_rast(pwl_ip_025, rsmpl_res, agg_fn)
names(pwl_ip_rsmpl) <- 'ip_obs'
ft_ip_025 <- rast("../../analysis/lidar-processing/data/dsm_ip/23_072_23_073_v2.0.0_sa_FSR_S_ip_normalised_resample_0.25_crop_mask.tif")
ft_ip_rsmpl <- resample_rast(ft_ip_025, rsmpl_res, agg_fn)
names(ft_ip_rsmpl) <- 'ip_obs'

pwl_cc_nadir_025m <- rast('../../analysis/lidar-processing/data/dsm_cpy_metrics/23_072_23_073_v2.0.0_sa_PWL_E_nadir_lca_0.25m_crop_mask.tif')
pwl_cc_nadir_rsmpl <- resample_rast(pwl_cc_nadir_025m, rsmpl_res, agg_fn)
ft_cc_nadir_025m <- rast('../../analysis/lidar-processing/data/dsm_cpy_metrics/23_072_23_073_v2.0.0_sa_FSR_S_nadir_lca_0.25m_crop_mask.tif')
ft_cc_nadir_rsmpl <- resample_rast(ft_cc_nadir_025m, rsmpl_res, agg_fn)

# compute interception efficiency using the vector based model

pwl_ip_vb_rsmpl <- (pwl_cc_nadir_rsmpl + pwl_lca_inc)*pwl_a
names(pwl_ip_vb_rsmpl) <- 'ip_mod'
ft_ip_vb_rsmpl <- (ft_cc_nadir_rsmpl + ft_lca_inc)*ft_a
names(ft_ip_vb_rsmpl) <- 'ip_mod'

# IP ----

## map 5m performance ----

plot(ft_ip_rsmpl - ft_ip_vb_rsmpl)

plot(pwl_ip_rsmpl - pwl_ip_vb_rsmpl)


## plot resampled performance with vector based adjustment ----

pwl_ip_df <- c(pwl_ip_rsmpl, pwl_ip_vb_rsmpl) |>
  terra::as.points() |>
  as.data.frame(geom="XY") |>
  # pivot_longer(c(ip_obs, ip_mod)) |>
  mutate(plot_name = 'PWL')

ft_ip_df <- c(ft_ip_rsmpl, ft_ip_vb_rsmpl) |>
  terra::as.points() |>
  as.data.frame(geom="XY") |>
  # pivot_longer(c(ip_obs, ip_mod)) |>
  mutate(plot_name = 'FT')

ip_df_rsmpl <- rbind(pwl_ip_df, ft_ip_df)

ggplot(ip_df_rsmpl, aes(ip_obs, ip_mod)) +
  geom_point() +
  geom_abline(aes(slope = 1, intercept = 0, linetype = "1:1 line"), alpha = 0.5) +
  facet_grid(~plot_name)

error_tbl_vb <- ip_df_rsmpl |>
  mutate(diff = ip_obs - ip_mod) |>
  group_by(plot_name) |>
  summarise(
    `Mean Bias` = mean(diff, na.rm = T),
    # `Max Error` = diff[which.max(abs(diff))],
    MAE = mean(abs(diff), na.rm = T),
    `RMS Error` = sqrt(mean(diff ^ 2, na.rm = T)))

## plot resampled performance using nadir cc as ip proxy ----

pwl_ip_df <- c(pwl_ip_rsmpl, pwl_cc_nadir_rsmpl) |>
  terra::as.points() |>
  as.data.frame(geom="XY") |>
  # pivot_longer(c(ip_obs, ip_mod)) |>
  mutate(plot_name = 'PWL')

ft_ip_df <- c(ft_ip_rsmpl, ft_cc_nadir_rsmpl) |>
  terra::as.points() |>
  as.data.frame(geom="XY") |>
  # pivot_longer(c(ip_obs, ip_mod)) |>
  mutate(plot_name = 'FT')

ip_df_rsmpl <- rbind(pwl_ip_df, ft_ip_df)

ggplot(ip_df_rsmpl, aes(ip_obs, Nadir)) +
  geom_point() +
  geom_abline(aes(slope = 1, intercept = 0, linetype = "1:1 line"), alpha = 0.5) +
  facet_grid(~plot_name)

error_tbl_cc_nadir <- ip_df_rsmpl |>
  mutate(diff = ip_obs - Nadir) |>
  group_by(plot_name) |>
  summarise(
    `Mean Bias` = mean(diff, na.rm = T),
    # `Max Error` = diff[which.max(abs(diff))],
    MAE = mean(abs(diff), na.rm = T),
    `RMS Error` = sqrt(mean(diff ^ 2, na.rm = T)))


# Throughfall ----

# calculate throughfall using vector based I/P

pwl_tf_vb_rsmpl <- (1-pwl_ip_vb_rsmpl) * event_precip
names(pwl_tf_vb_rsmpl) <- 'tf_mod'
ft_tf_vb_rsmpl <- (1-ft_ip_vb_rsmpl) * event_precip
names(ft_tf_vb_rsmpl) <- 'tf_mod'

plot(ft_tf_rsmpl - ft_tf_vb_rsmpl)

plot(pwl_tf_rsmpl - pwl_tf_vb_rsmpl)

pwl_tf_df <- c(pwl_tf_rsmpl, pwl_tf_vb_rsmpl) |>
  terra::as.points() |>
  as.data.frame(geom="XY") |>
  # pivot_longer(c(ip_obs, ip_mod)) |>
  mutate(plot_name = 'PWL')

ft_tf_df <- c(ft_tf_rsmpl, ft_tf_vb_rsmpl) |>
  terra::as.points() |>
  as.data.frame(geom="XY") |>
  # pivot_longer(c(ip_obs, ip_mod)) |>
  mutate(plot_name = 'FT')

tf_df_rsmpl <- rbind(pwl_tf_df, ft_tf_df)

ggplot(tf_df_rsmpl, aes(tf_obs, tf_mod)) +
  geom_point() +
  geom_abline(aes(slope = 1, intercept = 0, linetype = "1:1 line"), alpha = 0.5) +
  facet_grid(~plot_name)

error_tbl_tf <- tf_df_rsmpl |>
  mutate(diff = tf_obs - tf_mod) |>
  group_by(plot_name) |>
  summarise(
    `Mean Bias` = mean(diff, na.rm = T),
    # `Max Error` = diff[which.max(abs(diff))],
    MAE = mean(abs(diff), na.rm = T),
    `RMS Error` = sqrt(mean(diff ^ 2, na.rm = T)))

