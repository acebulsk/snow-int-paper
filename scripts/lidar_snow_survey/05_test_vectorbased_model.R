# Script to test model of shifted trajectory angle
# Run 04 first for some of the objects below

# also validate the model using the observed lca for the given zenith angle range

# Calculate the event trajectory angle ----

paste0('Given the height of wind deemed important for PWL: ', pwl_wind_height_of_event |> round(2),
       ' and FT: ', ft_wind_height_of_event |> round(2), '. This was based on the zone of heighest correlation of the hemisphere and canopy contacts and I/P.')

ws_pars <- data.frame(
  plot = c('PWL', 'FT'),
  event_mean_snow_depth = event_sd,
  wind_height_of_event = c(pwl_wind_height_of_event, ft_wind_height_of_event),
  best_trajectory = c(pwl_best_phi, ft_best_phi),
  best_wind = c(pwl_select_wind, ft_select_wind)
)

saveRDS(ws_pars, 'data/model_results/wind_speed_height_match_hemi_traj.rds')
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

# ggsave('figs/lidar_periods/wind_profile_20230313.png',width = 4, height = 3.5)

ggplot(event_wind_profile |>
         rename(
           `Simulated Trajectory Angle (°)` = traj_angle,
           `Simulated Wind Speed (m/s)` = wind_speed
         ) |>
         pivot_longer(c(`Simulated Trajectory Angle (°)`, `Simulated Wind Speed (m/s)`)), aes(value, height)) +
  geom_line() +
  labs(
    y = 'Height Above Snowpack (m)',
    x = element_blank(),
    # title = 'March 13-14, 2023 Wind Profile'
  ) +
  lims(
    y = c(0, NA),
    x = c(0, NA)
  ) +
  facet_wrap(~name, scales = 'free') #+
  # geom_point(
  #   data = . %>% filter(name == 'Simulated Wind Speed (m/s)'),
  #   aes(x = event_avg_wind, y = us_wind_height - event_sd),
  #   shape = 24,        # Filled triangle point-up
  #   color = 'red',
  #   fill = 'red',      # Fill color (for filled shapes)
  #   size = 4           # Increase point size
  # )

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

# adjust_wind <- function(wind_speed, z, z2, z_0m){
#   u2 <- wind_speed * (log(z2/z_0m) / log(z/z_0m))
#   return(u2)
# }
#
# ft_wind_estimate <- adjust_wind(event_avg_wind,
#                                 z = us_wind_height,
#                                 z2 = ft_wind_height_of_event,
#                                 z_0m = ft_wp_pars$z_0m)
#
# ft_event_ta_estimate <- traj_angle_deg(ft_wind_estimate, mean_vel)
#
# pwl_wind_estimate <- adjust_wind(event_avg_wind,
#                                 z = us_wind_height,
#                                 z2 = pwl_wind_height_of_event,
#                                 z_0m = ft_wp_pars$z_0m)
# pwl_event_ta_estimate <- traj_angle_deg(pwl_wind_estimate, mean_vel)

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

pwl_cc_nadir_025 <- pwl_cc_025 |>
  as.data.frame() |>
  filter(phi_d == 0)

pwl_cc_nadir <- pwl_cc_nadir_025 |>
  pull(lca) |>
  mean(na.rm = T)

ft_cc_nadir_025 <- ft_cc_025 |>
  as.data.frame() |>
  filter(phi_d == 0)

ft_cc_nadir <- ft_cc_nadir_025 |>
  pull(lca) |>
  mean(na.rm = T)

# Calculate the resulting increase in leaf contact area based on trajectory angle ----
site_mean_ht <- (pwl_e_mean_ht+ft_mean_ht)/2
# mature site pars from parvi
n = 2.43
m = 3.46

# regenerating site pars from parvi
# n = 2.97
# m = 3.2

a_parv <- parv_cionco_alpha(n, m, event_avg_wind)

event_avg_wind_onethird <-
  cionco_canopy_wind_flow(event_avg_wind,
                          a_parv,
                          (site_mean_ht/3)-event_sd,
                          us_wind_height - event_sd)

## FT ----
ft_event_ta_estimate <- traj_angle_deg(event_avg_wind_onethird, mean_vel)

ft_lca_inc <- sine_fn(ft_event_ta_estimate, cp_sine_model_coef[['b']], ft_cc_nadir)

ft_lca <- ft_cc_nadir + ft_lca_inc

## PWL ----

pwl_event_ta_estimate <- traj_angle_deg(event_avg_wind_onethird, mean_vel)

pwl_lca_inc <- sine_fn(pwl_event_ta_estimate, cp_sine_model_coef[['b']], pwl_cc_nadir)

pwl_lca <- pwl_cc_nadir + pwl_lca_inc

# Error Analysis

ft_ip_obs <- ft_ip_obs_rast |>
  values() |>
  mean(na.rm = T)
pwl_ip_obs <- pwl_ip_obs_rast |>
  values() |>
  mean(na.rm = T)

# Calculate the interception efficiency for the event

# select_ip_model <- 'Vector Based'
# ip_model_coefs <-
# readRDS('../../analysis/lidar-processing/data/models/lca_vs_ip_model_error_ft_pwl_nadir_adj.rds')
#
# ft_a <- ip_model_coefs |> filter(`Plot Name` == 'FT', `Canopy Metrics` ==
# select_ip_model) |> pull(`Model Slope`)
#
# pwl_a <- ip_model_coefs |> filter(`Plot Name` == 'PWL', `Canopy Metrics` ==
# select_ip_model) |> pull(`Model Slope`)

# force to 1
# ft_a <- 1
# pwl_a <- 1

alpha <- seq(.6, 1, by = 0.0001)
cal_alpha_df <- data.frame(
  alpha = alpha,
  ft_ip_mod = ft_lca * alpha,
  ft_ip_obs = ft_ip_obs,
  pwl_ip_mod = pwl_lca * alpha,
  pwl_ip_obs = pwl_ip_obs
) |>
  mutate(
    ft_bias = ft_ip_obs - ft_ip_mod,
    pwl_bias = pwl_ip_obs - pwl_ip_mod,
    mean_bias = (ft_bias + pwl_bias) / 2
  )

ggplot(cal_alpha_df |> pivot_longer(c(ft_bias, pwl_bias, mean_bias)), aes(alpha, value, colour = name)) +
  geom_line()

alpha_plot_scale <- cal_alpha_df$alpha[which.min(abs(cal_alpha_df$mean_bias))]

ft_ip_mod <- ft_lca * alpha_plot_scale
pwl_ip_mod <- pwl_lca * alpha_plot_scale

saveRDS(alpha_plot_scale, 'data/model_results/alpha_plot_scale.rds')

# found these actually did pretty well but vector based still better
# ft_ip_mod <- ft_cc_nadir # could agrue IP == nadir canopy coverage as in rainfall params
# pwl_ip_mod <- pwl_cc_nadir # could agrue IP == nadir canopy coverage as in rainfall params

# Vector Based
errors <- data.frame(
  ip = c(ft_ip_obs, pwl_ip_obs, ft_ip_mod, pwl_ip_mod, ft_cc_nadir, pwl_cc_nadir),
  group = c('UAV-lidar','UAV-lidar', 'VB-model', 'VB-model', 'Nadir-model', 'Nadir-model'),
  plot = c('FT', 'PWL', 'FT', 'PWL', 'FT', 'PWL')) |>
  mutate(tf = (1-ip)*event_precip) |>
  pivot_longer(c(ip, tf)) |>
  pivot_wider(names_from = c(group), values_from = value) |>
  pivot_longer(c(`VB-model`, `Nadir-model`), names_to = 'mod_type', values_to = 'mod_vals') |>
  mutate(bias = `UAV-lidar` - mod_vals,
         MAE = abs(bias),
         `Perc. Error` = (bias)/`UAV-lidar`*100)
errors

saveRDS(errors,
        paste0(
          'figs/lidar_periods/',
          pre_post_id,
          '_plot_scale_vb_model_error.rds'))

# plot accumulated throughfall over the event for each model
tf_df <- errors |>
  filter(name == 'tf') |>
  select(plot:mod_vals,-name) |>
  pivot_wider(names_from = mod_type, values_from = mod_vals) |>
  pivot_longer(c(`UAV-lidar`, `VB-model`, `Nadir-model`))
ggplot(tf_df, aes(plot, value, fill = name)) +
  geom_bar(stat = "identity", position = 'dodge') +
  labs(
    y = '𝚫SWE (mm)',
    # y = '𝚫SWE (kg m⁻²)',
    x = element_blank(),
    fill = 'Event Throughfall'
  ) +
  ylim(c(0, 30)) +
  geom_hline(aes(yintercept = event_precip, linetype = "Event Snowfall"), color = "black", show.legend = TRUE) +
  scale_linetype_manual(values = "dashed", name = NULL) +
  guides(
    fill = guide_legend(override.aes = list(linetype = "blank")),
    linetype = guide_legend(override.aes = list(color = "black"))
  )

ggsave(
  paste0(
  'figs/lidar_periods/',
  pre_post_id,
  '_event_throughfall_totals_obs_vs_vb_vs_nadir.png'),
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

# test at any grid scale ----

# i've found that the error is pretty good around 5/10/20 m and gets better as
# you get coarser/worse as you get finer. The differences between the two plots
# in terms of the error also get smaller at coarser resolutions.

# rsmpl_res <- 5
# rsmpl_fact <- rsmpl_res/0.25
# agg_fn <- 'mean'
# min_n_cells <- 25
# pwl_tf_025 <- rast("../../analysis/lidar-processing/data/dsm_swe/23_072_23_073_v2.0.0_sa_PWL_E_swe_normalised_resample_0.25_crop_mask.tif")
# pwl_tf_rsmpl <- resample_rast(pwl_tf_025, rsmpl_fact, agg_fn, min_n_cells)
# names(pwl_tf_rsmpl) <- 'tf_obs'
# ft_tf_025 <- rast("../../analysis/lidar-processing/data/dsm_swe/23_072_23_073_v2.0.0_sa_FSR_S_swe_normalised_resample_0.25_crop_mask.tif")
# ft_tf_rsmpl <- resample_rast(ft_tf_025, rsmpl_fact, agg_fn, min_n_cells)
# names(ft_tf_rsmpl) <- 'tf_obs'
#
# pwl_ip_025 <- rast("../../analysis/lidar-processing/data/dsm_ip/23_072_23_073_v2.0.0_sa_PWL_E_ip_normalised_resample_0.25_crop_mask.tif")
# pwl_ip_rsmpl <- resample_rast(pwl_ip_025, rsmpl_fact, agg_fn, min_n_cells)
# names(pwl_ip_rsmpl) <- 'ip_obs'
# ft_ip_025 <- rast("../../analysis/lidar-processing/data/dsm_ip/23_072_23_073_v2.0.0_sa_FSR_S_ip_normalised_resample_0.25_crop_mask.tif")
# ft_ip_rsmpl <- resample_rast(ft_ip_025, rsmpl_fact, agg_fn, min_n_cells)
# names(ft_ip_rsmpl) <- 'ip_obs'
#
# pwl_cc_nadir_025m <- rast('../../analysis/lidar-processing/data/dsm_cpy_metrics/23_072_vox_len_0.25m_sa_gridgen_v2.0.0_sa_PWL_E_nadir_lca_0.25m_crop_mask.tif')
# pwl_cc_nadir_rsmpl <- resample_rast(pwl_cc_nadir_025m, rsmpl_fact, agg_fn, min_n_cells)
# ft_cc_nadir_025m <- rast('../../analysis/lidar-processing/data/dsm_cpy_metrics/23_072_vox_len_0.25m_sa_gridgen_v2.0.0_sa_FSR_S_nadir_lca_0.25m_crop_mask.tif')
# ft_cc_nadir_rsmpl <- resample_rast(ft_cc_nadir_025m, rsmpl_fact, agg_fn, min_n_cells)
#
# # compute interception efficiency using the vector based model
#
# pwl_ip_vb_rsmpl <- (pwl_cc_nadir_rsmpl + pwl_lca_inc)*pwl_a
# names(pwl_ip_vb_rsmpl) <- 'ip_mod'
# ft_ip_vb_rsmpl <- (ft_cc_nadir_rsmpl + ft_lca_inc)*ft_a
# names(ft_ip_vb_rsmpl) <- 'ip_mod'
#
# # IP ----
#
# ## map Xm performance ----
#
# plot(ft_ip_rsmpl - ft_ip_vb_rsmpl)
#
# plot(pwl_ip_rsmpl - pwl_ip_vb_rsmpl)
#
#
# ## plot resampled performance with vector based adjustment ----
#
# pwl_ip_df <- c(pwl_ip_rsmpl, pwl_ip_vb_rsmpl) |>
#   terra::as.points() |>
#   as.data.frame(geom="XY") |>
#   # pivot_longer(c(ip_obs, ip_mod)) |>
#   mutate(plot_name = 'PWL')
#
# ft_ip_df <- c(ft_ip_rsmpl, ft_ip_vb_rsmpl) |>
#   terra::as.points() |>
#   as.data.frame(geom="XY") |>
#   # pivot_longer(c(ip_obs, ip_mod)) |>
#   mutate(plot_name = 'FT')
#
# ip_df_rsmpl <- rbind(pwl_ip_df, ft_ip_df)
#
# ggplot(ip_df_rsmpl, aes(ip_obs, ip_mod)) +
#   geom_point() +
#   geom_abline(aes(slope = 1, intercept = 0, linetype = "1:1 line"), alpha = 0.5) +
#   facet_grid(~plot_name)
#
# error_tbl_vb <- ip_df_rsmpl |>
#   mutate(diff = ip_obs - ip_mod) |>
#   group_by(plot_name) |>
#   summarise(
#     `Mean Bias` = mean(diff, na.rm = T),
#     # `Max Error` = diff[which.max(abs(diff))],
#     MAE = mean(abs(diff), na.rm = T),
#     `RMS Error` = sqrt(mean(diff ^ 2, na.rm = T)),
#     group = 'Vector Based')
#
# ## plot resampled performance using nadir cc as ip proxy ----
#
# pwl_ip_df <- c(pwl_ip_rsmpl, pwl_cc_nadir_rsmpl) |>
#   terra::as.points() |>
#   as.data.frame(geom="XY") |>
#   # pivot_longer(c(ip_obs, ip_mod)) |>
#   mutate(plot_name = 'PWL')
#
# ft_ip_df <- c(ft_ip_rsmpl, ft_cc_nadir_rsmpl) |>
#   terra::as.points() |>
#   as.data.frame(geom="XY") |>
#   # pivot_longer(c(ip_obs, ip_mod)) |>
#   mutate(plot_name = 'FT')
#
# ip_df_rsmpl <- rbind(pwl_ip_df, ft_ip_df)
#
# ggplot(ip_df_rsmpl, aes(ip_obs, lca)) +
#   geom_point() +
#   geom_abline(aes(slope = 1, intercept = 0, linetype = "1:1 line"), alpha = 0.5) +
#   facet_grid(~plot_name)
#
# error_tbl_cc_nadir <- ip_df_rsmpl |>
#   mutate(diff = ip_obs - lca) |>
#   group_by(plot_name) |>
#   summarise(
#     `Mean Bias` = mean(diff, na.rm = T),
#     # `Max Error` = diff[which.max(abs(diff))],
#     MAE = mean(abs(diff), na.rm = T),
#     `RMS Error` = sqrt(mean(diff ^ 2, na.rm = T)),
#     group = 'Nadir')
#
# error_tb_ip <- rbind(error_tbl_vb, error_tbl_cc_nadir)
# error_tb_ip
#
# # Throughfall ----
#
# # calculate throughfall using vector based I/P
#
# pwl_tf_vb_rsmpl <- (1-pwl_ip_vb_rsmpl) * event_precip
# names(pwl_tf_vb_rsmpl) <- 'tf_mod'
# ft_tf_vb_rsmpl <- (1-ft_ip_vb_rsmpl) * event_precip
# names(ft_tf_vb_rsmpl) <- 'tf_mod'
#
# plot(ft_tf_rsmpl - ft_tf_vb_rsmpl)
#
# plot(pwl_tf_rsmpl - pwl_tf_vb_rsmpl)
#
# pwl_tf_df <- c(pwl_tf_rsmpl, pwl_tf_vb_rsmpl) |>
#   terra::as.points() |>
#   as.data.frame(geom="XY") |>
#   # pivot_longer(c(ip_obs, ip_mod)) |>
#   mutate(plot_name = 'PWL')
#
# ft_tf_df <- c(ft_tf_rsmpl, ft_tf_vb_rsmpl) |>
#   terra::as.points() |>
#   as.data.frame(geom="XY") |>
#   # pivot_longer(c(ip_obs, ip_mod)) |>
#   mutate(plot_name = 'FT')
#
# tf_df_rsmpl <- rbind(pwl_tf_df, ft_tf_df)
#
# ggplot(tf_df_rsmpl, aes(tf_obs, tf_mod)) +
#   geom_point() +
#   geom_abline(aes(slope = 1, intercept = 0, linetype = "1:1 line"), alpha = 0.5) +
#   facet_grid(~plot_name)
#
# error_tbl_tf_vb <- tf_df_rsmpl |>
#   mutate(diff = tf_obs - tf_mod) |>
#   group_by(plot_name) |>
#   summarise(
#     `Mean Bias` = mean(diff, na.rm = T),
#     # `Max Error` = diff[which.max(abs(diff))],
#     MAE = mean(abs(diff), na.rm = T),
#     `RMS Error` = sqrt(mean(diff ^ 2, na.rm = T)),
#     group = 'Vector Based')
#
# # calculate throughfall using cc nadir as proxy for I/P
#
# pwl_tf_vb_rsmpl <- (1-pwl_cc_nadir_rsmpl) * event_precip
# names(pwl_tf_vb_rsmpl) <- 'tf_mod'
# ft_tf_vb_rsmpl <- (1-ft_cc_nadir_rsmpl) * event_precip
# names(ft_tf_vb_rsmpl) <- 'tf_mod'
#
# plot(ft_tf_rsmpl - ft_tf_vb_rsmpl)
#
# plot(pwl_tf_rsmpl - pwl_tf_vb_rsmpl)
#
# pwl_tf_df <- c(pwl_tf_rsmpl, pwl_tf_vb_rsmpl) |>
#   terra::as.points() |>
#   as.data.frame(geom="XY") |>
#   # pivot_longer(c(ip_obs, ip_mod)) |>
#   mutate(plot_name = 'PWL')
#
# ft_tf_df <- c(ft_tf_rsmpl, ft_tf_vb_rsmpl) |>
#   terra::as.points() |>
#   as.data.frame(geom="XY") |>
#   # pivot_longer(c(ip_obs, ip_mod)) |>
#   mutate(plot_name = 'FT')
#
# tf_df_rsmpl <- rbind(pwl_tf_df, ft_tf_df)
#
# ggplot(tf_df_rsmpl, aes(tf_obs, tf_mod)) +
#   geom_point() +
#   geom_abline(aes(slope = 1, intercept = 0, linetype = "1:1 line"), alpha = 0.5) +
#   facet_grid(~plot_name)
#
# error_tbl_tf_nadir <- tf_df_rsmpl |>
#   mutate(diff = tf_obs - tf_mod) |>
#   group_by(plot_name) |>
#   summarise(
#     `Mean Bias` = mean(diff, na.rm = T),
#     # `Max Error` = diff[which.max(abs(diff))],
#     MAE = mean(abs(diff), na.rm = T),
#     `RMS Error` = sqrt(mean(diff ^ 2, na.rm = T)),
#     group = 'Nadir')
#
# tf_error <- rbind(error_tbl_tf_vb,
#                   error_tbl_tf_nadir)
# tf_error

