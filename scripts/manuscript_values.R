# Bring values into environment for manuscript

source('../../analysis/disdrometer/scripts/00_source_functions.R')
library(dplyr)
library(ggpubr)

## data and methods ----
# CANOPY
ft_mean_ht <- readRDS('../../analysis/lidar-processing/data/lidR_canopy_metrics/frs_s_mean_tree_height.rds') |>
  round(2)
pwl_mean_ht <- readRDS('../../analysis/lidar-processing/data/lidR_canopy_metrics/pwl_e_mean_tree_height.rds') |>
  round(2)

# these data have canopy metrics even where we do not have snow depths...
nadir_cc <-
  readRDS('../../analysis/lidar-processing/data/hemi_stats/full_voxrs_not_filtered_to_swe_traj_angle_w_lca_23_072.rds')

ft_cc_nadir <- nadir_cc |>
  filter(plot_name == 'FT') |>
  pull(lca_nadir) |>
  unique() |>
  round(2)
pwl_cc_nadir <- nadir_cc |>
  filter(plot_name == 'PWL') |>
  pull(lca_nadir) |>
  unique() |>
  round(2)

# determine the fraction of canopy height the select wind speed height is

ws_pars <- readRDS('data/model_results/wind_speed_height_match_hemi_traj.rds')
ws_pars$mean_cpy_height <- c(pwl_mean_ht, ft_mean_ht)
ws_pars$frac_cpy_height_off_snow <- ws_pars$wind_height_of_event/ws_pars$mean_cpy_height
ws_pars$ht_above_ground <- ws_pars$event_mean_snow_depth+ws_pars$wind_height_of_event
ws_pars$frac_cpy_height_off_ground <- ws_pars$ht_above_ground/ws_pars$mean_cpy_height

pwl_best_wind <- ws_pars$best_wind[ws_pars$plot == 'PWL'] |> round(2)
ft_best_wind <- ws_pars$best_wind[ws_pars$plot == 'FT'] |> round(2)
pwl_wind_height_off_snow <- ws_pars$wind_height_of_event[ws_pars$plot == 'PWL'] |> round(2)
ft_wind_height_off_snow <- ws_pars$wind_height_of_event[ws_pars$plot == 'FT'] |> round(2)
pwl_wind_height_off_ground <- ws_pars$ht_above_ground[ws_pars$plot == 'PWL'] |> round(2)
ft_wind_height_off_ground <- ws_pars$ht_above_ground[ws_pars$plot == 'FT'] |> round(2)
pwl_frac_cpy_height_off_ground <- ws_pars$frac_cpy_height_off_ground[ws_pars$plot == 'PWL'] |> round(2)
ft_frac_cpy_height_off_ground <- ws_pars$frac_cpy_height_off_ground[ws_pars$plot == 'FT'] |> round(2)
event_sd <- ws_pars$event_mean_snow_depth |> mean() |> round(2)

# met of the snowfall periods
sf_event_avgs <- readRDS('data/event_avgs_maxmin.rds')
sf_event_t_range <- c(min(sf_event_avgs$min_t), max(sf_event_avgs$max_t)) |> round(1)
sf_event_u_range <- c(min(sf_event_avgs$min_u), max(sf_event_avgs$max_u)) |> round(1)
# sf_event_ip_range <- c(min(sf_event_avgs$min_IP_troughs), max(sf_event_avgs$max_IP_troughs))

scl_lai_cc <- read.csv('~/local-usask/analysis/interception/data/lai/scl_canopy_metrics.csv')

scl_lai_cc$Name <- c('Mixed', 'Sparse', 'Closed')

scl_meta <- read.csv('~/local-usask/analysis/interception/data/loadcell/calibrations/load_cell_meta_ac_fortress.csv')

### snow survey ----

lai_measurements <- read.csv('../../analysis/interception/data/lai/results/2023_compiled_lai_vza_15_60.csv')  |>
  filter(vza == 60)

cc_range <- paste(round(min(lai_measurements$cc), 2), '–', round(max(lai_measurements$cc), 2))
lai_range <- paste(round(min(lai_measurements$Le), 2), '–', round(max(lai_measurements$Le), 2))

## Results ----

### Influence of meteorology

mean_ip_by_trough <- readRDS('data/mean_ip_by_trough.rds')

mean_ip_sparse <- mean_ip_by_trough$IP[mean_ip_by_trough == 'sparse_forest'] |> round(2)
mean_ip_med <- mean_ip_by_trough$IP[mean_ip_by_trough == 'mixed'] |> round(2)
mean_ip_dense <- mean_ip_by_trough$IP[mean_ip_by_trough == 'dense_forest'] |> round(2)

### Influence of forest structure

# MET
event_avg_met <- readRDS('data/event_met/lidar_events_met_avg.rds') |>
  filter(event_id == '2023-03-14')
event_sf <- event_avg_met |> pull(`Cuml. Snowfall (mm)`) |> round(1)
snowing_met <- readRDS('data/event_met/lidar_events_met_avgs_snowing.rds') |>
  filter(event_id == '2023-03-14')

ft_ws_avg <- snowing_met |>
  pull(`mean FT Wind Speed (m/s)`) |>
  round(2)
pwl_ws_avg <- snowing_met |>
  pull(`mean PWL Wind Speed (m/s)`) |>
  round(2)
wd <- snowing_met |>
  pull(`mean FT Wind Dir. (°)`) |>
  round()
hm <- 0.9 # mean measured event parsivel
ft_ta <- traj_angle_deg(ft_ws_avg, hm) |>
  round(0)
pwl_ta <- traj_angle_deg(pwl_ws_avg, hm) |>
  round(0)

# HEMI STATS

# first found the theta range which corresponded to the upper 97.5^th^ percentile of rho values
vox_config_id <- "23_072_vox_len_0.25m_sa_gridgen_v2.0.0_sa"
pwl_hemi_stat <-
  readRDS(
    paste0(
      '../../analysis/lidar-processing/data/hemi_stats/hemi_raw_theta_phi_for_rho_s_upper_2_5th_percentile_',
      vox_config_id,
      "_",
      'PWL_E',
      '.rds'
    ))

ft_hemi_stat <-
  readRDS(
    paste0(
      '../../analysis/lidar-processing/data/hemi_stats/hemi_raw_theta_phi_for_rho_s_upper_2_5th_percentile_',
      vox_config_id,
      "_",
      'FSR_S',
      '.rds'
    ))


pwl_theta_to <- max(pwl_hemi_stat$theta_d) |> round()
ft_theta_to <- max(ft_hemi_stat$theta_d) |> round()
pwl_theta_from <- min(pwl_hemi_stat$theta_d) |> round()
ft_theta_from <- min(ft_hemi_stat$theta_d) |> round()

# then found the phi which had the best rho based on the theta range above
cor_stats <-
  readRDS('../../analysis/lidar-processing/data/hemi_stats/r2_vs_integrated_and_single_zentith.rds')
# find the zenith corresponding to the highest R value
cor_smry <- cor_stats |>
  group_by(plot_name, group) |>
  summarise(
    rp = max(rp),
    peak_r2 = max(r2),
    phi_at_peak_r2 = phi_d[which.max(r2)])

pwl_best_phi_rho <- cor_smry$rp[cor_smry$plot_name == 'PWL' & cor_smry$group == 'Single Zenith']  |> round(2)
ft_best_phi_rho <- cor_smry$rp[cor_smry$plot_name == 'FT' & cor_smry$group == 'Single Zenith'] |> round(2)

pwl_best_phi <- cor_smry$phi_at_peak_r2[cor_smry$plot_name == 'PWL' & cor_smry$group == 'Single Zenith']
ft_best_phi <- cor_smry$phi_at_peak_r2[cor_smry$plot_name == 'FT' & cor_smry$group == 'Single Zenith']

# model error stats
mod_error <- readRDS('../../analysis/lidar-processing/data/models/lca_vs_ip_model_error_ft_pwl_nadir_adj.rds')

ft_a_val <- mod_error$`Model Slope`[mod_error$`Plot Name` == 'FT' & mod_error$`Canopy Metrics` == 'Vector Based'] |> round(2)
pwl_a_val <- mod_error$`Model Slope`[mod_error$`Plot Name` == 'PWL' & mod_error$`Canopy Metrics` == 'Vector Based'] |> round(2)
ft_r2_val <- mod_error$R2_adj[mod_error$`Plot Name` == 'FT' & mod_error$`Canopy Metrics` == 'Vector Based'] |> round(2)
pwl_r2_val <- mod_error$R2_adj[mod_error$`Plot Name` == 'PWL' & mod_error$`Canopy Metrics` == 'Vector Based'] |> round(2)
pwl_a_val_nadir <- mod_error$`Model Slope`[mod_error$`Plot Name` == 'PWL' & mod_error$`Canopy Metrics` == 'Nadir'] |> round(2)
ft_a_val_nadir <- mod_error$`Model Slope`[mod_error$`Plot Name` == 'FT' & mod_error$`Canopy Metrics` == 'Nadir'] |> round(2)
pwl_r2_val_nadir <- mod_error$R2_adj[mod_error$`Plot Name` == 'PWL' & mod_error$`Canopy Metrics` == 'Nadir'] |> round(2)
ft_r2_val_nadir <- mod_error$R2_adj[mod_error$`Plot Name` == 'FT' & mod_error$`Canopy Metrics` == 'Nadir'] |> round(2)

ft_10m_example <- readRDS('data/event_met/ft_20230313_wind_speed_traj_angle_10m.rds')
ft_2m_example <- readRDS('data/event_met/ft_20230313_wind_speed_traj_angle_2m.rds')
ft_mean_wind_prof_integral <- readRDS('data/event_met/ft_20230313_mean_integral_wind_profile_and_traj.rds')
ft_mean_wind_speed_prof_integral <- ft_mean_wind_prof_integral$wind_speed |> round(1)

### Combined effects ----

hemi_stats <- readRDS('../../analysis/lidar-processing/data/hemi_stats/full_voxrs_not_filtered_to_swe_traj_angle_w_lca_23_072.rds') |>
  mutate(find_wind = abs(1-wind_speed))

select_wind_speed <- 1
pwl <- hemi_stats |> filter(plot_name == 'PWL')
ft <- hemi_stats |> filter(plot_name == 'FT')
pwl_inc <- pwl$lca_inc[which.min(abs(select_wind_speed-pwl$wind_speed))]
ft_inc <- ft$lca_inc[which.min(abs(select_wind_speed-ft$wind_speed))]
ta <- pwl$traj_angle[which.min(abs(select_wind_speed-pwl$wind_speed))]

pwl_nadir <- pwl$lca_nadir |> unique()
ft_nadir <- ft$lca_nadir |> unique()
pwl_frac_inc <- pwl_inc / pwl_nadir
ft_frac_inc <- ft_inc / ft_nadir

pwl_nls_coefs <- readRDS('../../analysis/lidar-processing/data/models/ta_vs_lca_nls_coefs_pwl.rds')
ft_nls_coefs <- readRDS('../../analysis/lidar-processing/data/models/ta_vs_lca_nls_coefs_pwl.rds')


ft_wp_pars <- readRDS(
  '../../analysis/eddy-cov/data/forest_tower_wind_profile_params_clean_ec_events.rds'
) |> select(-ustar) # need to calculate based on the event wind speed

### Throughfall model performance ----

vb_plot_scale_err <- readRDS('figs/lidar_periods/23_072_23_073_plot_scale_vb_model_error.rds') |>
  select(plot,
         val_name = name,
         mod_type,
         obs_val = `UAV-lidar`,
         mod_val = mod_vals,
         `Mean Bias` = bias,
         # MAE,
         `Perc. Error`)

pwl_vb_tf_mod <- vb_plot_scale_err$mod_val[vb_plot_scale_err$plot == 'PWL' & vb_plot_scale_err$mod_type == 'VB-model' & vb_plot_scale_err$val_name == 'tf'] |> round(2)
ft_vb_tf_mod <- vb_plot_scale_err$mod_val[vb_plot_scale_err$plot == 'FT' & vb_plot_scale_err$mod_type == 'VB-model' & vb_plot_scale_err$val_name == 'tf'] |> round(1)
pwl_vb_tf_obs <- vb_plot_scale_err$obs_val[vb_plot_scale_err$plot == 'PWL' & vb_plot_scale_err$mod_type == 'VB-model' & vb_plot_scale_err$val_name == 'tf'] |> round(1)
ft_vb_tf_obs <- vb_plot_scale_err$obs_val[vb_plot_scale_err$plot == 'FT' & vb_plot_scale_err$mod_type == 'VB-model' & vb_plot_scale_err$val_name == 'tf'] |> round(1)

pwl_vb_bias <- vb_plot_scale_err$`Mean Bias`[vb_plot_scale_err$plot == 'PWL' & vb_plot_scale_err$mod_type == 'VB-model' & vb_plot_scale_err$val_name == 'tf'] |> round(1)
ft_vb_bias <- vb_plot_scale_err$`Mean Bias`[vb_plot_scale_err$plot == 'FT' & vb_plot_scale_err$mod_type == 'VB-model' & vb_plot_scale_err$val_name == 'tf'] |> round(1)
pwl_nadir_bias <- vb_plot_scale_err$`Mean Bias`[vb_plot_scale_err$plot == 'PWL' & vb_plot_scale_err$mod_type == 'Nadir-model' & vb_plot_scale_err$val_name == 'tf'] |> round(1)
ft_nadir_bias <- vb_plot_scale_err$`Mean Bias`[vb_plot_scale_err$plot == 'FT' & vb_plot_scale_err$mod_type == 'Nadir-model' & vb_plot_scale_err$val_name == 'tf'] |> round(1)

alpha_plot_scale <- readRDS('data/model_results/alpha_plot_scale.rds') |> round(3)
