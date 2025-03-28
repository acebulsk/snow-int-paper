# Bring values into environment for manuscript
setwd('~/local-usask/working-papers/snow-int-paper/') # need this to run for the thesis-outline proj
# source('../../analysis/disdrometer/scripts/00_source_functions.R')
library(dplyr)
library(ggpubr)

source('scripts/00_define_global_attributes.R')
source('scripts/01_load_processed_data.R')

## data and methods ----
lidar_data_path <- 'data/lidar-data/'
# CANOPY
ft_mean_ht <- readRDS(paste0(lidar_data_path, 'frs_s_mean_tree_height.rds')) |>
  round(1)
pwl_mean_ht <- readRDS(paste0(lidar_data_path, 'pwl_e_mean_tree_height.rds')) |>
  round(1)

# these data have canopy metrics even where we do not have snow depths...
nadir_cc <-
  readRDS(
    paste0(
      lidar_data_path,
      'stats/full_voxrs_not_filtered_to_swe_traj_angle_w_lca_23_072.rds'
    ))

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
sf_event_avgs <- readRDS('data/event_met/event_avgs_maxmin.rds')
sf_event_t_range <- c(min(sf_event_avgs$min_t), max(sf_event_avgs$max_t)) |> round(1)
sf_event_u_range <- c(min(sf_event_avgs$min_u), max(sf_event_avgs$max_u)) |> round(1)
# sf_event_ip_range <- c(min(sf_event_avgs$min_IP_troughs), max(sf_event_avgs$max_IP_troughs))

### snow survey ----

lai_measurements <- read.csv('data/hemisphere-photo-data/2023_compiled_lai_vza_15_60.csv')  |>
  filter(vza == 60)

cc_range <- paste(round(min(lai_measurements$cc), 2), '–', round(max(lai_measurements$cc), 2))
lai_range <- paste(round(min(lai_measurements$Le), 2), '–', round(max(lai_measurements$Le), 2))

## Results ----

### Influence of meteorology

mean_ip_by_trough <- readRDS('data/lysimeter-data/mean_ip_by_trough.rds')

mean_ip_sparse <- mean_ip_by_trough$IP[mean_ip_by_trough == 'sparse_forest'] |> round(2)
mean_ip_med <- mean_ip_by_trough$IP[mean_ip_by_trough == 'mixed'] |> round(2)
mean_ip_dense <- mean_ip_by_trough$IP[mean_ip_by_trough == 'dense_forest'] |> round(2)

lysimeter_hourly_mod_smry <-
  readRDS('data/lysimeter-data/processed/lysimter_hourly_avg_regression_stats.rds')

scl_hourly_stats <- readRDS('data/lysimeter-data/processed/lysimter_hourly_avg_wilcox_stats.rds') |>
  mutate(median_low = round(median_low, 2),
         median_high = round(median_high, 2)) |>
  select(-test_type)

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
      'data/lidar-data/stats/hemi_raw_theta_phi_for_rho_s_upper_2_5th_percentile_',
      vox_config_id,
      "_",
      'PWL_E',
      '.rds'
    ))

ft_hemi_stat <-
  readRDS(
    paste0(
      'data/lidar-data/stats/hemi_raw_theta_phi_for_rho_s_upper_2_5th_percentile_',
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
mod_error <- readRDS('data/lidar-data/stats/lca_vs_ip_model_error_ft_pwl_nadir_adj.rds')

ft_a_val <- mod_error$`Model Slope`[mod_error$`Plot Name` == 'FT' & mod_error$`Canopy Metrics` == 'Vector Based'] |> round(2)
pwl_a_val <- mod_error$`Model Slope`[mod_error$`Plot Name` == 'PWL' & mod_error$`Canopy Metrics` == 'Vector Based'] |> round(2)
ft_r2_val <- mod_error$R2_adj[mod_error$`Plot Name` == 'FT' & mod_error$`Canopy Metrics` == 'Vector Based'] |> round(2)
pwl_r2_val <- mod_error$R2_adj[mod_error$`Plot Name` == 'PWL' & mod_error$`Canopy Metrics` == 'Vector Based'] |> round(2)
pwl_a_val_nadir <- mod_error$`Model Slope`[mod_error$`Plot Name` == 'PWL' & mod_error$`Canopy Metrics` == 'Nadir'] |> round(2)
ft_a_val_nadir <- mod_error$`Model Slope`[mod_error$`Plot Name` == 'FT' & mod_error$`Canopy Metrics` == 'Nadir'] |> round(2)
pwl_r2_val_nadir <- mod_error$R2_adj[mod_error$`Plot Name` == 'PWL' & mod_error$`Canopy Metrics` == 'Nadir'] |> round(2)
ft_r2_val_nadir <- mod_error$R2_adj[mod_error$`Plot Name` == 'FT' & mod_error$`Canopy Metrics` == 'Nadir'] |> round(2)

### Combined effects ----

hemi_stats <- readRDS('data/lidar-data/stats/full_voxrs_not_filtered_to_swe_traj_angle_w_lca_23_072.rds') |>
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

nls_sin_coefs <- readRDS('data/lidar-data/models/ta_vs_lca_nls_sin_fn_fit_both_ft_pwl_coefs.rds') |> round(2)
lca_mod_error <- readRDS('data/lidar-data/models/lca_obs_mod_sin_fn_error_tbl.rds') |> mutate(Model = ifelse(Model == 'nls', 'Eq. 10', Model))
pwl_errs_nls <- lca_mod_error |> filter(Model == 'Eq. 10', Plot == 'PWL') |> mutate(across(where(is.numeric), \(x) round(x, digits = 2))) |> rename(MB = `Mean Bias`, RMSE = `RMS Error`)
ft_errs_nls <- lca_mod_error |> filter(Model == 'Eq. 10', Plot == 'FT') |> mutate(across(where(is.numeric), \(x) round(x, digits = 2))) |> rename(MB = `Mean Bias`, RMSE = `RMS Error`)
pwl_errs_hp <- lca_mod_error |> filter(Model == 'HP98', Plot == 'PWL') |> mutate(across(where(is.numeric), \(x) round(x, digits = 2))) |> rename(MB = `Mean Bias`, RMSE = `RMS Error`)
ft_errs_hp <- lca_mod_error |> filter(Model == 'HP98', Plot == 'FT') |> mutate(across(where(is.numeric), \(x) round(x, digits = 2))) |> rename(MB = `Mean Bias`, RMSE = `RMS Error`)

lca_mod_error$`Mean Bias`[lca_mod_error$Plot == 'PWL' & lca_mod_error$Model == 'nls']

ft_wp_pars <- readRDS(
  'data/wind-profile-data/forest_tower_wind_profile_params_clean_ec_events.rds'
) |> select(-ustar) # need to calculate based on the event wind speed

### Throughfall model performance ----

vb_plot_scale_err <- readRDS('data/lidar-data/models/23_072_23_073_plot_scale_vb_model_error.rds') |>
  select(plot,
         val_name = name,
         mod_type,
         obs_val = `UAV-lidar`,
         mod_val = mod_vals,
         `Mean Bias` = bias,
         # MAE,
         `Perc. Error`)

pwl_vb_tf_mod <- vb_plot_scale_err$mod_val[vb_plot_scale_err$plot == 'PWL' & vb_plot_scale_err$mod_type == 'VB-model' & vb_plot_scale_err$val_name == 'tf'] |> round(1)
ft_vb_tf_mod <- vb_plot_scale_err$mod_val[vb_plot_scale_err$plot == 'FT' & vb_plot_scale_err$mod_type == 'VB-model' & vb_plot_scale_err$val_name == 'tf'] |> round(1)
pwl_vb_tf_obs <- vb_plot_scale_err$obs_val[vb_plot_scale_err$plot == 'PWL' & vb_plot_scale_err$mod_type == 'VB-model' & vb_plot_scale_err$val_name == 'tf'] |> round(1)
ft_vb_tf_obs <- vb_plot_scale_err$obs_val[vb_plot_scale_err$plot == 'FT' & vb_plot_scale_err$mod_type == 'VB-model' & vb_plot_scale_err$val_name == 'tf'] |> round(1)

pwl_vb_bias <- vb_plot_scale_err$`Mean Bias`[vb_plot_scale_err$plot == 'PWL' & vb_plot_scale_err$mod_type == 'VB-model' & vb_plot_scale_err$val_name == 'tf'] |> round(1)
ft_vb_bias <- vb_plot_scale_err$`Mean Bias`[vb_plot_scale_err$plot == 'FT' & vb_plot_scale_err$mod_type == 'VB-model' & vb_plot_scale_err$val_name == 'tf'] |> round(1)
pwl_nadir_bias <- vb_plot_scale_err$`Mean Bias`[vb_plot_scale_err$plot == 'PWL' & vb_plot_scale_err$mod_type == 'Nadir-model' & vb_plot_scale_err$val_name == 'tf'] |> round(1)
ft_nadir_bias <- vb_plot_scale_err$`Mean Bias`[vb_plot_scale_err$plot == 'FT' & vb_plot_scale_err$mod_type == 'Nadir-model' & vb_plot_scale_err$val_name == 'tf'] |> round(1)

alpha_plot_scale <- readRDS('data/model_results/alpha_plot_scale.rds') |> round(3)
