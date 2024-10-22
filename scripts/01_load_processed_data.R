# Script to load processed data files used in manuscript. See
# centralize_processed_data.R script for origin of files.

scl_lai_cc <- read.csv('data/hemisphere-photo-data/scl_canopy_metrics.csv')
scl_lai_cc$Name <- c('Mixed', 'Sparse', 'Closed')

throughfall_periods <- read.csv('data/lysimeter-data/select_storms_datetime_wide_independent_snow_surveys.csv', skip = 1)

load_df <- readRDS('data/lysimeter-data/treefort_load_main_cal_plv_fsd_mm.rds')

met_intercept <- readRDS(paste0(paper_lysimeter_data_path,
                                '/continuous_throughfall_data_binned_met_select_events.rds')) |>
  filter(q_sf > 0,
         d_tf > 0.01,
         # u <= 2,
         q_sf > q_tf) |> # if troughs > q_sf may be some unloading
  mutate(
    q_int = q_sf - q_tf,
    IP = q_int / q_sf) |>
  filter(IP < 1) |>
  left_join(scl_names_dict)

parsivel <- readRDS('data/parsivel-data/disdro_spectrum_processed_agg_15_min.RDS')

ffr_met <- readRDS('data/met-data/ffr_crhm_modelling_obs.rds')
ffr_met_wnd <- readRDS('data/met-data/ffr_t_rh_u_qaqc_fill.rds')
# not enough ec wind obs over the event
ffr_ec <- readRDS('data/met-data/ec_high_tower_30_min_2021_2023_qc_rough.rds') |>
  mutate(ec_wind_dir = wind_dir_mag - calg_mag_declination) |>
  select(datetime, ec_wind_speed = wind_speed, ec_wind_dir)
# ffr_ec <- readRDS('../../analysis/eddy-cov/data/low-tower/low_tower_15min_2021_2023_qc_rough.rds') |>
#   mutate(ec_wind_dir = wind_dir_mag - calg_mag_declination) |>
#   select(datetime, ec_wind_speed = wind_speed, ec_wind_dir)
pwl_sf <- readRDS('data/met-data/pwl_pluvio_15_min_qaqc_undercatch_corr_ac.rds')
pwl_wind <- readRDS('data/met-data/pwl_met_qaqc.rds') |>
  select(
    datetime,
    wind_speed = WindSpeed_S_WVT,
    wind_dir_true = WindDir_D1_WVT # keir confirmed junction box is pointed at 180 deg south (true) as per spec
    # sd_wind_dir = WindDir_SD1_WVT
  )

# LIDAR DATA ----

cor_stats <-
  readRDS('data/lidar-data/stats/r2_vs_integrated_and_single_zentith.rds')

# base_path <- '../../analysis/lidar-processing/data/dsm_ip/'
# lidr_data_path <- 'data/lidar-data/'
# pre_post_id <- '23_072_23_073'
# vox_config_id <- '_v2.0.0_sa_'
# ip_config_id <- '_ip_normalised_resample_0.25_crop_mask'
# plot <- 'FSR_S'
# ft_ip_obs_rast <- rast(
#   paste0(
#     lidr_data_path,
#     paste0(pre_post_id, vox_config_id, plot, ip_config_id, '.tif')
#   ))
# plot <- 'PWL_E'
# pwl_ip_obs_rast <- rast(
#   paste0(
#     lidr_data_path,
#     paste0(pre_post_id, vox_config_id, plot, ip_config_id, '.tif')
#   ))
#
# ft_cc_025 <- readRDS(paste0(
#   lidar_data_stats_path,
#   'ft_lca_avg_event_theta_for_each_phi.rds'))
#
# pwl_cc_025 <- readRDS(paste0(
#   lidar_data_stats_path,
#   'pwl_lca_avg_event_theta_for_each_phi.rds'))
#
# ft_nls_coefs <- readRDS(
#   paste0(
#     lidar_data_models_path,
#     'ta_vs_lca_nls_coefs_ft.rds'))
#
# pwl_nls_coefs <- readRDS(
#   paste0(
#     lidar_data_models_path,
#     'ta_vs_lca_nls_coefs_pwl.rds'))
