# Script to centralize processed data used in publication documented here to
# show where data files originate from

# LYSIMETER DATA ----

# See github repo https://github.com/acebulsk/interception for creation of this dataset
paper_lysimeter_data_path <- 'data/lysimeter-data/raw/'

file.copy(from = '../../analysis/interception/data/select_storms_datetime_wide_independent_snow_surveys.csv',
          to = paper_lysimeter_data_path,
          recursive = F,
          overwrite = F,
          copy.date = T)

file.copy(from = '../../working-papers/ablation-paper/data/raw-data/snow_in_canopy_pre_and_post_snowfall.csv',
          to = paper_lysimeter_data_path,
          recursive = F,
          overwrite = F,
          copy.date = T)

load_suffix <- 'fsd_cal_for_each_trough_vza_60' # calibrated tree to snow surveys selected to match canopy closure of each SCL at inclination angle up to 60
file.copy(
  from = paste0('../../analysis/interception/data/loadcell/treefort_weighed_tree_cal_kg_m2_plv_',
                load_suffix,
                '.rds'),
  to = paper_lysimeter_data_path,
  recursive = F,
  overwrite = F,
  copy.date = T
)

file.copy('../../analysis/interception/data/loadcell/treefort_scl_qaqc.rds',
          to = paper_lysimeter_data_path,
          overwrite = F,
          copy.date = T)

file.copy('../../analysis/interception/data/loadcell/treefort_scls_raw_kg.rds',
          to = paper_lysimeter_data_path,
          overwrite = F,
          copy.date = T)

file.copy('../../analysis/interception/data/loadcell/calibrations/load_cell_meta_ac_fortress.csv',
          to = paper_lysimeter_data_path,
          overwrite = F,
          copy.date = T)

# LIDAR DATA ----
# See github repo https://github.com/acebulsk/lidar-processing for creation of this dataset

raw_cor_stats_path <- '../../analysis/lidar-processing/data/hemi_stats/r2_vs_integrated_and_single_zentith.rds'
paper_cor_stats_path <- 'data/lidar-data/stats'
file.copy(from = raw_cor_stats_path,
          to = paper_cor_stats_path,
          recursive = F,
          overwrite = F,
          copy.date = T)

base_path <- '../../analysis/lidar-processing/data/dsm_ip/'
lidr_data_path <- 'data/lidar-data/'
pre_post_id <- '23_072_23_073'
vox_config_id <- '_v2.0.0_sa_'
ip_config_id <- '_ip_normalised_resample_0.25_crop_mask'
plot <- 'FSR_S'
file.copy(from = paste0(base_path, pre_post_id, vox_config_id, plot, ip_config_id, '.tif'),
          to = lidr_data_path,
          recursive = F,
          overwrite = F,
          copy.date = T)

plot <- 'PWL_E'
file.copy(from = paste0(base_path, pre_post_id, vox_config_id, plot, ip_config_id, '.tif'),
          to = lidr_data_path,
          recursive = F,
          overwrite = F,
          copy.date = T)

ft_lca_avg_path <- '../../analysis/lidar-processing/data/hemi_stats/ft_lca_avg_event_theta_for_each_phi.rds'
lidar_data_stats_path <- 'data/lidar-data/stats/'
file.copy(from = ft_lca_avg_path,
          to = lidar_data_stats_path,
          recursive = F,
          overwrite = F,
          copy.date = T)

pwl_lca_avg_path <- '../../analysis/lidar-processing/data/hemi_stats/pwl_lca_avg_event_theta_for_each_phi.rds'
file.copy(from = pwl_lca_avg_path,
          to = lidar_data_stats_path,
          recursive = F,
          overwrite = F,
          copy.date = T)

vox_config_id <- "23_072_vox_len_0.25m_sa_gridgen_v2.0.0_sa"
hemi_stats_paths <- list.files('../../analysis/lidar-processing/data/hemi_stats/',
                               pattern = paste0(
                                 'hemi_raw_theta_phi_for_rho_s_upper_2_5th_percentile_',
                                 vox_config_id,
                                 '_*'),
                               full.names = T)
file.copy(
  from = hemi_stats_paths,
  to = lidar_data_stats_path,
  recursive = F,
  overwrite = F,
  copy.date = T
)

lidar_data_model_files <- list.files('../../analysis/lidar-processing/data/models/',
                                     full.names = T)
lidar_data_models_path <- 'data/lidar-data/models/'
file.copy(from = lidar_data_model_files,
          to = lidar_data_models_path,
          recursive = F,
          overwrite = F,
          copy.date = T)

tree_height_paths <- list.files('../../analysis/lidar-processing/data/lidR_canopy_metrics/',
                                pattern = '*mean_tree*', full.names = T)
file.copy(from = tree_height_paths,
          to = lidr_data_path,
          recursive = F,
          overwrite = F,
          copy.date = T)

file.copy(from = '../../analysis/lidar-processing/data/hemi_stats/full_voxrs_not_filtered_to_swe_traj_angle_w_lca_23_072.rds',
          to = lidar_data_stats_path,
          recursive = F,
          overwrite = F,
          copy.date = T)

# HEMISPHERIC PHOTO DATA ----

# LAI measurements of the snow survey stns
file.copy(
  from = '../../analysis/interception/data/lai/results/2023_compiled_lai_vza_15_60.csv',
  to = 'data/hemisphere-photo-data/',
  recursive = F,
  overwrite = F,
  copy.date = T)

# this one has the LAI measurements of the troughs
file.copy(
  from = '../../analysis/interception/data/lai/lai_site_id_2022_08_31.csv',
  to = 'data/hemisphere-photo-data/',
  recursive = F,
  overwrite = F,
  copy.date = T)
file.copy(
  from = '../../analysis/interception/data/lai/results/2025_compiled_troughs_auto_th.csv',
  to = 'data/hemisphere-photo-data/',
  recursive = F,
  overwrite = F,
  copy.date = T)



# WIND PROFILE COEFs ----

file.copy(
  from = '../../analysis/eddy-cov/data/forest_tower_wind_profile_params_clean_ec_events.rds',
  to = 'data/wind-profile-data/',
  recursive = F,
  overwrite = F,
  copy.date = T)

# PARSIVEL DATA ----

file.copy(
  from = '../../analysis/disdrometer/data/disdro_spectrum_processed_agg_15_min.RDS',
  to = 'data/parsivel-data/',
  recursive = F,
  overwrite = F,
  copy.date = T)

# MET DATA ----

met_file_paths <- c(
  '../../analysis/met-data-processing/data/ffr_crhm_modelling_obs.rds',
  '../../analysis/met-data-processing/data/ffr_t_rh_u_qaqc_fill.rds',
  '../../analysis/eddy-cov/data/high-tower/ec_high_tower_30_min_2021_2023_qc_rough.rds',
  '../../analysis/met-data-processing/data/pluvio-qaqc/pwl_pluvio_15_min_qaqc_undercatch_corr_ac.rds',
  '../../analysis/met-data-processing/data/pluvio-qaqc/pwl_pluvio_15_min_raw.rds',
  '../../analysis/met-data-processing/data/pwl_met_qaqc.rds'
)

file.copy(
  from = met_file_paths,
  to = 'data/met-data/',
  recursive = F,
  overwrite = F,
  copy.date = T)


