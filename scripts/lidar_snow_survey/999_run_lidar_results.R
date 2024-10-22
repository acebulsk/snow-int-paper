# Script to run lidar results sequence

source('scripts/00_define_global_attributes.R')
source('scripts/01_load_processed_data.R')
source('scripts/lidar_snow_survey/00_load_lidar_meta.R')
source('scripts/lidar_snow_survey/01_plot_lidar_met_time_series.R')
source('scripts/lidar_snow_survey/02_plot_wind_rose_lidar.R')
source('scripts/lidar_snow_survey/04_integrated_traj_angle.R')
source('scripts/lidar_snow_survey/05_test_vectorbased_model.R')

# not used currently
source('scripts/scl_snowfall_events/06_stats_on_15min_met_vs_ip.R')
