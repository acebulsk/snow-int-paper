# Script to load processed data files used in manuscript. See
# centralize_processed_data.R script for origin of files.
paper_lysimeter_data_path <- 'data/lysimeter-data/raw/'

scl_names_dict2 <- data.frame(
  note = c('Trough 1', 'Trough 2', 'Trough 3'),
  new_name = c('Mixed', 'Sparse', 'Closed')
)
cc_troughs <- read.csv('data/hemisphere-photo-data/2025_compiled_troughs_auto_th.csv')
scl_lai_cc <- read.csv('data/hemisphere-photo-data/lai_site_id_2022_08_31.csv') |>
  # mutate(id = paste0('DSCN6', id, '.JPG')) |>
  filter(grepl("Trough", note)) |> # just keep troughs
  left_join(cc_troughs) |>
  left_join(scl_names_dict2) |>
  # filter(vza %in% c(10, 20, 30, 60, 90)) |>
  select(trough_name = new_name, vza, Le, cc) |>
  mutate(cc = round(cc, 2)) |>
  filter(!is.na(cc))

throughfall_periods <- read.csv(paste0(paper_lysimeter_data_path,
                                       'select_storms_datetime_wide_independent_snow_surveys.csv')
                                , skip = 1)

# get periods updated to not include times where troughs are unloading

throughfall_periods_wide <- throughfall_periods |>
  filter(quality < 4,
         event_starts_late == F) |>
  mutate(
    from = as.POSIXct(from, tz = 'Etc/GMT+6'),
    to = as.POSIXct(to, tz = 'Etc/GMT+6'),
    storm_id = format(from, "%Y-%m-%d_%H")) |>
  select(from, to, w_tree_event, storm_id, bad_troughs)

to_long_tf <- function(from, to, w_tree_event, storm_id, bad_troughs){
  datetime <- seq(from, to, 900)

  out <- data.frame(datetime, w_tree_event, storm_id, bad_troughs)

  return(out)
}

throughfall_periods_long <- purrr::pmap_dfr(throughfall_periods_wide, to_long_tf)
throughfall_periods_long$bad_troughs <- gsub('dense', 'closed', throughfall_periods_long$bad_troughs)
# raw weighed tree data calibrated to snow survey stations that have average
# canopy closure the same as the SCLs
load_suffix <- 'fsd_cal_for_each_trough_vza_60' # calibrated tree to snow surveys selected to match canopy closure of each SCL at inclination angle up to 60

w_tree_zrd <- readRDS(
  paste0(
    'data/lysimeter-data/processed/zero_weighed_tree_kg_m2_pre_post_cnpy_snow_',
    load_suffix,
    '.rds'
  )
) |> select(datetime, trough_name = tree_cal_trough_name, tree_cal_cc, weighed_tree_canopy_load_mm = value)

w_tree_zrd <- readRDS(
  paste0(
    'data/lysimeter-data/processed/zero_weighed_tree_kg_m2_pre_post_cnpy_snow_',
    load_suffix,
    '.rds'
  )
) |> select(datetime, trough_name = tree_cal_trough_name, tree_cal_cc, weighed_tree_canopy_load_mm = value)

ggplot(w_tree_zrd, aes(datetime, weighed_tree_canopy_load_mm)) +
  geom_line() +
  facet_grid(rows = vars(trough_name))

q_int_tree <-
  readRDS(
    paste0(
      'data/lysimeter-data/processed/zero_weighed_tree_kg_m2_pre_post_cnpy_snow_',
      load_suffix,
      '.rds'
    )) |>
  filter(!event_id %in% c('2022-01-17')) |>
  group_by(tree_cal_trough_name, event_id) |>
  mutate(d_int = ifelse(is.na(lag(value)), 0, value - lag(value)),
         d_int = ifelse(d_int<0, 0, d_int),
         q_int = d_int * 4) |> # mm/15 min to mm /hr
  select(datetime, trough_name = tree_cal_trough_name, tree_cal_cc, value, q_int, d_int) |>
  ungroup()

ggplot(q_int_tree |> pivot_longer(c(value:d_int)), aes(datetime, value)) +
  geom_line() +
  facet_grid(rows = vars(name))

scl_raw_kg <- readRDS(paste0(paper_lysimeter_data_path, 'treefort_scls_raw_kg.rds')) |> select(datetime, trough_name = name, scl_raw_kg = value)
scl_raw <- readRDS(paste0(paper_lysimeter_data_path, 'treefort_scl_qaqc.rds'))
scl_meta <- read.csv(paste0(paper_lysimeter_data_path, 'load_cell_meta_ac_fortress.csv')) # need surface area to bring interval measurements in kg/m2 back to kg
scl_meta$trough_name <- c('mixed', 'sparse', 'closed')

q_tf_scl <- scl_raw |>
  group_by(name) |>
  mutate(d_tf = ifelse(is.na(lag(value)), 0, value - lag(value)),
         d_tf = ifelse(d_tf<0, 0, d_tf),
         q_tf = d_tf * 4) |> # mm/15 min to mm /hr
  select(datetime, name, d_tf, q_tf) |>
  left_join(throughfall_periods_long) |>
  mutate(row_flag = mapply(grepl, name, bad_troughs)) |> # remove troughs where only one is unloading, could also jsut use more reliable medium trough but would have to remove some events (see notes)
  ungroup() |>
  filter(row_flag == F,
         is.na(q_tf) == F) |>
  select(datetime, trough_name = name, d_tf, q_tf) |>
  ungroup()

q_tf_scl_avg <- q_tf_scl |>
  group_by(datetime) |>
  summarise(q_tf = mean(q_tf, na.rm = T))

met_intercept <- readRDS(paste0('data/lysimeter-data/processed/',
                                '/continuous_throughfall_data_binned_met_select_events.rds'))
met_intercept$trough_name <- paste0(toupper(substr(met_intercept$trough_name, 1, 1)), substr(met_intercept$trough_name, 2, nchar(met_intercept$trough_name)))
met_intercept$trough_name <- factor(met_intercept$trough_name, levels = c('Sparse', 'Mixed', 'Closed'))

parsivel <- readRDS('data/parsivel-data/disdro_spectrum_processed_agg_15_min.RDS')

ffr_met <- readRDS('data/met-data/ffr_crhm_modelling_obs.rds') |>
  mutate(q_sf = p * 4) # mm/15min to mm/hr
ffr_met_wnd <- readRDS('data/met-data/ffr_t_rh_u_qaqc_fill.rds')
# not enough ec wind obs over the event
ffr_ec <- readRDS('data/met-data/ec_high_tower_30_min_2021_2023_qc_rough.rds') |>
  mutate(ec_wind_dir = wind_dir_mag - calg_mag_declination) |>
  select(datetime, ec_wind_speed = wind_speed, ec_wind_dir)
# ffr_ec <- readRDS('../../analysis/eddy-cov/data/low-tower/low_tower_15min_2021_2023_qc_rough.rds') |>
#   mutate(ec_wind_dir = wind_dir_mag - calg_mag_declination) |>
#   select(datetime, ec_wind_speed = wind_speed, ec_wind_dir)
pwl_sf <- readRDS('data/met-data/pwl_pluvio_15_min_qaqc_undercatch_corr_ac.rds')
pwl_pluvio_raw <- readRDS('data/met-data/pwl_pluvio_15_min_raw.rds')
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

lidr_data_path <- 'data/lidar-data/'
pre_post_id <- '23_072_23_073'
vox_config_id <- '_v2.0.0_sa_'
ip_config_id <- '_ip_normalised_resample_0.25_crop_mask'
plot <- 'FSR_S'

ft_ip_obs_rast <- rast(
  paste0(
    lidr_data_path,
    paste0(pre_post_id, vox_config_id, plot, ip_config_id, '.tif')
  ))
plot <- 'PWL_E'
pwl_ip_obs_rast <- rast(
  paste0(
    lidr_data_path,
    paste0(pre_post_id, vox_config_id, plot, ip_config_id, '.tif')
  ))

lidar_data_stats_path <- 'data/lidar-data/stats/'

ft_cc_025 <- readRDS(paste0(
  lidar_data_stats_path,
  'ft_lca_avg_event_theta_for_each_phi.rds'))

pwl_cc_025 <- readRDS(paste0(
  lidar_data_stats_path,
  'pwl_lca_avg_event_theta_for_each_phi.rds'))
lidar_data_models_path <- 'data/lidar-data/models/'
cp_sine_model_coef <- readRDS(paste0(lidar_data_models_path,
                                     'ta_vs_lca_nls_sin_fn_fit_both_ft_pwl_coefs.rds')
                              )
