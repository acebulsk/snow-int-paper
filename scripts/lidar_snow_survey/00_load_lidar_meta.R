source('scripts/00_define_global_attributes.R')
#source('scripts/centralize_processed_data.R') # only need to run if making changes to raw data processing
source('scripts/load_processed_data.R')

pre_events <- c('22_045', '23_026', '23_072')
post_events <- c('22_047', '23_027', '23_073')

pre_traj_files <- paste0(
  '/media/alex/phd-data/local-usask/analysis/lidar-processing/data/metadata/drone_trajectory/',
  pre_events,
  '_all_lidar_trajectory.txt'
)

post_traj_files <- paste0(
  '/media/alex/phd-data/local-usask/analysis/lidar-processing/data/metadata/drone_trajectory/',
  post_events,
  '_all_lidar_trajectory.txt'
)

# manual snow survey metadata
fsd_meta <-
  read.csv('../../analysis/snow-stats/data/processed/fsd_survey_periods_w_snowfall_start_times.csv', skip = 1) |>
  mutate(
    snowfall_start_time = as.POSIXct(snowfall_start_time, tz = 'Etc/GMT+6'),
    snowfall_end_time = as.POSIXct(snowfall_end_time, tz = 'Etc/GMT+6'),
    fsd_end_time = as.POSIXct(fsd_end_time, tz = 'Etc/GMT+6'),
    event_id = as.Date(event_id, tz = 'Etc/GMT+6')) |> select(event_id,
                                                              snowfall_start_time,
                                                              sample_type)
# lidar related metadata
# start_time <- sapply(pre_traj_files, get_traj_time, USE.NAMES = F) |> as.POSIXct(tz = 'Etc/GMT+6')
#
# end_time <- sapply(post_traj_files, get_traj_time, USE.NAMES = F) |> as.POSIXct(tz = 'Etc/GMT+6')
#
# scan_dates <- data.frame(
#   pre_jday = pre_events,
#   post_jday = post_events,
#   from = lubridate::round_date(start_time, '15 minutes'),
#   to = lubridate::round_date(end_time, '15 minutes'),
#   event_id = as.Date(end_time, tz = 'Etc/GMT+6')
# )

# saveRDS(scan_dates, 'data/lidar_event_wide.rds')

scan_dates <- readRDS('data/lidar_event_wide.rds')

# currently set this so we have periods of snowfall
# i.e., from is snowfall start time not takeoff time
lidar_events_long_dt <-
  purrr::pmap_dfr(scan_dates |>
                    select(from = from,
                           to = to,
                           event_id), to_long)

