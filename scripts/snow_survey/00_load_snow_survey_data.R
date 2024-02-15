# load snow survey related data

good_canopies <- c('O', 'M') # dont include edge samples for fsd

fsd_all <- readRDS('../../analysis/snow-stats/data/processed/fresh_snow_densities_with_ground_partials.rds') |>
  mutate(
    datetime = as.POSIXct(round(datetime, 'hours')),
    event_id = as.Date(datetime)) |>
  filter(
    canopy %in% good_canopies) |>
  rename(del_tf = swe_partial)

fsd_all <- fsd_all[!(fsd_all$transect == 'T1' & fsd_all$num == 12), ]

fsd_periods_wide <-
  read.csv('../../analysis/snow-stats/data/processed/fsd_survey_periods_w_snowfall_start_times.csv', skip = 1) |>
  mutate(
    snowfall_start_time = as.POSIXct(snowfall_start_time, tz = 'Etc/GMT+6'),
    snowfall_end_time = as.POSIXct(snowfall_end_time, tz = 'Etc/GMT+6'),
    fsd_end_time = as.POSIXct(fsd_end_time, tz = 'Etc/GMT+6'),
    event_id = as.Date(event_id, tz = 'Etc/GMT+6')) |>
  filter(include == T)
