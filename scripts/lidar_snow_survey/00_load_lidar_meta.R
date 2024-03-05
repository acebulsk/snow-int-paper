source('scripts/00_define_global_attributes.R')

fsd_meta <-
  read.csv('../../analysis/snow-stats/data/processed/fsd_survey_periods_w_snowfall_start_times.csv', skip = 1) |>
  mutate(
    snowfall_start_time = as.POSIXct(snowfall_start_time, tz = 'Etc/GMT+6'),
    snowfall_end_time = as.POSIXct(snowfall_end_time, tz = 'Etc/GMT+6'),
    fsd_end_time = as.POSIXct(fsd_end_time, tz = 'Etc/GMT+6'),
    event_id = as.Date(event_id, tz = 'Etc/GMT+6')) |> select(event_id,
                                                              snowfall_start_time,
                                                              sample_type)

# custom file from maddie
scan_dates <- readxl::read_xlsx('../../analysis/lidar-processing/data/metadata/Harasyn_DroneProcessing2023.xlsx', sheet = 'FT') |>
  select(date = Date,
         from = `Takeoff Time`,
         to = `Land Time`,
         event_id = snow_survey_id) |>
  mutate(year = paste0('20', as.numeric(substr(date, 1, 2))),
         jday = substr(date, 4,6),
         from_time = strftime(from, format = '%H:%M:%S', tz = 'UTC'),
         from_datetime = paste0(year, ' ', jday, ' ', from_time),
         from_datetime = as.POSIXct(from_datetime, format = '%Y %j %H:%M:%S', tz = 'Etc/GMT+6'),
         from = lubridate::floor_date(from_datetime, unit="15 mins"),
         to_time = strftime(to, format = '%H:%M:%S', tz = 'UTC'),
         to_datetime = paste0(year, ' ', jday, ' ', to_time),
         to_datetime = as.POSIXct(to_datetime, format = '%Y %j %H:%M:%S', tz = 'Etc/GMT+6'),
         to = lubridate::ceiling_date(to_datetime, unit="15 mins"),
         event_id = as.Date(event_id)
  ) |>
  select(from, to, flight_id = date, event_id)

lidar_event_periods <- scan_dates |>
  filter(!is.na(event_id)) |>
  group_by(event_id) |>
  summarise(from = min(from),
            to = max(to),
            pre_flight_id = min(flight_id),
            post_flight_id = max(flight_id)) |>
  ungroup() |>
  left_join(fsd_meta, by = 'event_id')

# currently set this so we have periods of snowfall
# i.e., from is snowfall start time not takeoff time
lidar_events_long_dt <-
  purrr::pmap_dfr(lidar_event_periods |>
                    select(from = snowfall_start_time,
                           to = to,
                           event_id), to_long)
