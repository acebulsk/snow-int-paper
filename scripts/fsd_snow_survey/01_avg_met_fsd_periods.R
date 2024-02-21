# script to average met data over snow survey periods
source('scripts/00_define_global_attributes.R')
source('scripts/fsd_snow_survey/00_load_snow_survey_data.R')

# to long
storms_long_datetime <-
  purrr::pmap_dfr(fsd_periods_wide |>
                    select(from = snowfall_start_time,
                           to = fsd_end_time,
                           event_id), to_long)


ffr_met_wnd_events <- storms_long_datetime |>
  left_join(ffr_met_wnd)  |>
  left_join(parsivel, by = 'datetime') |>
  left_join(pwl_sf) |>
  filter(event_id |> is.na() == F)

# average over events
ffr_met_avg_event <- ffr_met_wnd_events |>
  group_by(event_id) |>
  summarise(
    del_sf = sum(ppt),
    across(c(air_temp:wind_dir, part_diam, part_vel), \(x) mean(x,
                                                         na.rm = TRUE)),
    mean_u = mean(wind_speed),
    med_u = median(wind_speed),
    stdev_u = sd(wind_speed)
  )

ffr_met_avg_event

# classify wind

ffr_met_avg_event$wind_class <- ifelse(ffr_met_avg_event$med_u > 1.2, 'High (> 1.2 m/s)', 'Low (< 1.2 m/s)')



