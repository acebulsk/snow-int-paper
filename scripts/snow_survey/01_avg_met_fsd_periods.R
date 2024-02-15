# script to average met data over snow survey periods
source('scripts/00_define_global_attributes.R')
source('scripts/snow_survey/00_load_snow_survey_data.R')

# to long
storms_long_datetime <-
  purrr::pmap_dfr(fsd_periods_wide |>
                    select(from = snowfall_start_time,
                           to = fsd_end_time,
                           event_id), to_long)


ffr_met <- left_join(ffr_met, storms_long_datetime) |>
  left_join(parsivel) |>
  filter(event_id |> is.na() == F)

ffr_met_avg_event <- ffr_met |>
  group_by(event_id) |>
  summarise(
    del_sf = sum(p),
    across(c(t:rh,Qsi,p, part_diam, part_vel), \(x) mean(x,
                                                         na.rm = TRUE)),
    mean_u = mean(u),
    med_u = median(u),
    stdev_u = sd(u)
  )

# classify wind

ffr_met_avg_event$wind_class <- ifelse(ffr_met_avg_event$med_u > 1.1, 'High (> 1.2 m/s)', 'Low (< 1.2 m/s)')
