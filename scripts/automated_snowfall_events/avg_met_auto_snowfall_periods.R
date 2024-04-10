# script to average met data over snow survey periods
source('scripts/00_define_global_attributes.R')
source('scripts/fsd_snow_survey/00_load_snow_survey_data.R')

# average over events
ffr_met_avg_event <- met_intercept |>
  group_by(storm_id) |>
  summarise(
    del_sf = sum(p),
    mean_u = mean(u),
    med_u = median(u),
    stdev_u = sd(u),
    across(c(t:p, part_diam, part_vel, IP), \(x) mean(x,
                                                                na.rm = TRUE))
  )

ffr_met_avg_event

# classify wind

ffr_met_avg_event$wind_class <- ifelse(ffr_met_avg_event$med_u > 1.2, 'High (> 1.2 m/s)', 'Low (< 1.2 m/s)')


