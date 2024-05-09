library(tidyverse)
source('scripts/00_define_global_attributes.R')
source('scripts/lidar_snow_survey/00_load_lidar_meta.R')
source('scripts/examples/hydrometeor_traj_angle_contact_number.R')

select_event <- '2023-03-14'
ffr_met_wnd_lidar_events <- lidar_events_long_dt |>
  left_join(ffr_met_wnd)  |>
  left_join(parsivel, by = 'datetime') |>
  left_join(pwl_sf) |>
  filter(event_id == select_event,
         ppt > 0)

median_wind_dir <- median(ffr_met_wnd_lidar_events$wind_dir)
median_vel <- mean(ffr_met_wnd_lidar_events$part_vel, na.rm = T)

ffr_met_wnd_lidar_events$traj_angle <-
  traj_angle_deg(ffr_met_wnd_lidar_events$wind_speed, median_vel)

ggplot(ffr_met_wnd_lidar_events |> pivot_longer(c(part_vel, wind_speed, traj_angle)),
       aes(datetime, value, colour = name)) +
  geom_line() +
  facet_grid(rows = vars(name), scales = 'free_y')

mean_traj_angle <- mean(ffr_met_wnd_lidar_events$traj_angle)
median_traj_angle <- median(ffr_met_wnd_lidar_events$traj_angle)


select_event <- '2023-01-27'
ffr_met_wnd_lidar_events <- lidar_events_long_dt |>
  left_join(ffr_met_wnd)  |>
  left_join(parsivel, by = 'datetime') |>
  left_join(pwl_sf) |>
  filter(event_id == select_event)

median_vel <- 1

ffr_met_wnd_lidar_events$traj_angle <-
  traj_angle_deg(ffr_met_wnd_lidar_events$wind_speed, median_vel)

ggplot(ffr_met_wnd_lidar_events |> pivot_longer(c(part_vel, wind_speed, traj_angle)),
       aes(datetime, value, colour = name)) +
  geom_line() +
  facet_grid(rows = vars(name), scales = 'free_y')
