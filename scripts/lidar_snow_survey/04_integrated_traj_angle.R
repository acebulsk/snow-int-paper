# Determine the trajectory angle at various heights within the canopy
# Potentially aggregate this based on the portion of canopy elements at each height
# Using gap filled wind speed from forest tower at 4 m
# snowfall during these events precents the use of canopy top wind from the EC system
# TODO make plot of traj angle with height and avg wind speed below canopy
# TODO calculate area of typical needleleaf tree and use this as weighted average for each canopy height slice

sel_event <- "2023-03-14"
us_wind_height <- 4.28  # m, this was measured in situ, see joplin notes
ft_mean_ht <- readRDS('../../analysis/lidar-processing/data/lidR_canopy_metrics/frs_s_mean_tree_height.rds') |>
  round(2)
pwl_e_mean_ht <- readRDS('../../analysis/lidar-processing/data/lidR_canopy_metrics/pwl_e_mean_tree_height.rds') |>
  round(2)

# first we need the wind speed over the event at different heights

# developed a wind profile based on events where we knew the EC systems were clean and working
ft_wp_pars <- readRDS(
  '../../analysis/eddy-cov/data/forest_tower_wind_profile_params_clean_ec_events.rds'
) |> select(-ustar) # need to calculate based on the event wind speed

# get snowdepth to calculate actual wind speed sensor height
event_sd <- readRDS('../../analysis/met-data-processing/data/ffr_t_u_sd_qaqc_shortfill.rds') |>
  select(datetime, SnowDepth) |>
  mutate(SnowDepth = imputeTS::na_interpolation(SnowDepth, maxgap = Inf)) |>  # fill all with linear
  left_join(lidar_events_long_dt) |>
  group_by(event_id) |>
  summarise(SnowDepth = mean(SnowDepth)) |>
  filter(event_id == sel_event) |>
  pull(SnowDepth)

event_precip <-
  ffr_met_lidr_events_avg$`Cuml. Snowfall (mm)`[ffr_met_lidr_events_avg$event_id == sel_event]
event_avg_wind <-
  ffr_met_lidr_events_avg$`mean FT Wind Speed (m/s)`[ffr_met_lidr_events_avg$event_id == sel_event]

# calculate the wind profile based on the wind speed observed over the event and
# the roughness and displacement height measured over several events

event_avg_ustar <- trbtransfeR::friction_velocity(
  uMeas = event_avg_wind,
  zHeight = us_wind_height - event_sd,
  d_0 = ft_wp_pars$d_0,
  z_0m = ft_wp_pars$z_0m,
  phi_m = 0
)

mod_df <- data.frame(avg_height = seq(ft_wp_pars$d_0+ft_wp_pars$z_0m, pwl_e_mean_ht, 0.01))

mod_df$avg_wind <- trbtransfeR::fit_neutral_wind_helpr(c(event_avg_ustar,
                                                         ft_wp_pars$z_0m,
                                                         ft_wp_pars$d_0),
                                                       mod_df$avg_height)
# plot the wind profile
ggplot(mod_df, aes(avg_wind, avg_height)) +
  geom_line() +
  geom_point(aes(x = event_avg_wind, y = us_wind_height - event_sd))

# avg wind over the profile over the event for forest tower and pwl which have different heights

# FT First

# by integral

# Create a function to calculate wind speed for a given height
wind_speed_function <- function(h) {
  trbtransfeR::fit_neutral_wind_helpr(c(event_avg_ustar, ft_wp_pars$z_0m, ft_wp_pars$d_0), h)
}

# Define the range of heights
heights <- ft_wp_pars$z_0m+ft_wp_pars$d_0:ft_mean_ht

# Use the integrate function to calculate the integral of the wind speed function over the height range
mean_wind_speed <- function(h1, h2, wind_speed_function) {
  integral <- integrate(wind_speed_function, lower = h1, upper = h2)$value
  mean_speed <- integral / (h2 - h1)
  return(mean_speed)
}

ft_mean_speed <- mean_wind_speed(min(heights), max(heights), wind_speed_function)
ft_mean_speed

ft_mean_speed_traj <- traj_angle_deg(ft_mean_speed, mean_vel)

saveRDS(
  list(wind_speed = ft_mean_speed, traj_angle = ft_mean_speed_traj),
  'data/event_met/ft_20230313_mean_integral_wind_profile_and_traj.rds'
)

# by mean of sequence
ft_mean_speed_discrete <- trbtransfeR::fit_neutral_wind_helpr(c(event_avg_ustar,
                                      ft_wp_pars$z_0m,
                                      ft_wp_pars$d_0),
                                    seq(ft_wp_pars$z_0m+ft_wp_pars$d_0, ft_mean_ht, by = 0.01)) |> mean()
ft_mean_speed_discrete

# PWL
# doesnt make sense to do PWL because we did not observed the u star/ z0 /d0
# here and therefore falsely extrapolating to higher canopy. Althought these
# implications would be interesting, i.e., that a taller canopy is exposed to
# higher wind speeds and thus more interception due to more horizontal traj
# angles. However, the ustar would have likely been lower and different z0,d0
# params which would probs counteract this effect.

#Define the range of
# heights
heights <- ft_wp_pars$z_0m+ft_wp_pars$d_0:pwl_e_mean_ht

# Use the integrate function to calculate the integral of the wind speed function over the height range
mean_wind_speed <- function(h1, h2, wind_speed_function) {
  integral <- integrate(wind_speed_function, lower = h1, upper = h2)$value
  mean_speed <- integral / (h2 - h1)
  return(mean_speed)
}

ft_mean_speed <- mean_wind_speed(min(heights), max(heights), wind_speed_function)
ft_mean_speed

# what height wind gets us 30 / 31 deg obs from the IP/correlation
calculate_zHeight <- function(FittedWspeed, ustar, d_0, z_0m) {
  zHeight <- exp((FittedWspeed * 0.4 / ustar)) * z_0m + d_0
  return(zHeight)
}

vox_config_id <- "23_072_vox_len_0.25m_sa_gridgen_v2.0.0_sa"
pwl_hemi_stat <-
  readRDS(
    paste0(
      '../../analysis/lidar-processing/data/hemi_stats/hemi_raw_theta_phi_for_rho_s_upper_2_5th_percentile_',
      vox_config_id,
      "_",
      'PWL_E',
      '.rds'
    ))

ft_hemi_stat <-
  readRDS(
    paste0(
      '../../analysis/lidar-processing/data/hemi_stats/hemi_raw_theta_phi_for_rho_s_upper_2_5th_percentile_',
      vox_config_id,
      "_",
      'FSR_S',
      '.rds'
    ))

pwl_phi_to <- max(pwl_hemi_stat$phi_d)
ft_phi_to <- max(ft_hemi_stat$phi_d)

# get avg hydrometeor velocity
select_event <- '2023-03-14'
ffr_met_wnd_lidar_events <- lidar_events_long_dt |>
  left_join(ffr_met_wnd)  |>
  left_join(parsivel, by = 'datetime') |>
  left_join(pwl_sf) |>
  filter(event_id == select_event,
         ppt > 0)

mean_vel <- mean(ffr_met_wnd_lidar_events$part_vel, na.rm = T)

## PWL ----

pwl_select_wind <- wind_speed(pwl_phi_to, mean_vel)

# this is the height above the snowpack which is about 1.5 m for this event
pwl_wind_height_of_event <- calculate_zHeight(pwl_select_wind,event_avg_ustar, ft_wp_pars$d_0, ft_wp_pars$z_0m)

## FT ----

ft_select_wind <- wind_speed(ft_phi_to, mean_vel)

ft_wind_height_of_event <- calculate_zHeight(ft_select_wind,event_avg_ustar, ft_wp_pars$d_0, ft_wp_pars$z_0m)

# we could also calculate the percent area of total canopy area at each height
# then calculate a weighed average wind speed. I expect that since most of the canopy
# elements are near 1.5 m above the ground we may get close using this weighed
# average. This is about 15% of the height of the average canopy elements.

wind_vars <- data.frame(
  plot = c('FT', 'PWL'),
  wind_speed = c(ft_select_wind, pwl_select_wind),
  traj_angle = c(pwl_phi_to, ft_phi_to),
  height_of_wind = c(ft_wind_height_of_event, pwl_wind_height_of_event)
)
