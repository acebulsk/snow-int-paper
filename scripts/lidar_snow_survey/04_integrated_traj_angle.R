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

mod_df <- data.frame(avg_height = seq(ft_wp_pars$d_0+ft_wp_pars$z_0m, pwl_e_mean_ht, 0.1))

mod_df$avg_wind <- trbtransfeR::fit_neutral_wind_helpr(c(event_avg_ustar,
                                                         ft_wp_pars$z_0m,
                                                         ft_wp_pars$d_0),
                                                       mod_df$avg_height)
# plot the wind profile
ggplot(mod_df, aes(avg_wind, avg_height)) +
  geom_line() +
  geom_point(aes(x = event_avg_wind, y = us_wind_height - event_sd))

# avg wind over the profile over the event

mod_df$avg_wind |> mean()

# to get 30 degrees traj angle we would;ve needed 0.5 m/s wind speeds

# this is the height above the snowpack which is about 1.5 m for this event
wind_height_of_event <- mod_df$avg_height[which.min(abs(0.5-mod_df$avg_wind))]

# we could also calculate the percent area of total canopy area at each height
# then calculate a weighed average wind speed. I expect that since most of the canopy
# elements are near 1.5 m above the ground we may get close using this weighed
# average. This is about 15% of the height of the average canopy elements.


