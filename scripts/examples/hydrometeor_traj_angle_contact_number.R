library(tidyverse)

mcn_lab <- 'Mean Contact Number (-)'
ws_lab <- 'Wind Speed (m/s)'
traj_lab <- 'Hydrometeor Trajectory Angle (°)'
ip_lab <- 'Interception Efficiency (-)'

# returns I/P given calibrated parameters
mcn_to_ip <- function(mcn, j, k){
  ip <- k * log(mcn) + j
  return(ip)
}

traj_angle_to_mcn <- function(traj_angle, a, b){
  zenith_angle <- traj_angle + 90
  mcn <- a * exp(b*zenith_angle)
  return(mcn)
}

# returns angle in degrees from zenith
traj_angle_deg <- function(wind_speed, velocity){
  slope <- wind_speed/velocity
  angle_deg <- atan(slope) * 180 / pi

  return(angle_deg)
}

wind_speed <- seq(0,20, 0.1)
hm_velocity <- seq(0.5, 1.5, by = 0.5)
# coefs below are from lidar-processing/voxrs/scripts/03
a <- 1.01824 # cal across all plots
b <- 0.03984 # cal across all plots
k <- 0.04096 # cal on PWL SW
j <- 0.36786 # cal on PWL SW

# Plot wind vs trajectory angle

ta_u_hm_vel_df <- expand_grid(wind_speed, hm_velocity)

ta_u_hm_vel_df$traj_angle_deg_vect <-
  traj_angle_deg(ta_u_hm_vel_df$wind_speed, ta_u_hm_vel_df$hm_velocity)

ggplot(
  ta_u_hm_vel_df |>
    mutate(hm_velocity = as.character(hm_velocity)),
  aes(
    wind_speed,
    traj_angle_deg_vect,
    colour = hm_velocity,
    group = hm_velocity
  )
) +
  geom_line() +
  ylab(traj_lab) +
  xlab('Horizontal Velocity (m/s)') +
  ylim(c(0, 90)) +
  scale_color_viridis_d(name = 'Fall Velocity (m/s)') +
  theme(legend.position = 'bottom')

ggsave('figs/examples/wind_speed_vs_traj_angle.png', device = png,
      width = 4, height = 4)

# Plot trajectory angle vs mean contact number

mcn_df <- data.frame(
  wind_speed,
  traj_angle_deg_vect,
  mcn = traj_angle_to_mcn(traj_angle_deg_vect, a, b)
)

ggplot(mcn_df,
       aes(traj_angle_deg_vect, mcn)) +
  geom_line() +
  ylab(mcn_lab) +
  xlab(traj_lab) +
  xlim(c(-90, 0))

ggsave(paste0('figs/examples/traj_angle_vs_contact_number.png'), device = png,
       width = 6, height = 5)

ggplot(mcn_df,
       aes(wind_speed, mcn)) +
  geom_line() +
  ylab(mcn_lab) +
  xlab(ws_lab)

ggsave(paste0('figs/examples/wind_speed_vs_contact_number.png'), device = png,
       width = 6, height = 5)

# Plot I/P vs wind

ip_df <- mcn_df |>
  mutate(`I/P` = mcn_to_ip(mcn, j, k))

ggplot(ip_df, aes(mcn, `I/P`)) +
  geom_line()  +
  ylab(ip_lab) +
  xlab(mcn_lab)

ggsave(paste0('figs/examples/contact_number_vs_ip.png'), device = png,
       width = 6, height = 5)

ggplot(ip_df, aes(wind_speed, `I/P`)) +
  geom_line()+
  ylab(ip_lab) +
  xlab(ws_lab)

ggsave(paste0('figs/examples/wind_speed_vs_ip.png'), device = png,
       width = 6, height = 5)
