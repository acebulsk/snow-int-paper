library(tidyverse)
# returns the contact length relative to the base length
# couldnt get the right shape with this one likely because it assumes the canopy is opaque
cp_rect <- function(t_height, t_width, traj_angle, cc){
  top_contact_width <- cos(traj_angle)*t_width

  side_contact_width <- cos(90-traj_angle)*t_height
  total_contact <- top_contact_width + side_contact_width

  cp_inc <- total_contact/top_contact_width

  tree_angle <- atan(t_height/(t_width/2))
  x <- tree_angle - traj_angle
  contact_frac_hyp <- cos(x)
  hyp <- sqrt(t_height^2+(t_width/2)^2)
  contact_length_right <-  contact_frac_hyp * hyp

  contact_length_left <- (cos(traj_angle)*t_width) - contact_length_right
  if(contact_length_left > 0){
    contact_length <- contact_length_left + contact_length_right
  } else {
    contact_length = contact_length_right
  }

  relative_open_width <- (t_width/cc)-t_width # rearranged cc = t_width/(t_width+open_width)

  cp <- contact_length / (t_width+relative_open_width)

  cp <- cp*1-exp(-k*(LAI/cos(theta)))

  cp <- max(c(cp, cc))
  cp <- min(c(cp, 1))
  cp_inc = cp-cc
  return(cp_inc)
}

cp_tri <- function(t_height, t_width, traj_angle, cc){

  half_crown_angle <- atan((t_width/2)/t_height)

  hyp <- sqrt(t_height^2+(t_width/2)^2)

  c_w_r <- sin(half_crown_angle + traj_angle)*hyp
  c_w_l <- sin(half_crown_angle - traj_angle)*hyp
  c_w_l <- max(c(0, c_w_l))
  total_contact_width <- c_w_r + c_w_l

  cp_inc_frac <- total_contact_width/t_width
  return(cp_inc_frac)

  # cp <- cp_inc_frac*cc
  # cp <- cp^1
  # cp <- if_else(cp>1, 1, cp)
  # cp <- if_else(cp<cc, cc, cp)
  # return(cp)


  # relative_open_width <- (t_width/cc)-t_width # rearranged cc = t_width/(t_width+open_width)
  #
  # cp <- total_contact_width / (t_width+relative_open_width)
  # cp <- max(c(cp, cc)) # cant be less than the original
  # cp <- min(c(cp, 1)) # cant be higher than 1
  # cp_inc = cp-cc
  # return(cp_inc)
}

t_height <- 20
t_width <- t_height/4
cc <- 0.3
LAI <- 3
plot_df$cp_inc <- cp_tri(t_height, t_width, plot_df$theta_rad, cc)

ggplot(plot_df, aes(theta, cp_inc)) + geom_line()


plot_df <- data.frame(theta = seq(0, 90, 1),
                      theta_rad = seq(0, 90, 1)*(pi/180),
                      cp = NA)
plot_df$cp_inc <- cc*exp((sin(plot_df$theta_rad)))
ggplot(plot_df, aes(theta, cp_inc)) + geom_line()



cp_cone <- function(t_height, t_width, traj_angle, cc){
  ## based on idea that the contact area is equal to the surface area of the
  ## base of the tree plus the surface area that pokes above this base surface
  ## area ignores overlap currently. Also ignores that there should be no
  ## increase initially until above critical angle where tree pokes above the
  ## surface area and starts to contribute since this is a very small effect.

  # t_height is the mean height of the canopy (m)
  # t_width is the mean width of the individual trees in the canopy (m)
  # traj_angle is the inclination angle from zenith of snowfall (radians)
  # cc is the canopy coverage viewed from nadir (-)

  tree_base_sa <- pi*(t_width/2)^2

  half_crown_angle <- atan((t_width/2)/t_height)
  hyp <- sqrt(t_height^2+(t_width/2)^2) # hypotenuse of tree if it was a triangle
  contact_length_right <- sin(half_crown_angle + traj_angle)*hyp # length of the new contact area perpendicular to the snowfall

  h0 <- sin(traj_angle)*(contact_length_right) # the height of the tree poking above the surface area from side view

  w0 <- (t_width/t_height)*h0 # width of the area of tree poking above

  area0 <- h0*w0 # area of the tree above critical angle (area of two right angle triangles is b x h)

  # Apply Beer's Law for overlap
  # Path length through canopy increases with angle: 1/cos(angle)
  path_length <- 1 / cos(traj_angle)
  LAI = exp((cc-0.55)/0.29)+1 # from CRHM

  # Calculate interception probability using Beer's Law
  p_intercept <- exp(-k * LAI * path_length)

  # Scale additional area by Beer's Law factor
  effective_additional_area <- area0 * p_intercept

  total_contact_area <- tree_base_sa + effective_additional_area

  # return(cp)

  relative_open_area <- (tree_base_sa/cc)-tree_base_sa # rearranged cc = tree_base_sa/(tree_base_sa+open_area)

  cp <- total_contact_area/(relative_open_area+tree_base_sa)
  cp <- if_else(cp>1, 1, cp)
  cp <- if_else(cp<cc, cc, cp)

  cp_inc <- cp-cc

  return(cp_inc)
}

t_height <- 20
t_width <- t_height/4
cc <- 0.5
LAI <- 3
theta_seq <- seq(0, 90)
cc_seq <- seq(0, 1, 0.25)
plot_df <- expand_grid(theta_seq, cc_seq)
plot_df$cp_inc <- cp_cone(t_height, t_width, plot_df$theta_seq*(pi/180), plot_df$cc_seq)

ggplot(plot_df, aes(theta_seq, cp_inc, colour = factor(cc_seq))) + geom_line()

#alternative to above but fails for large angles
cp_cone2 <- function(t_height, t_width, traj_angle, cc){
  ## based on idea that the contact area is equal to the surface area of the
  ## base of the tree plus the surface area that pokes above this base surface
  ## area ignores overlap currently. Also ignores that there should be no
  ## increase initially until above critical angle where tree pokes above the
  ## surface area and starts to contribute since this is a very small effect.

  # t_height is the mean height of the canopy (m)
  # t_width is the mean width of the individual trees in the canopy (m)
  # traj_angle is the inclination angle from zenith of snowfall (radians)
  # cc is the canopy coverage viewed from nadir (-)

  tree_base_sa <- pi*(t_width/2)^2

  dH <- tan(traj_angle)*t_width/2
  new_contact_length <- sin(traj_angle)*(t_height-dH)
  h0 <- sin(traj_angle)*(new_contact_length + dH)
  w0 <- (t_width/t_height)*h0 # width of the area of tree poking above

  area0 <- h0*w0 # area of the tree above critical angle (area of two right angle triangles is b x h)

  # cc_adjusted <- cc*cos(traj_angle)
  # p_no_overlap <- (1-cc_adjusted)

  # Apply Beer's Law for overlap
  # Path length through canopy increases with angle: 1/cos(angle)
  path_length <- 1 / cos(traj_angle)
  LAI = exp((cc-0.55)/0.29)+1 # from CRHM

  # Calculate interception probability using Beer's Law
  p_intercept <- exp(-k * LAI * path_length)

  # Scale additional area by Beer's Law factor
  effective_additional_area <- area0 * p_intercept

  total_contact_area <- tree_base_sa + effective_additional_area

  # return(cp)

  relative_open_area <- (tree_base_sa/cc)-tree_base_sa # rearranged cc = tree_base_sa/(tree_base_sa+open_area)

  cp <- total_contact_area/(relative_open_area+tree_base_sa)
  cp <- if_else(cp>1, 1, cp)
  cp <- if_else(cp<cc, cc, cp)

  cp_inc <- cp-cc

  return(cp_inc)
}

t_height <- 20
t_width <- t_height/4
k <- .5
theta_seq <- seq(0, 90)
cc_seq <- seq(0, 1, 0.25)
plot_df <- expand_grid(theta_seq, cc_seq)
plot_df$cp_inc <- cp_cone2(t_height, t_width, plot_df$theta_seq*(pi/180), plot_df$cc_seq)

ggplot(plot_df, aes(theta_seq, cp_inc, colour = factor(cc_seq))) + geom_line()

cp_smpl <- function(k, traj_angle, cc){
  # traj_angle is the inclination angle from zenith of snowfall (radians)
  # cc is the canopy coverage viewed from nadir (-)

  # need to also adjust based on the void space available to increase, i.e. at more horizontal angles its more likely we cannot increase the cp since snow is already blocked be a tree overlapping
  # vf0 <- (1-cc)*cos(traj_angle) # void space declines at inclined angles as more likely to be canopy overlapping
  # maybe some function of lai could be used
  # lai <- 1.5
  # vf0 <- exp(-lai/cos(traj_angle))

  t_height <- 20
  t_width <- t_height/4
  dH <- tan(traj_angle)*t_width/2
  new_contact_length <- sin(traj_angle)*(t_height-dH)
  h0 <- sin(traj_angle)*(new_contact_length + dH)
  w0 <- (t_width/t_height)*h0 # width of the area of tree poking above
  area0 <- h0*w0
  return(area0)
  #
  #
  # cp_inc <- (sin(traj_angle)^2*k)^2
  # # cp_inc <- ifelse(cp_inc > vf0, vf0, cp_inc)
  #
  # cp <- cc + (k*cc*cp_inc)
  #
  # cp_inc <- cp-cc
  #
  # return(cp_inc)
}

theta_seq <- seq(0, 90)
cc_seq <- seq(0, 1, 0.25)
# cc_seq <- c(0.3, 0.5)

plot_df <- expand_grid(theta_seq, cc_seq)

plot_df$cp_inc <- cp_smpl(k = 1, plot_df$theta_seq*(pi/180), plot_df$cc_seq)

ggplot(plot_df, aes(theta_seq, cp_inc, colour = factor(cc_seq), group = cc_seq)) +
  geom_line() +
  labs(colour = 'Canopy Cover from Nadir (-)',
       y = 'Increase in Canopy Contact from Nadir (-)',
       x = 'Snowfall Trajectory Angle (°)') #+
  xlim(c(0, 90)) +
  ylim(c(0,1))


plot_df <- data.frame(theta = seq(0, 90, 1),
                      theta_rad = seq(0, 90, 1)*(pi/180),
                      cp = NA)
plot_df$test <- 1-exp(-0.01*5*(1/cos(plot_df$theta_rad)))
plot_df$test <- cos(plot_df$theta_rad)

ggplot(plot_df |> filter(theta<89), aes(theta, test )) + geom_line() + ylab('cos(theta)')


