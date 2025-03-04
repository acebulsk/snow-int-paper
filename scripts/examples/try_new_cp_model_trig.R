# tried some trig to derrive a increase in leaf contact area function but does
# not seem to work

cc <- .5
traj_angle <- 45
traj_angle <- traj_angle*(pi/180)
t_height <- 20
t_width <- 4

# returns the contact length relative to the base length
contact_length <- function(t_height, t_width, traj_angle){
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
  return(contact_length)
}

cl <- contact_length(t_height, t_width, traj_angle)
cl

ol <- (t_width/cc)-t_width
ol

cp <- cl/(t_width+ol)
cp

# returns the contact length relative to the base length
cp_inc_rel_contact_length_vs_base <- function(t_height, t_width, traj_angle, cc){
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
  cp <- max(c(cp, cc))
  cp <- min(c(cp, 1))
  cp_inc = cp-cc
  return(cp_inc)
}

cp_inc_rel_contact_length_vs_base(t_height, t_width, traj_angle, cc)
