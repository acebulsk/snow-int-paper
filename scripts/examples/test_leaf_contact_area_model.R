# Script to test sensitivity of logistic equation
source('scripts/manuscript_values.R')

# define some models to represent the increase in leaf contact area with trajectory angle ----
sigmoidal <- function(x, Cc, scal) {
  (1 - Cc) / (1 + exp(-scal * (sin(x) - (1 - Cc))))
}

logistic_origin <- function(x, Asym, xmid, scal) {
  # function that fits observed data almost perfectly
  Asym / (1 + exp((xmid - x) / scal)) - Asym / (1 + exp(xmid / scal))
}

rl65_svf <- function(Surrounding_Ht, Gap_diameter, x, Cc){
  # gap_diameter: characteristic gap width (same units as height)
  # surrounding_height: height of the canopy (same units as gap_diameter)
  # x: incident angle (in radians) measured from vertical
  # The idea is to adjust the height by cos(x) to reflect the snow trajectory.

  effective_height <- Surrounding_Ht / cos(x)
  phi <- atan(Gap_diameter / (2 * effective_height))
  V_gap <- sin(phi)^2
  # The canopy contact fraction is the complement of the gap factor
  canopy_contact_fraction <- 1 - V_gap
  Cc_inc = canopy_contact_fraction - Cc
  return(Cc_inc)
}

# Function 1: Adjust canopy coverage based on incident angle
calc_effective_coverage <- function(C, theta) {
  # C: canopy coverage fraction at nadir (0 <= C <= 1)
  # theta: incident angle (in radians) measured from vertical
  # When theta = 0, cos(theta)=1 so C_eff = C; for more horizontal angles, effective coverage increases.

  # Ensure cos(theta) is positive (i.e. theta is less than 90 degrees)
  if (any(cos(theta) <= 0)) {
    stop("Theta must be less than pi/2 (90 degrees) to ensure cos(theta) > 0.")
  }

  C_eff <- 1 - (1 - C)^(1 / cos(theta))
  C_inc <- C_eff - C
  return(C_inc)
}

# Function 1: Adjust canopy coverage based on incident angle and scaled relative to tree height
calc_effective_coverage_tree_inc <- function(C, theta, tree_height, k) {
  # C: canopy coverage fraction at nadir (0 <= C <= 1)
  # theta: incident angle (in radians) measured from vertical
  # When theta = 0, cos(theta)=1 so C_eff = C; for more horizontal angles, effective coverage increases.

  # Ensure cos(theta) is positive (i.e. theta is less than 90 degrees)
  if (any(cos(theta) <= 0)) {
    stop("Theta must be less than pi/2 (90 degrees) to ensure cos(theta) > 0.")
  }

  # Calculate the scaling factor based on tree height
  scale <- tree_height * k

  # Adjust the exponent: here we multiply 1/cos(theta) by the scale factor
  exponent <- (1 / cos(theta))^scale

  C_eff <- 1 - (1 - C)^exponent
  ## C_eff <- 1-exp(-k*(LAI/cos(theta))) # beers law for reference
  C_eff <- 1 - k*(1 - C)/cos(theta)


  C_inc <- C_eff - C
  return(C_inc)
}

# returns the contact length relative to the base length
# couldnt get the right shape with this one likely because it assumes the canopy is opaque
# cp_inc_rel_contact_length_vs_base <- function(t_height, t_width, traj_angle, cc){
#   tree_angle <- atan(t_height/(t_width/2))
#   x <- tree_angle - traj_angle
#   contact_frac_hyp <- cos(x)
#   hyp <- sqrt(t_height^2+(t_width/2)^2)
#   contact_length_right <-  contact_frac_hyp * hyp
#
#   contact_length_left <- (cos(traj_angle)*t_width) - contact_length_right
#   if(contact_length_left > 0){
#     contact_length <- contact_length_left + contact_length_right
#   } else {
#     contact_length = contact_length_right
#   }
#
#   relative_open_width <- (t_width/cc)-t_width # rearranged cc = t_width/(t_width+open_width)
#
#   cp <- contact_length / (t_width+relative_open_width)
#
#   cp <- cp*1-exp(-k*(LAI/cos(theta)))
#
#   cp <- max(c(cp, cc))
#   cp <- min(c(cp, 1))
#   cp_inc = cp-cc
#   return(cp_inc)
# }


# generate some test data to apply models on ----

# forest tower using NLS
df_base_ft <- data.frame(
  traj_angles = seq(0, 90, 0.25),
  radians = seq(0, 90, 0.25)* pi / 180,
  sin_rad = sin(seq(0, 90, 0.25)* pi / 180),
  C_c = 0.3
)

df_base_ft$c_inc <- logistic_origin(x = df_base_ft$traj_angles,
                              Asym = ft_nls_coefs['Asym'],
                              xmid = ft_nls_coefs['xmid'],
                              scal = ft_nls_coefs['scal'])  |> as.numeric()
df_base_ft$group <- 'observed'
df_base_ft <- df_base_ft |>
  mutate(c_inc = ifelse(c_inc > 0.7, 0.7, c_inc))
# PWL using NLS
df_base_pwl <- data.frame(
  traj_angles = seq(0, 90, 0.25),
  radians = seq(0, 90, 0.25)* pi / 180,
  sin_rad = sin(seq(0, 90, 0.25)* pi / 180),
  C_c  = 0.5
)

df_base_pwl$c_inc <- logistic_origin(x = df_base_pwl$traj_angles,
                                    Asym = pwl_nls_coefs['Asym'],
                                    xmid = pwl_nls_coefs['xmid'],
                                    scal = pwl_nls_coefs['scal'])  |> as.numeric()
df_base_pwl$group <- 'observed'

# sigmoid equation adapted to just take Cc as a parameter has the right idea but
# it is a bit too steep off the bat and couldne get workign by adjusting the
# scal param, removing for now in favor of the more cosine based function
# angles <- seq(0, 90, 0.25)
# C_c_list <- rep(seq(0, 1, 0.1), each = length(angles))
# df_sig <- data.frame(
#   traj_angles = rep(angles, length.out = length(C_c_list)),
#   radians = rep(angles * pi / 180, length.out = length(C_c_list)),
#   sin_rad = sin(angles*pi/180),
#   C_c = C_c_list
# )
# df_sig$c_inc <- sigmoidal(x = df_sig$radians, scal = 20, Cc = df_sig$C_c)  |> as.numeric()
# df_sig$group <- 'sigmoid'

# based on some of the solar functions in CRHM doesnt work
# df_rl65 <- data.frame(
#   traj_angles = rep(angles, length.out = length(C_c_list)),
#   radians = rep(angles * pi / 180, length.out = length(C_c_list)),
#   sin_rad = sin(angles*pi/180),
#   C_c = C_c_list
# )
# df_rl65$c_inc <- rl65_svf(2, 10, x = df_rl65$radians, df_rl65$C_c)  |> as.numeric()
# df_rl65$group <- 'rl65'

# best function so far based on Cc and tree height
site_df <- data.frame(C_c = c(0.3, 0.5),
                      tree_height = c(7.1, 10.5),
                      k = c(0.24, 0.24)) # mean height measured by lidar at fortress PWL and FT
angles <- seq(0, 90, 1)
df_new <- expand_grid(site_df, angles) |>
  mutate(
    radians = angles * pi / 180,
    sin_rad = sin(angles*pi/180),
  ) |>
  select(
    traj_angles = angles,
    sin_rad,
    radians,
    C_c,
    tree_height = tree_height,
    k
  )

df_new$c_inc <- calc_effective_coverage_tree_inc(C = df_new$C_c,
                                            theta = df_new$radians,
                                            tree_height = df_new$tree_height,
                                            k = df_new$k)  |> as.numeric()
df_new$group <- 'simulated'


df <- rbind(df_base_ft, df_base_pwl) |>
  # rbind(df_new2|> select(-c(tree_height, tree_width))) |>
  rbind(df_new |> select(-c(tree_height, k)))

# df |>
#   ggplot(aes(traj_angles, c_inc, colour = factor(C_c))) +
#   geom_point() +
#   facet_grid(~group, scales = 'free_x')

err_stats <- df |>
  pivot_wider(names_from = group,
              values_from = c_inc) |>
  mutate(diff = observed - simulated) |>
  # group_by(C_c) |>
  summarise(
    `Mean Bias` = mean(diff, na.rm = T),
    MAE = mean(abs(diff), na.rm = T),
    `RMS Error` = sqrt(mean(diff ^ 2, na.rm = T)),
    # NRMSE = `RMS Error` / (max(observed, na.rm = TRUE) - min(observed, na.rm = TRUE)),
    NRMSE = `RMS Error` / mean(observed, na.rm = T),
    R = cor(observed, simulated),
    `r^2` = R^2,
    R2_cd = 1 - sum(diff^2, na.rm = T) / sum((observed - mean(observed, na.rm = T))^2, na.rm = T) # better for non linear/ without intercepts
    ) |>
  mutate(across(`Mean Bias`:`r^2`, round, digits = 3))
err_stats
df |>
  filter(traj_angles < 75) |>
  # mutate(new_group = ifelse(grepl('base', group), 'observed', group)) |> # not actually observed but close enoough
  ggplot(aes(traj_angles, c_inc, colour = factor(C_c), linetype = group)) +
  geom_line() +
  labs(
    y = 'Increase in Leaf Contact Area (-)',
    x = 'Simulated Trajectory Angle (°)',
    colour = 'Canopy\nCoverage (-)',
    linetype = 'Data Type'
  )

ggsave('figs/external_figures/example_simulated_cosinepow_vs_observed_increase_in_Cp_with_traj_angle.png',
       width = 5,
       height = 4)

# plot new model sensitivity
cc_seq <- seq(0, 1, 0.2)
tree_height_seq <- seq(5, 30, 5)
angles <- seq(0, 90, 1)
df_new_sens <- expand_grid(cc_seq, tree_height_seq, angles) |>
  mutate(
    radians = angles * pi / 180,
  ) |>
  select(
    traj_angles = angles,
    radians,
    C_c = cc_seq,
    tree_height = tree_height_seq
  )
df_new_sens$c_inc <- calc_effective_coverage_tree_inc(C = df_new_sens$C_c,
                                                      theta = df_new_sens$radians,
                                                      tree_height = df_new_sens$tree_height,
                                                      k = 0.25)  |> as.numeric()

df_new_sens |>
  # mutate(new_group = ifelse(grepl('base', group), 'observed', group)) |> # not actually observed but close enoough
  ggplot(aes(traj_angles, c_inc, colour = factor(tree_height))) +
  geom_line() +
  labs(
    y = 'Increase in Leaf Contact Area (-)',
    x = 'Simulated Trajectory Angle (°)',
    colour = 'Tree\nHeight (m)',
    linetype = 'Data Type'
  ) + facet_wrap(~C_c)

ggsave('figs/external_figures/example_simulated_cosinepow_increase_in_Cp_with_traj_angle_withtree_sensitivity.png',
       width = 6,
       height = 4)

# based on sensitivity analysis below found k = 0.25 to be best fit for rel tree height at both sites
site_df <- data.frame(C_c = c(0.3, 0.5),
                      tree_height = c(7.1, 10.5))
angles <- seq(0, 90, 1)
k_seq <- seq(0.2, 0.3, 0.01)
df_new_long <- expand_grid(site_df, angles, k_seq) |>
  mutate(
    radians = angles * pi / 180,
  ) |>
  select(
    traj_angles = angles,
    radians,
    C_c,
    tree_height = tree_height,
    k = k_seq
  )
df_new_long$c_inc <- calc_effective_coverage_tree_inc(C = df_new_long$C_c,
                                                 theta = df_new_long$radians,
                                                 tree_height = df_new_long$tree_height,
                                                 k = df_new_long$k)  |> as.numeric()

obs_long <- rbind(df_base_ft, df_base_pwl) |>
  select(traj_angles, C_c, c_inc)

error_df <- df_new_long |>
  left_join(obs_long, by = c('traj_angles', 'C_c'), suffix = c('_mod', '_obs'))

err_df_tree_cc <- error_df |>
  mutate(diff = c_inc_obs - c_inc_mod) |>
  group_by(C_c, tree_height, k) |>
  summarise(
    `Mean Bias` = mean(diff, na.rm = T),
    MAE = mean(abs(diff), na.rm = T),
    `RMS Error` = sqrt(mean(diff ^ 2, na.rm = T)),
    # NRMSE = `RMS Error` / (max(observed, na.rm = TRUE) - min(observed, na.rm = TRUE)),
    NRMSE = `RMS Error` / mean(c_inc_obs, na.rm = T),
    R = cor(c_inc_obs, c_inc_mod),
    `r^2` = R^2) #|>
  #mutate(across(`Mean Bias`:`r^2`, round, digits = 3))
select <- err_df_tree_cc |>
  group_by(C_c, tree_height) |>
  arrange(`RMS Error`) |>
  slice(1)

print(paste('The best k value for Forest Tower is:', select$k[select$C_c == 0.3]))
print(paste('Based on the lowest RMSE val of:', select$`RMS Error`[select$C_c == 0.3]))
print(paste('The best k value for PWL is:', select$k[select$C_c == 0.5]))
print(paste('Based on the lowest RMSE val of:', select$`RMS Error`[select$C_c == 0.5]))

err_df_tree <- error_df |>
  mutate(diff = c_inc_obs - c_inc_mod) |>
  group_by(k) |>
  summarise(
    `Mean Bias` = mean(diff, na.rm = T),
    MAE = mean(abs(diff), na.rm = T),
    `RMS Error` = sqrt(mean(diff ^ 2, na.rm = T)),
    # NRMSE = `RMS Error` / (max(observed, na.rm = TRUE) - min(observed, na.rm = TRUE)),
    NRMSE = `RMS Error` / mean(c_inc_obs, na.rm = T),
    R = cor(c_inc_obs, c_inc_mod),
    `r^2` = R^2) |>
  mutate(across(`Mean Bias`:`r^2`, round, digits = 3))

err_df_tree

select <- err_df_tree |>
  # group_by(C_c, tree_height) |>
  arrange(`RMS Error`) |>
  slice(1)

print(paste('The best k value fit to both plots is:', select$k))
print(paste('Based on the lowest RMSE val of:', select$`RMS Error`))

# powerline
# df_base <- data.frame(
#   traj_angles = seq(0, 90, 0.25),
#   radians = seq(0, 90, 0.25)* pi / 180
#   )
#
# df_base$c_inc <- logistic_origin(x = traj_angles,
#                                  Asym = pwl_nls_coefs['Asym'],
#                                  xmid = pwl_nls_coefs['xmid'],
#                                  scal = pwl_nls_coefs['scal'])  |> as.numeric()
# df_base$group <- 'base'
#
# df_test <- data.frame(
#   traj_angles = seq(0, 90, 0.25))
#
# df_test$c_inc <- logistic_origin(x = traj_angles,
#                                  Asym = 1-0.5,
#                                  # Asym = ft_nls_coefs['Asym'],
#                                  xmid = 45,
#                                  # xmid = ft_nls_coefs['xmid'],
#                                  scal = 20
#                                  # scal = ft_nls_coefs['scal']
# )  |> as.numeric()
# df_test$group <- 'test'
#
# df <- rbind(df_test, df_base)
#
# ggplot(df, aes(traj_angles, c_inc, colour = group)) + geom_point()
#
# # very sparse
# df_base <- data.frame(
#   traj_angles = seq(0, 90, 0.25))
#
# df_base$c_inc <- logistic_origin(x = traj_angles,
#                                  Asym = pwl_nls_coefs['Asym'],
#                                  xmid = pwl_nls_coefs['xmid'],
#                                  scal = pwl_nls_coefs['scal'])  |> as.numeric()
# df_base$group <- 'base'
#
# df_test <- data.frame(
#   traj_angles = seq(0, 90, 0.25))
#
# df_test$c_inc <- logistic_origin(x = traj_angles,
#                                  Asym = 1-0.1,
#                                  # Asym = ft_nls_coefs['Asym'],
#                                  xmid = 80,
#                                  # xmid = ft_nls_coefs['xmid'],
#                                  scal = 15
#                                  # scal = ft_nls_coefs['scal']
# )  |> as.numeric()
# df_test$group <- 'test'
#
# df <- rbind(df_test, df_base)
#
# ggplot(df, aes(traj_angles, c_inc, colour = group)) + geom_point()
#
# # experimental
#
# logistic_origin <- function(canopy_closure,
#                             trajectory_angle,
#                             L = 1,            # Maximum value for logistic function
#                             k = .1,            # Growth rate or steepness
#                             x_0 = .5,        # Midpoint of logistic curve
#                             alpha = 10) {    # Scaling factor for canopy_closure effect
#   leaf_plane_area <- L-canopy_closure / (1 + exp(-k * (cos(trajectory_angle * pi / 180) - x_0)))
#
# }
#
# df_test <- data.frame(
#   traj_angles = seq(0, 90, 0.25))
#
# df_test$c_p <- logistic_origin(
#   canopy_closure = 0.5,
#   trajectory_angle = df_test$traj_angles
# )  |> as.numeric()
#
# ggplot(df_test, aes(traj_angles, c_p)) + geom_point()
