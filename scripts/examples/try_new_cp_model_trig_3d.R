# Function to calculate cp increment for conical trees in 3D
cp_inc_rel_contact_3d <- function(t_height, t_width, traj_angle_deg, cc, debug=FALSE) {
  # Convert trajectory angle to radians
  traj_angle <- traj_angle_deg * (pi/180)

  # Calculate cone properties
  radius <- t_width/2
  cone_angle <- atan(t_height/radius)
  slant_height <- sqrt(t_height^2 + radius^2)

  # Calculate projected area of cone (vertical projection)
  base_area <- pi * radius^2

  # Calculate effective surface area based on trajectory angle
  # For vertical snow (traj_angle = 0), effective area is just the horizontal projection
  if(traj_angle_deg == 0) {
    effective_area <- base_area
  } else {
    # Calculate exposed surface area considering angle
    # This accounts for the fact that more of the cone surface is exposed at angles
    exposed_area <- pi * radius * slant_height
    effective_area <- exposed_area * abs(cos(traj_angle))
  }

  # Calculate contact proportion
  # Normalize by the area including gaps (base_area/cc)
  cp <- effective_area / (base_area/cc)
  cp <- max(c(cp, cc))
  cp_inc <- cp - cc

  if(debug) {
    cat("Debug Information:\n")
    cat("Cone angle (degrees):", cone_angle * 180/pi, "\n")
    cat("Base area:", base_area, "\n")
    cat("Effective area:", effective_area, "\n")
    cat("CP:", cp, "\n")
    cat("CP increment:", cp_inc, "\n")
  }

  return(list(
    angle = traj_angle_deg,
    base_area = base_area,
    effective_area = effective_area,
    cp = cp,
    cp_inc = cp_inc
  ))
}

# Create visualization
library(ggplot2)
library(dplyr)

# Generate comparison data for 2D and 3D models
t_height <- 10
t_width <- 5
cc <- 0.25

# Calculate values for angles from 0 to 90 degrees
angles <- seq(0, 90, by=1)

# 2D calculations (from previous version)
results_2d <- lapply(angles, function(angle) {
  cp_inc_rel_contact_length(t_height, t_width, angle, cc)
})

# 3D calculations
results_3d <- lapply(angles, function(angle) {
  cp_inc_rel_contact_3d(t_height, t_width, angle, cc)
})

# Convert results to data frames
plot_data_2d <- do.call(rbind, lapply(results_2d, function(x) {
  data.frame(
    angle = x$angle,
    cp_inc = x$cp_inc,
    model = "2D Rectangle"
  )
}))

plot_data_3d <- do.call(rbind, lapply(results_3d, function(x) {
  data.frame(
    angle = x$angle,
    cp_inc = x$cp_inc,
    model = "3D Cone"
  )
}))

# Combine data
plot_data <- rbind(plot_data_2d, plot_data_3d)

# Create comparison plot
comparison_plot <- ggplot(plot_data) +
  geom_line(aes(x=angle, y=cp_inc, color=model), size=1) +
  labs(title="CP Increment vs Trajectory Angle: 2D vs 3D Comparison",
       x="Trajectory Angle (degrees)",
       y="CP Increment",
       color="Model") +
  theme_minimal() +
  theme(legend.position="bottom")

# Print plot
print(comparison_plot)

# Print some key comparison values
key_angles <- c(0, 10, 45, 80)
for(angle in key_angles) {
  cat("\nAngle:", angle, "degrees\n")
  result_2d <- cp_inc_rel_contact_length(t_height, t_width, angle, cc)
  result_3d <- cp_inc_rel_contact_3d(t_height, t_width, angle, cc, debug=TRUE)
  cat("2D CP increment:", round(result_2d$cp_inc, 3), "\n")
  cat("3D CP increment:", round(result_3d$cp_inc, 3), "\n")
}
