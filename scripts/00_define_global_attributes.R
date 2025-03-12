# define global attributes

library(tidyverse)
library(ggpubr)
library(cowplot)
library(plotly)
library(abind)
library(terra)
library(modelr)
library(gt)

# source('../../analysis/lidar-processing/scripts/voxrs/voxrs_helper_fns.R')
source('../../analysis/disdrometer/scripts/00_source_functions.R')
perc_err_fltr <- 100 # percent error above which is unnacceptible to apply after accumulation period

scl_names_dict <- data.frame(
  name = c('sparse_forest', 'medium_density_forest', 'dense_forest'),
  scl_names_new = factor(c('Sparse', 'Mixed', 'Closed'), levels = c(c('Sparse', 'Mixed', 'Closed')))
)

calg_mag_declination <- 13.5 # deg + east in 2020 https://www.ngdc.noaa.gov/geomag/magfield-wist/

# theme_bw(base_size = 14)
options(ggplot2.discrete.colour= palette.colors(palette = "R4"))
options(ggplot2.discrete.fill= palette.colors(palette = "R4")[2:6])
cc_colours <- c("#9E9E9E",  "#F5C710", "#61D04F")

# [1] "#000000" "#DF536B" "#61D04F" "#2297E6" "#28E2E5" "#CD0BBC" "#F5C710"
# [8] "#9E9E9E"

fig_width <- 5.5
fig_height <- 4

wnd_bin_ax_lab <- "Wind Speed (m/s)"
temp_bin_ax_lab <- 'Air Temperature (°C)'
ip_y_ax_lab <- 'Interception Efficiency (-)'
temp_ax_lab <- 'Air Temperature (°C)'
wind_ax_lab <- 'Wind Speed (m/s)'
diam_ax_lab <- 'Hydrometeor Diameter (mm)'
vel_ax_lab <- 'Hydrometeor Velocity (m/s)'
le_ax_lab <- 'Leaf Area Index (-)'
cc_ax_lab <- 'Canopy Coverage (-)'
w_tree_ax_lab <- "Initial Canopy Snow Load (mm)"

label_bin_fn <- function(bins){
  (bins[-1] + bins[-length(bins)]) / 2
}

give.n <- function(x){
  return(c(y = median(x)*1.05, label = length(x)))
  # experiment with the multiplier to find the perfect position
}

to_long <- function(from, to, event_id){
  datetime <- seq(from, to, 900)

  out <- data.frame(datetime, event_id)

  return(out)
}

to_long_one_minute <- function(from, to, event_id){
  datetime <- seq(from, to, 60)

  out <- data.frame(datetime, event_id)

  return(out)
}

get_traj_time <- function(file, fin = T){
  origin <- as.POSIXct('1980-01-06 00:00:00', tz = 'UTC')
  traj <- read.csv(file)

  if(fin){ # i.e. grab tail
    traj_time <- tail(traj$Time.s., n = 1)+1e9
  } else { # grab head
    traj_time <- head(traj$Time.s., n = 1)+1e9
  }

  time_out <- as.POSIXct(traj_time, origin = origin, tz = 'UTC')
  time_out <- format(time_out, tz = 'Etc/GMT+6') |> as.POSIXct(tz = 'Etc/GMT+6')

  return(time_out)
}


# based on the idea that the increase in snow-leaf contact area is equal to the
# void space multiplied by the increase in leaf contact area with snowfall
# trajectory angle represented by sin(theta)
# see /home/alex/local-usask/working-papers/snow-int-paper/figs/examples/contact_area_reasoning.qmd for reasoning behind this model
sine_fn <- function(traj_angle, b,  cc){
  # traj_angle is the inclination angle from zenith of snowfall (degrees)
  # cc is the canopy coverage viewed from nadir (-)
  theta <- traj_angle * (pi/180)

  # 1-cc sets the max potential increase based on the initial void space
  # sin(theta)^2 is the increase in surface area snow can contact
  cp_inc <- b*((1-cc)*sin(theta)^2)#*1/(cos(theta))
  return(cp_inc)
}

traj_angle_deg <- function(wind_speed, velocity){
  slope <- wind_speed/velocity
  angle_deg <- atan(slope) * 180 / pi

  return(angle_deg)
}

# rearrange to return wind speed given angle and velocity
wind_speed <- function(traj_angle_deg, velocity){
  angle_rad <- traj_angle_deg * pi / 180
  wind_speed <- velocity * tan(angle_rad)

  return(wind_speed)
}

prop_err_ratio <- function(ratio, x, y, sigma_x, sigma_y){
  sigma_ratio <- ratio*sqrt((sigma_x/x)^2+(sigma_y/y)^2)
  return(sigma_ratio)
}

prop_err_sum <- function(sigma_x, sigma_y){
  sigma_sum <- sqrt(sigma_x^2 + sigma_y^2)
  return(sigma_sum)
}

# Function to perform ANOVA
anova_fn <- function(data, x_var) {
  # Perform ANOVA
  anova_model <- aov(as.formula(paste0('IP ~ ', x_var)), data = data)
  anova_results <- tidy(anova_model) %>%
    mutate(analysis_type = "ANOVA")

  # Combine ANOVA and post-hoc results
  return(anova_results)
}

# Function to perform ANOVA post hoc analysis
anova_posthoc_fn <- function(data, x_var, alternative_hypothesis) {
  # Ensure x_var is a factor and has correct levels
  data[[x_var]] <- factor(data[[x_var]], levels = unique(data[[x_var]]))

  # Perform ANOVA
  anova_model <- aov(as.formula(paste0('IP ~ ', x_var)), data = data)

  anova_results <- tidy(anova_model) %>%
    mutate(analysis_type = "ANOVA")

  # Perform GLHT for adjacent bins
  levels <- levels(data[[x_var]])
  n_levels <- length(levels)

  # Create contrast matrix for adjacent comparisons
  if (n_levels < 2) {
    return(anova_results)  # Return only ANOVA results if less than 2 levels
  }

  contrasts <- matrix(0, nrow = n_levels - 1, ncol = n_levels)
  for (i in 1:(n_levels - 1)) {
    contrasts[i, i] <- -1
    contrasts[i, i + 1] <- 1
  }
  rownames(contrasts) <- paste(levels[-n_levels], "-", levels[-1])

  # Perform GLHT
  contrast_list <- list(contrasts)
  names(contrast_list) <- x_var

  glht_result <- multcomp::glht(anova_model,
                                # linfct = mcp(temp_binned = contrasts), # typically its this but we make some adjustements to work as a function
                                linfct = do.call(multcomp::mcp, contrast_list),
                                alternative = alternative_hypothesis)

  # Extract and tidy GLHT results
  posthoc <- tidy(summary(glht_result)) %>%
    mutate(
      analysis_type = "Post-hoc",
      contrast = rownames(contrasts)
    )

  # Combine ANOVA and post-hoc results
  bind_rows(anova_results, posthoc)
}

# Function to generate summary statement
generate_summary <- function(dataset_results) {
  anova_result <- dataset_results %>% filter(analysis_type == "ANOVA")
  posthoc_results <- dataset_results %>% filter(analysis_type == "Post-hoc")

  anova_significant <- anova_result$p.value[1] < 0.05

  significant_bins <- posthoc_results %>%
    filter(adj.p.value < 0.05, estimate > 0) %>%
    pull(contrast)

  summary <- paste0(
    "ANOVA: ", ifelse(anova_significant, "Significant", "Not significant"), "\n",
    "Significantly greater bins: ",
    ifelse(length(significant_bins) > 0,
           paste(significant_bins, collapse = ", "),
           "None"),
    "\n\n"
  )

  return(summary)
}

# Function to compute Wilcoxon test and extract relevant statistics
compute_wilcoxon <- function(data, group_var, group_low, group_high, test_type, alternative) {
  data %>%
    mutate(group = ifelse({{ group_var }} < get(paste0(group_var, "_th")), group_low, group_high)) %>%
    group_by(trough_name) %>%
    summarise(
      w_test = list(wilcox.test(IP[group == group_low], IP[group == group_high],
                                alternative = alternative) %>% tidy()),
      median_low = median(IP[group == group_low], na.rm = TRUE),
      median_high = median(IP[group == group_high], na.rm = TRUE),
      n_samples = n()
    ) %>%
    unnest(cols = c(w_test)) %>%
    mutate(
      reject_null_hyp = ifelse(p.value < 0.05, TRUE, FALSE),
      test_type = test_type,
      null_hypothesis = case_when(
        test_type == "Air Temperature" ~ paste0("Median IP (cold) ≥ Median IP (warm)"),
        test_type == "Wind Speed" ~ paste0("Median IP (calm) ≥ Median IP (windy)"),
        test_type == "Snow Load" ~ paste0("Median IP (low snow load) ≤ Median IP (high snow load)")
      )
    ) %>%
    select(test_type, trough_name, null_hypothesis, p.value, n_samples, median_low, median_high, reject_null_hyp)
}
