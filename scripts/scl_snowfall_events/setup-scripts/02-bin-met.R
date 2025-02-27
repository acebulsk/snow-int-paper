# this script compiles the precip and throughfall rates and computes average met
# conditions within each timestep as in the ablation 00_load_data.R script

hourly <- F # should we convert the 15 min raw data to hourly before processing?

good_wind <- 'u' # this is the qc and gap filled wind from FFR ultrasonic/3cup/pwlrmyoung
good_temp <- 't' # this is the mid tree FFR air temp

if(hourly == T){
  stop("need to update this to use new dfs.")
  # start to hourly
  q_tf <- q_tf_scl |>
    # to hourly
    mutate(datetime = format(datetime, '%Y-%m-%d %H:00:00')) |>
    group_by(datetime, name) |>
    summarise(q_tf = sum(d_tf, na.rm = T)) |>
    mutate(datetime = as.POSIXct(datetime),
           q_tf = q_tf)
  # end to hourly
  # Gather met data

  met <- ffr_met |>
    left_join(weighed_tree_zeroed) |>
    left_join(parsivel) |>
    # start to hourly
    mutate(datetime = format(datetime, '%Y-%m-%d %H:00:00')) |>
    group_by(datetime) |>
    summarise(
      t = mean(t, na.rm = T),
      t_ice_bulb = mean(t_ice_bulb, na.rm = T),
      u = mean(u, na.rm = T),
      p = sum(p, na.rm =T),
      weighed_tree_canopy_load_mm = mean(weighed_tree_canopy_load_mm, na.rm =T),
      part_diam = mean(part_diam, na.rm = T),
      part_vel = mean(part_vel, na.rm = T)
    ) |>
    mutate(datetime = as.POSIXct(datetime, tz = 'Etc/GMT+6')) |>
    # end to hourly
    inner_join(storms_long, by = 'datetime') |>  # limit to only periods where snow is in the canopy
    select(datetime, storm_id, t, t_ice_bulb, u, p, weighed_tree_canopy_load_mm, part_diam, part_vel, precip_name)
} else {
  # keep at 15 min
  q_tf <- q_tf_scl

  met <- throughfall_periods_long |>
    left_join(ffr_met) |> # join with the event datetimes so our bin sizes are appropriate for the events
  select(
    datetime,
    t,
    rh,
    # t_ice_bulb,
    u,
    p,
    Qsi,
    # part_diam,
    # part_vel,
    # precip_name
  )

  tree <- throughfall_periods_long |>
    left_join(w_tree_zrd) |>
    select(datetime, trough_name, weighed_tree_canopy_load_mm)
}

# met[,good_wind][met[,good_wind]==0] <- 0.001 # need to remove 0 wind speeds that breaks regression, safe to assume 0 wind no possible so assign a very low value

met$q_sf <- met$p * 4 # mm per interval to mm per hour

# Bin the met vars ----

label_bin_fn <- function(bins){
  (bins[-1] + bins[-length(bins)]) / 2
}

## bin temp ----

min_temp <- round(
  min(met[,good_temp], na.rm = T))

max_temp <- round(
  max(met[,good_temp], na.rm = T))

temp_step <- (max_temp-min_temp) / 11

temp_breaks <- seq(
  min_temp-0.5,
  max_temp,
  temp_step)

temp_breaks_labs <- temp_breaks
temp_breaks[length(temp_breaks)] <- 1.2

temp_labs_seq <- label_bin_fn(bins = temp_breaks_labs)

stopifnot(tail(temp_breaks, 1) > max(met[,good_temp], na.rm = T))
stopifnot(head(temp_breaks, 1) < min(met[,good_temp], na.rm = T))

stopifnot(length(temp_labs_seq) + 1 == length(temp_breaks))

met$temp_binned <- cut(met[,good_temp, drop = TRUE], temp_breaks)

met$temp_labs <- cut(met[,good_temp, drop = TRUE],
                     temp_breaks,
                     labels = temp_labs_seq)

## bin ice-bulb temp ----

# min_temp <- round(
#   min(met[,'t_ice_bulb'], na.rm = T))
#
# max_temp <- round(
#   max(met[,'t_ice_bulb'], na.rm = T))
#
# temp_step <- 1.5
#
# temp_breaks <- seq(
#   min_temp-0.5,
#   max_temp+1,
#   temp_step)
#
# temp_labs_seq <- label_bin_fn(bins = temp_breaks)
#
# stopifnot(tail(temp_breaks, 1) > max(met[,'t_ice_bulb'], na.rm = T))
# stopifnot(head(temp_breaks, 1) < min(met[,'t_ice_bulb'], na.rm = T))
#
# stopifnot(length(temp_labs_seq) + 1 == length(temp_breaks))
#
# met$t_ice_binned <- cut(met[,'t_ice_bulb', drop = TRUE], temp_breaks)
#
# met$t_ice_labs <- cut(met[,'t_ice_bulb', drop = TRUE],
#                       temp_breaks,
#                       labels = temp_labs_seq)

## bin ice-bulb temp depression ----

# met$t_ice_dep <- met[,good_temp] - met[,'t_ice_bulb']
# met$t_ice_dep <- ifelse(met$t_ice_dep < 0, 0, met$t_ice_dep)
#
# min_temp <- round(
#   min(met[,'t_ice_dep'], na.rm = T), digits = 1)
#
# max_temp <- round(
#   max(met[,'t_ice_dep'], na.rm = T), digits = 1)
#
# temp_step <- .2
#
# temp_breaks <- seq(
#   min_temp-0.1,
#   max_temp,
#   temp_step)
#
# temp_labs_seq <- label_bin_fn(bins = temp_breaks)
#
# stopifnot(tail(temp_breaks, 1) > max(met[,'t_ice_dep'], na.rm = T))
# stopifnot(head(temp_breaks, 1) < min(met[,'t_ice_dep'], na.rm = T))
#
# stopifnot(length(temp_labs_seq) + 1 == length(temp_breaks))
#
# met$t_ice_dep_binned <- cut(met[,'t_ice_dep', drop = TRUE], temp_breaks)
#
# met$t_ice_dep_labs <- cut(met[,'t_ice_dep', drop = TRUE],
#                           temp_breaks,
#                           labels = temp_labs_seq)

## bin RH ----

min_rh <- round(
  min(met[,'rh'], na.rm = T), digits = 1)

max_rh <- round(
  max(met[,'rh'], na.rm = T), digits = 1)

rh_step <- 5

rh_breaks <- seq(
  60,
  100,
  rh_step)

rh_labs_seq <- label_bin_fn(bins = rh_breaks)

stopifnot(tail(rh_breaks, 1) >= max(met[,'rh'], na.rm = T))
stopifnot(head(rh_breaks, 1) < min(met[,'rh'], na.rm = T))

stopifnot(length(rh_labs_seq) + 1 == length(rh_breaks))

met$rh_binned <- cut(met[,'rh', drop = TRUE], rh_breaks)

met$rh_labs <- cut(met[,'rh', drop = TRUE],
                   rh_breaks,
                   labels = rh_labs_seq)

## bin solar ----

min_Qsi <- round(
  min(met[,'Qsi'], na.rm = T), digits = 1)

max_Qsi <- round(
  max(met[,'Qsi'], na.rm = T), digits = 1)

Qsi_step <- 50

Qsi_breaks <- seq(
  min_Qsi,
  max_Qsi+50,
  Qsi_step)

Qsi_labs_seq <- label_bin_fn(bins = Qsi_breaks)

stopifnot(tail(Qsi_breaks, 1) > max(met[,'Qsi'], na.rm = T))
stopifnot(head(Qsi_breaks, 1) <= min(met[,'Qsi'], na.rm = T))

stopifnot(length(Qsi_labs_seq) + 1 == length(Qsi_breaks))

met$Qsi_binned <- cut(met[,'Qsi', drop = TRUE], Qsi_breaks, include.lowest = T)

met$Qsi_labs <- cut(met[,'Qsi', drop = TRUE],
                    Qsi_breaks,
                    labels = Qsi_labs_seq, include.lowest = T)
## bin wind ----

min_wind <- 0
max_wind <- round(
  max(met[,good_wind], na.rm = T))

wind_step <- 0.5

wind_breaks <- seq(
  min_wind,
  max_wind+wind_step,
  wind_step)

wind_labs_seq <- label_bin_fn(bins = wind_breaks)

stopifnot(tail(wind_breaks, 1) > max(met[,good_wind], na.rm = T))

stopifnot(length(wind_labs_seq) + 1 == length(wind_breaks))

met$wind_binned <- cut(met[,good_wind, drop = TRUE], wind_breaks, include.lowest = T)

met$wind_labs <- cut(met[,good_wind, drop = TRUE],
                     wind_breaks,
                     labels = wind_labs_seq, include.lowest = T
)

## bin mid canopy wind as shear stress ----

# estimate shear stress using the regressions we created in
# eddy-cov/scripts/converting_mid_canopy_wind_to_tau.R

# lm_mid_wnd_sqrd_low_tau <-
#   readRDS('../eddy-cov/data/est_tau_from_wnd/lm_mid_wnd_sqrd_low_tau.rds')
#
# # since we forced the above through the origin we just need to multiply by the slope
#
# met$mid_can_est_tau <- (met[,good_wind]^2)*lm_mid_wnd_sqrd_low_tau$slope
#
# min_mid_can_tau <- 0
# max_mid_can_tau <- round(
#   max(met$mid_can_est_tau, na.rm = T), digits = 2)
#
# mid_can_tau_step <- 0.05
#
# mid_can_tau_breaks <- seq(
#   min_mid_can_tau,
#   max_mid_can_tau + mid_can_tau_step,
#   mid_can_tau_step)
#
# mid_can_tau_labs_seq <- label_bin_fn(bins = mid_can_tau_breaks)
#
# stopifnot(tail(mid_can_tau_breaks, 1) > max(met[,'mid_can_est_tau'], na.rm = T))
#
# stopifnot(length(mid_can_tau_labs_seq) + 1 == length(mid_can_tau_breaks))
#
# met$tau_mid_binned <- cut(met[,'mid_can_est_tau', drop = TRUE], mid_can_tau_breaks, include.lowest = T)
#
# met$tau_mid_labs <- cut(met[,'mid_can_est_tau', drop = TRUE],
#                         mid_can_tau_breaks,
#                         labels = mid_can_tau_labs_seq, include.lowest = T
# )

## bin tree load (mm) ----

min_tree <- 0
max_tree <- round(
  max(tree$weighed_tree_canopy_load_mm, na.rm = T),3)
tree_step <- 1.5

tree_breaks <- seq(
  min_tree,
  max_tree + tree_step,
  tree_step)

tree_labs_seq <- label_bin_fn(bins = tree_breaks)

stopifnot(tail(tree_breaks, 1) > max_tree)
stopifnot(length(tree_labs_seq) + 1 == length(tree_breaks))

tree$tree_binned <- cut(tree[,'weighed_tree_canopy_load_mm', drop = TRUE], tree_breaks, include.lowest = T)

tree$tree_labs <- cut(tree[,'weighed_tree_canopy_load_mm', drop = TRUE],
                     tree_breaks,
                     labels = tree_labs_seq, include.lowest = T
)

## bin particle velocity ----

# min_vel <- round(min(met$part_vel, na.rm = T), 3)
# #max_vel <- round(max(met$part_vel, na.rm = T), 3)
# max_vel <- 1.4
#
# vel_step <- (max_vel-min_vel) / 11
#
# vel_breaks <- seq(
#   min_vel,
#   max_vel + vel_step,
#   vel_step)
#
# vel_labs_seq <- label_bin_fn(bins = vel_breaks)
#
# stopifnot(tail(vel_breaks, 1) > max_vel)
# stopifnot(length(vel_labs_seq) + 1 == length(vel_breaks))
#
# met$part_vel_binned <- cut(met[,'part_vel', drop = TRUE], vel_breaks, include.lowest = T)
#
# met$part_vel_labs <- cut(met[,'part_vel', drop = TRUE],
#                          vel_breaks,
#                          labels = vel_labs_seq, include.lowest = T
# )
#
# ## bin particle diameter ----
#
# min_diam <- round(min(met$part_diam, na.rm = T), 3)
# max_diam <- round(max(met$part_diam, na.rm = T), 3)
# diam_step <- (max_diam-min_diam) / 15
#
# diam_breaks <- seq(
#   min_diam,
#   max_diam + diam_step,
#   diam_step)
#
# diam_labs_seq <- label_bin_fn(bins = diam_breaks)
#
# stopifnot(tail(diam_breaks, 1) > max_diam)
# stopifnot(length(diam_labs_seq) + 1 == length(diam_breaks))
#
# met$part_diam_binned <- cut(met[,'part_diam', drop = TRUE], diam_breaks, include.lowest = T)
#
# met$part_diam_labs <- cut(met[,'part_diam', drop = TRUE],
#                           diam_breaks,
#                           labels = diam_labs_seq, include.lowest = T
# )
#
## bin snowfall rate ----

min_sf <- round(min(met$q_sf, na.rm = T), 3)
max_sf <- round(max(met$q_sf, na.rm = T), 3)
sf_step <- 0.5

sf_breaks <- seq(
  min_sf,
  max_sf + sf_step,
  sf_step)

sf_labs_seq <- label_bin_fn(bins = sf_breaks)

stopifnot(tail(sf_breaks, 1) > max_sf)
stopifnot(length(sf_labs_seq) + 1 == length(sf_breaks))

met$q_sf_binned <- cut(met[,'q_sf', drop = TRUE], sf_breaks, include.lowest = T)

met$q_sf_labs <- cut(met[,'q_sf', drop = TRUE],
                     sf_breaks,
                     labels = sf_labs_seq, include.lowest = T
)

# Combine met and load data at 15 min timestep

met_intercept <- throughfall_periods_long |>
  left_join(q_tf, by = c('datetime')) |>
  left_join(tree, by = c('datetime', 'trough_name')) |>
  left_join(met, by = c('datetime')) |>
  mutate(
    q_sf_labs = as.numeric(as.character(q_sf_labs)),
    temp_labs = as.numeric(as.character(temp_labs)),
    # t_ice_labs = as.numeric(as.character(t_ice_labs)),
    # t_ice_dep_labs = as.numeric(as.character(t_ice_dep_labs)),
    rh_labs = as.numeric(as.character(rh_labs)),
    Qsi_labs = as.numeric(as.character(Qsi_labs)),
    wind_labs = as.numeric(as.character(wind_labs)),
    # tau_mid_labs = as.numeric(as.character(tau_mid_labs)),
    tree_labs = as.numeric(as.character(tree_labs)),
    # part_vel_labs = as.numeric(as.character(part_vel_labs)),
    # part_diam_labs = as.numeric(as.character(part_diam_labs))
  )

saveRDS(met_intercept, 'data/lysimeter-data/processed/continuous_throughfall_data_binned_met_select_events.rds')

# rmvd qc below as should be handeled in the run-precip.R pipeline

# short_storm_ids <- storms_long$storm_id |> unique()
#
# pivot_longer(met_intercept, c(q_sf, q_tf), names_to = 'rate_name', values_to = 'rate_value') |>
#   filter(storm_id %in% short_storm_ids[1:6]) |>
#   ggplot(aes(datetime, rate_value, colour = rate_name)) +
#   geom_point() +
#   facet_wrap(~storm_id, scales = 'free')
#
# pivot_longer(met_intercept, c(q_sf, q_tf), names_to = 'rate_name', values_to = 'rate_value') |>
#   filter(storm_id %in% short_storm_ids[20:23]) |>
#   ggplot(aes(datetime, rate_value, colour = rate_name)) +
#   geom_point() +
#   facet_wrap(~storm_id, scales = 'free')
#
# pivot_longer(met_intercept, c(q_sf, q_tf), names_to = 'rate_name', values_to = 'rate_value') |>
#   filter(storm_id %in% short_storm_ids[20:23]) |>
#   ggplot(aes(datetime, rate_value, colour = rate_name)) +
#   geom_point() +
#   facet_wrap(~storm_id, scales = 'free')
#
# pivot_longer(met_intercept, c(q_sf, q_tf), names_to = 'rate_name', values_to = 'rate_value') |>
#   filter(storm_id %in% short_storm_ids[20:23]) |>
#   ggplot(aes(datetime, rate_value, colour = rate_name)) +
#   geom_point() +
#   facet_wrap(~storm_id, scales = 'free')
