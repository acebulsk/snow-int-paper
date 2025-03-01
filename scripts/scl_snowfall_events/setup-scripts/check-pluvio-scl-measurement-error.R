# Script to determine minimum hourly interval to have suitible weight registerd
# in the pluvio/troughs so we are above the relative accuracy since the pluvio
# relative error is higher than the troughs we use this as the limiting factor
# and basis for determing the time interval to use

# How many of the raw obs have a interval measurement greater than their absolute error

error_th <- 50 # % error that is acceptable
scl_weight <- 15 # kg
scl_rel_accuracy <- 0.02/100 # rel accuracy is 0.02% as stated in the strain gauge manual
pluvio_rel_accuracy <- 0.2/100 # relative accuracy, or 0.1 mm if absolute error is less than 0.1 mm so use ifelse to control for this, pluvio measures at 0.1 mm resolution

tf_periods_scl_pcp_15min <- throughfall_periods_long |>
  left_join(ffr_met |> select(datetime, p, t, u)) |>
  left_join(pwl_pluvio_raw |> select(datetime, pluvio_raw_mm = value)) |>
  left_join(q_tf_scl) |>
  left_join(scl_raw_kg, by = c('datetime', 'trough_name')) |>
  left_join(scl_meta |> select(trough_name, surface_area), by = 'trough_name') |>
  select(-c(storm_id, bad_troughs))

tf_periods_scl_pcp_15min_err <- tf_periods_scl_pcp_15min |>
  mutate(
    # SCL calcs
    scl_raw_kg = scl_raw_kg + 15, # raw_kg does not include weight of SCL so add estimate here based on diff of when the troughs were taken down in the spring/summer
    scl_abs_accuracy = scl_raw_kg * scl_rel_accuracy,
    d_tf_kg = d_tf*surface_area,
    # Pluvio calcs
    pluvio_abs_accuracy = pluvio_raw_mm * pluvio_rel_accuracy,
    pluvio_abs_accuracy = ifelse(pluvio_abs_accuracy < 0.1, 0.1, pluvio_abs_accuracy),

    scl_rel_perc_error = (scl_abs_accuracy/d_tf_kg)*100,
    pluvio_rel_perc_error = (pluvio_abs_accuracy/ p)*100,
    scl_flag = ifelse(scl_rel_perc_error > error_th, T, F),
    pluvio_flag = ifelse(pluvio_rel_perc_error > error_th, T, F)
    )

n_records <- nrow(tf_periods_scl_pcp_15min_err)
scl_flags <- sum(tf_periods_scl_pcp_15min_err$scl_flag)
pluvio_flags <- sum(tf_periods_scl_pcp_15min_err$pluvio_flag)

print(paste0('Perc of SCL records flagged: ', (scl_flags/n_records)*100))

print(paste0('Perc of Pluvio records flagged: ', (pluvio_flags/n_records)*100))

# Try 1 hour intervals ----

tf_periods_scl_pcp_1hr <- tf_periods_scl_pcp_15min |>
  mutate(datetime = ceiling_date(datetime, unit = '1 hour')) |> # ceiling ensures the timestamp corresponds to preeceeding records
  group_by(datetime, w_tree_event, trough_name) |>
  summarise(p = sum(p),
            t = mean(t),
            u = mean(u),
            pluvio_raw_mm = last(pluvio_raw_mm), # take the last measurement of the interval
            d_tf = sum(d_tf),
            scl_raw_kg = last(scl_raw_kg),# take the last measurement of the interval
            surface_area = mean(surface_area))|>
  mutate(
    # SCL calcs
    scl_raw_kg = scl_raw_kg + 15, # raw_kg does not include weight of SCL so add estimate here based on diff of when the troughs were taken down in the spring/summer
    scl_abs_accuracy = scl_raw_kg * scl_rel_accuracy,
    d_tf_kg = d_tf*surface_area,
    # Pluvio calcs
    pluvio_abs_accuracy = pluvio_raw_mm * pluvio_rel_error,
    pluvio_abs_accuracy = ifelse(pluvio_abs_accuracy < 0.1, 0.1, pluvio_abs_accuracy),
    scl_rel_perc_error = (scl_abs_accuracy/d_tf_kg)*100,
    pluvio_rel_perc_error = (pluvio_abs_accuracy/ p)*100,
    scl_flag = ifelse(scl_rel_perc_error > error_th, T, F),
    pluvio_flag = ifelse(pluvio_rel_perc_error > error_th, T, F)
  )

n_records <- nrow(tf_periods_scl_pcp_1hr)
scl_flags <- sum(tf_periods_scl_pcp_1hr$scl_flag)
pluvio_flags <- sum(tf_periods_scl_pcp_1hr$pluvio_flag)

print(paste0('Perc of SCL records flagged: ', (scl_flags/n_records)*100))

print(paste0('Perc of Pluvio records flagged: ', (pluvio_flags/n_records)*100))


# Try 6 hour intervals ----

tf_periods_scl_pcp_6hr <- tf_periods_scl_pcp_15min |>
  mutate(datetime = ceiling_date(datetime, unit = '6 hours')) |> # ceiling ensures the timestamp corresponds to preeceeding records
  group_by(datetime, w_tree_event, trough_name) |>
  summarise(p = sum(p),
            t = mean(t),
            u = mean(u),
            pluvio_raw_mm = last(pluvio_raw_mm), # take the last measurement of the interval
            d_tf = sum(d_tf),
            scl_raw_kg = last(scl_raw_kg),# take the last measurement of the interval
            surface_area = mean(surface_area))|>
  mutate(
    # SCL calcs
    scl_raw_kg = scl_raw_kg + 15, # raw_kg does not include weight of SCL so add estimate here based on diff of when the troughs were taken down in the spring/summer
    scl_abs_accuracy = scl_raw_kg * scl_rel_accuracy,
    d_tf_kg = d_tf*surface_area,
    # Pluvio calcs
    pluvio_abs_accuracy = pluvio_raw_mm * pluvio_rel_error,
    pluvio_abs_accuracy = ifelse(pluvio_abs_accuracy < 0.1, 0.1, pluvio_abs_accuracy),
    scl_rel_perc_error = (scl_abs_accuracy/d_tf_kg)*100,
    pluvio_rel_perc_error = (pluvio_abs_accuracy/ p)*100,
    scl_flag = ifelse(scl_rel_perc_error > error_th, T, F),
    pluvio_flag = ifelse(pluvio_rel_perc_error > error_th, T, F)
  )

n_records <- nrow(tf_periods_scl_pcp_6hr)
scl_flags <- sum(tf_periods_scl_pcp_6hr$scl_flag)
pluvio_flags <- sum(tf_periods_scl_pcp_6hr$pluvio_flag)

print(paste0('Perc of SCL records flagged: ', (scl_flags/n_records)*100))

print(paste0('Perc of Pluvio records flagged: ', (pluvio_flags/n_records)*100))

# Try 12 hour intervals ----

tf_periods_scl_pcp_12hr <- tf_periods_scl_pcp_15min |>
  mutate(datetime = ceiling_date(datetime, unit = '12 hours')) |> # ceiling ensures the timestamp corresponds to preeceeding records
  group_by(datetime, w_tree_event, trough_name) |>
  summarise(p = sum(p),
            t = mean(t),
            u = mean(u),
            pluvio_raw_mm = last(pluvio_raw_mm), # take the last measurement of the interval
            d_tf = sum(d_tf),
            scl_raw_kg = last(scl_raw_kg),# take the last measurement of the interval
            surface_area = mean(surface_area))|>
  mutate(
    # SCL calcs
    scl_raw_kg = scl_raw_kg + 15, # raw_kg does not include weight of SCL so add estimate here based on diff of when the troughs were taken down in the spring/summer
    scl_abs_accuracy = scl_raw_kg * scl_rel_accuracy,
    d_tf_kg = d_tf*surface_area,
    # Pluvio calcs
    pluvio_abs_accuracy = pluvio_raw_mm * pluvio_rel_error,
    pluvio_abs_accuracy = ifelse(pluvio_abs_accuracy < 0.1, 0.1, pluvio_abs_accuracy),
    scl_rel_perc_error = (scl_abs_accuracy/d_tf_kg)*100,
    pluvio_rel_perc_error = (pluvio_abs_accuracy/ p)*100,
    scl_flag = ifelse(scl_rel_perc_error > error_th, T, F),
    pluvio_flag = ifelse(pluvio_rel_perc_error > error_th, T, F)
  )
n_records <- nrow(tf_periods_scl_pcp_12hr)
scl_flags <- sum(tf_periods_scl_pcp_12hr$scl_flag)
pluvio_flags <- sum(tf_periods_scl_pcp_12hr$pluvio_flag)

print(paste0('Perc of SCL records flagged: ', (scl_flags/n_records)*100))

print(paste0('Perc of Pluvio records flagged: ', (pluvio_flags/n_records)*100))

# Try 16 hour intervals ----

tf_periods_scl_pcp_16hr <- tf_periods_scl_pcp_15min |>
  mutate(datetime = ceiling_date(datetime, unit = '16 hours')) |> # ceiling ensures the timestamp corresponds to preeceeding records
  group_by(datetime, w_tree_event, trough_name) |>
  summarise(p = sum(p),
            t = mean(t),
            u = mean(u),
            pluvio_raw_mm = last(pluvio_raw_mm), # take the last measurement of the interval
            d_tf = sum(d_tf),
            scl_raw_kg = last(scl_raw_kg),# take the last measurement of the interval
            surface_area = mean(surface_area))|>
  mutate(
    # SCL calcs
    scl_raw_kg = scl_raw_kg + 15, # raw_kg does not include weight of SCL so add estimate here based on diff of when the troughs were taken down in the spring/summer
    scl_abs_accuracy = scl_raw_kg * scl_rel_accuracy,
    d_tf_kg = d_tf*surface_area,
    # Pluvio calcs
    pluvio_abs_accuracy = pluvio_raw_mm * pluvio_rel_error,
    pluvio_abs_accuracy = ifelse(pluvio_abs_accuracy < 0.1, 0.1, pluvio_abs_accuracy),
    scl_rel_perc_error = (scl_abs_accuracy/d_tf_kg)*100,
    pluvio_rel_perc_error = (pluvio_abs_accuracy/ p)*100,
    scl_flag = ifelse(scl_rel_perc_error > error_th, T, F),
    pluvio_flag = ifelse(pluvio_rel_perc_error > error_th, T, F)
  )
# n_records <- nrow(tf_periods_scl_pcp_12hr)
# scl_flags <- sum(tf_periods_scl_pcp_12hr$scl_flag)
# pluvio_flags <- sum(tf_periods_scl_pcp_12hr$pluvio_flag)
#
# print(paste0('Perc of SCL records flagged: ', (scl_flags/n_records)*100))
#
# print(paste0('Perc of Pluvio records flagged: ', (pluvio_flags/n_records)*100))

# Try 20 hour intervals ----

tf_periods_scl_pcp_20hr <- tf_periods_scl_pcp_15min |>
  mutate(datetime = ceiling_date(datetime, unit = '20 hours')) |> # ceiling ensures the timestamp corresponds to preeceeding records
  group_by(datetime, w_tree_event, trough_name) |>
  summarise(p = sum(p),
            t = mean(t),
            u = mean(u),
            pluvio_raw_mm = last(pluvio_raw_mm), # take the last measurement of the interval
            d_tf = sum(d_tf),
            scl_raw_kg = last(scl_raw_kg),# take the last measurement of the interval
            surface_area = mean(surface_area))|>
  mutate(
    # SCL calcs
    scl_raw_kg = scl_raw_kg + 15, # raw_kg does not include weight of SCL so add estimate here based on diff of when the troughs were taken down in the spring/summer
    scl_abs_accuracy = scl_raw_kg * scl_rel_accuracy,
    d_tf_kg = d_tf*surface_area,
    # Pluvio calcs
    pluvio_abs_accuracy = pluvio_raw_mm * pluvio_rel_error,
    pluvio_abs_accuracy = ifelse(pluvio_abs_accuracy < 0.1, 0.1, pluvio_abs_accuracy),
    scl_rel_perc_error = (scl_abs_accuracy/d_tf_kg)*100,
    pluvio_rel_perc_error = (pluvio_abs_accuracy/ p)*100,
    scl_flag = ifelse(scl_rel_perc_error > error_th, T, F),
    pluvio_flag = ifelse(pluvio_rel_perc_error > error_th, T, F)
  )
# n_records <- nrow(tf_periods_scl_pcp_12hr)
# scl_flags <- sum(tf_periods_scl_pcp_12hr$scl_flag)
# pluvio_flags <- sum(tf_periods_scl_pcp_12hr$pluvio_flag)
#
# print(paste0('Perc of SCL records flagged: ', (scl_flags/n_records)*100))
#
# print(paste0('Perc of Pluvio records flagged: ', (pluvio_flags/n_records)*100))

# Try 24 hour intervals ----

tf_periods_scl_pcp_24hr <- tf_periods_scl_pcp_15min |>
  mutate(datetime = ceiling_date(datetime, unit = '24 hours')) |> # ceiling ensures the timestamp corresponds to preeceeding records
  group_by(datetime, w_tree_event, trough_name) |>
  summarise(p = sum(p),
            t = mean(t),
            u = mean(u),
            pluvio_raw_mm = last(pluvio_raw_mm), # take the last measurement of the interval
            d_tf = sum(d_tf),
            scl_raw_kg = last(scl_raw_kg),# take the last measurement of the interval
            surface_area = mean(surface_area))|>
  mutate(
    # SCL calcs
    scl_raw_kg = scl_raw_kg + 15, # raw_kg does not include weight of SCL so add estimate here based on diff of when the troughs were taken down in the spring/summer
    scl_abs_accuracy = scl_raw_kg * scl_rel_accuracy,
    d_tf_kg = d_tf*surface_area,
    # Pluvio calcs
    pluvio_abs_accuracy = pluvio_raw_mm * pluvio_rel_error,
    pluvio_abs_accuracy = ifelse(pluvio_abs_accuracy < 0.1, 0.1, pluvio_abs_accuracy),
    scl_rel_perc_error = (scl_abs_accuracy/d_tf_kg)*100,
    pluvio_rel_perc_error = (pluvio_abs_accuracy/ p)*100,
    scl_flag = ifelse(scl_rel_perc_error > error_th, T, F),
    pluvio_flag = ifelse(pluvio_rel_perc_error > error_th, T, F)
  )
# n_records <- nrow(tf_periods_scl_pcp_12hr)
# scl_flags <- sum(tf_periods_scl_pcp_12hr$scl_flag)
# pluvio_flags <- sum(tf_periods_scl_pcp_12hr$pluvio_flag)
#
# print(paste0('Perc of SCL records flagged: ', (scl_flags/n_records)*100))
#
# print(paste0('Perc of Pluvio records flagged: ', (pluvio_flags/n_records)*100))

# Try 36 hour intervals ----

tf_periods_scl_pcp_48hr <- tf_periods_scl_pcp_15min |>
  mutate(datetime = ceiling_date(datetime, unit = '2 days')) |> # ceiling ensures the timestamp corresponds to preeceeding records
  group_by(datetime, w_tree_event, trough_name) |>
  summarise(p = sum(p),
            t = mean(t),
            u = mean(u),
            pluvio_raw_mm = last(pluvio_raw_mm), # take the last measurement of the interval
            d_tf = sum(d_tf),
            scl_raw_kg = last(scl_raw_kg),# take the last measurement of the interval
            surface_area = mean(surface_area))|>
  mutate(
    # SCL calcs
    scl_raw_kg = scl_raw_kg + 15, # raw_kg does not include weight of SCL so add estimate here based on diff of when the troughs were taken down in the spring/summer
    scl_abs_accuracy = scl_raw_kg * scl_rel_accuracy,
    d_tf_kg = d_tf*surface_area,
    # Pluvio calcs
    pluvio_abs_accuracy = pluvio_raw_mm * pluvio_rel_error,
    pluvio_abs_accuracy = ifelse(pluvio_abs_accuracy < 0.1, 0.1, pluvio_abs_accuracy),
    scl_rel_perc_error = (scl_abs_accuracy/d_tf_kg)*100,
    pluvio_rel_perc_error = (pluvio_abs_accuracy/ p)*100,
    scl_flag = ifelse(scl_rel_perc_error > error_th, T, F),
    pluvio_flag = ifelse(pluvio_rel_perc_error > error_th, T, F)
  )
# n_records <- nrow(tf_periods_scl_pcp_12hr)
# scl_flags <- sum(tf_periods_scl_pcp_12hr$scl_flag)
# pluvio_flags <- sum(tf_periods_scl_pcp_12hr$pluvio_flag)
#
# print(paste0('Perc of SCL records flagged: ', (scl_flags/n_records)*100))
#
# print(paste0('Perc of Pluvio records flagged: ', (pluvio_flags/n_records)*100))

# Event intervals ----

tf_periods_scl_pcp_event <- tf_periods_scl_pcp_15min |>
  group_by(w_tree_event, trough_name) |>
  summarise(
    datetime = NA,
    p = sum(p),
            t = mean(t),
            u = mean(u),
            pluvio_raw_mm = last(pluvio_raw_mm), # take the last measurement of the interval
            d_tf = sum(d_tf),
            scl_raw_kg = last(scl_raw_kg),# take the last measurement of the interval
            surface_area = mean(surface_area))|>
  mutate(
    # SCL calcs
    scl_raw_kg = scl_raw_kg + 15, # raw_kg does not include weight of SCL so add estimate here based on diff of when the troughs were taken down in the spring/summer
    scl_abs_accuracy = scl_raw_kg * scl_rel_accuracy,
    d_tf_kg = d_tf*surface_area,
    # Pluvio calcs
    pluvio_abs_accuracy = pluvio_raw_mm * pluvio_rel_error,
    pluvio_abs_accuracy = ifelse(pluvio_abs_accuracy < 0.1, 0.1, pluvio_abs_accuracy),
    scl_rel_perc_error = (scl_abs_accuracy/d_tf_kg)*100,
    pluvio_rel_perc_error = (pluvio_abs_accuracy/ p)*100,
    scl_flag = ifelse(scl_rel_perc_error > error_th, T, F),
    pluvio_flag = ifelse(pluvio_rel_perc_error > error_th, T, F)
  )
n_records <- nrow(tf_periods_scl_pcp_12hr)
scl_flags <- sum(tf_periods_scl_pcp_12hr$scl_flag)
pluvio_flags <- sum(tf_periods_scl_pcp_12hr$pluvio_flag)

print(paste0('Perc of SCL records flagged: ', (scl_flags/n_records)*100))

print(paste0('Perc of Pluvio records flagged: ', (pluvio_flags/n_records)*100))
