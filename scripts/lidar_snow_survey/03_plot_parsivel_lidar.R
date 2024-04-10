# plot PSVD matrix for each snowfall event ----

source('scripts/00_define_global_attributes.R')

source('scripts/fsd_snow_survey/00_load_snow_survey_data.R')

psvd_base_file <- '/media/alex/phd-data/local-usask/field-downloads/disdrometer/raw_data/Parsivel_Data_Until_20231004/Parsivel_Data/'

lidar_events_long_dt$psvd_file_name <- paste0(psvd_base_file,
                                              format(lidar_events_long_dt$datetime,
                                                     '%Y%m%d%H%M%SFORTRESSPW.txt'))

velocity_counts_all <- data.frame()

events <- lidar_events_long_dt$event_id |> unique()
for(event in events){
  psvd_files <- lidar_events_long_dt$psvd_file_name[lidar_events_long_dt$event_id == event]
  # psvd_files <- c('/media/alex/phd-data/local-usask/field-downloads/disdrometer/raw_data/Parsivel_Data_Until_20231004/Parsivel_Data//20210422154700FORTRESSPW.txt',
  #                '/media/alex/phd-data/local-usask/field-downloads/disdrometer/raw_data/Parsivel_Data_Until_20231004/Parsivel_Data//20210422154700FORTRESSPW.txt')

  # psvd_vector_to_3d_matrix('/media/alex/phd-data/local-usask/field-downloads/disdrometer/raw_data/Parsivel_Data_Until_20231004/Parsivel_Data/20230313131500FORTRESSPW.txt')
  # psvd_vector_to_3d_matrix('/media/alex/phd-data/local-usask/field-downloads/disdrometer/raw_data/Parsivel_Data_Until_20231004/Parsivel_Data/20230324213200FORTRESSPW.txt',
  #                          incomplete_psvd = T)
  # parse each file and return a nested list of 32x32 matrixes if the file contains spectrum data
  psvd_3d_mtx <-
    parallel::mclapply(
      psvd_files,
      psvd_vector_to_3d_matrix,
      incomplete_psvd = T,
      mc.cores = 8
    )

  if(!is.null(psvd_3d_mtx |> unlist())){
    psvd_3d_mtx_sum <- sum_psvd_3d_mtx(psvd_3d_mtx = psvd_3d_mtx)

    velocity_counts <- colSums(psvd_3d_mtx_sum) |>
      as_tibble(rownames = 'vel_bin') |>
      mutate(across(everything(), as.numeric),
             event_id = as.Date(event))

    vel_mean <- rep(velocity_counts$vel_bin,
        times = velocity_counts$value, na.rm = T) |> mean()

    vel_med <- rep(velocity_counts$vel_bin,
        times = velocity_counts$value, na.rm = T) |> median()

    line_data <- data.frame(xintercept = c(vel_mean, vel_med),
                            color = c("red", "blue"),
                            linetype = c("Mean", "Median"))

    p_vel <- ggplot(velocity_counts, aes(x = vel_bin, y = value)) +
      geom_bar(stat = "identity", fill = "skyblue") +
      xlim(c(0,3)) +
      geom_text(data = line_data, aes(x = 2, y = 10000, label = paste0('Mean: ',round(vel_mean, 2), '\nMedian: ', round(vel_med, 2)))) +
      geom_vline(data = line_data, aes(xintercept = xintercept, linetype = linetype), size = 1) +
      labs(title = as.Date(event),
           x = "Velocity (m/s)",
           y = "Frequency")
    # p_vel
    ggsave(paste0('figs/lidar_periods/parsivel_freq_distributions/velocity/', format(as.Date(event), '%Y%m%d'), '_velocity_freq_dist.png'),
           p_vel,
           width = 8.5, height = 4)

    psvd_long <- pivot_psvd_longer(psvd_3d_mtx_sum)
    p_psvd <- psvd_plot(
      psvd_long,
      model = c(
        # 'Rain',
        # 'P1e - Dendrite',
        'Wet', # from rasmussern1999 wet sphere
        'Dry', # from rasmussern1999 dry sphere
        'R2b - Densely Rimed Stellar'
        # 'R4c - Conical Graupel'
      ),
      alt = 2000
    )
    ggsave(paste0('figs/lidar_periods/psvd_frequency_distributions/', format(as.Date(event), '%Y%m%d'), '_dia_vs_velocity_freq_distributions.png'),
           p_psvd,
           width = 7, height = 5)

  } else {
    print(paste('No data for:', as.Date(event)))
    # confirmed no data for 20220216 and 20230127 using:
    # alex@water-usask:/media/alex/phd-data/local-usask/field-downloads/disdrometer/raw_data/Parsivel_Data_Until_20231004/Parsivel_Data$ ls 20220216*
    # ls: cannot access '20220216*': No such file or directory
    # alex@water-usask:/media/alex/phd-data/local-usask/field-downloads/disdrometer/raw_data/Parsivel_Data_Until_20231004/Parsivel_Data$ ls 20230127*
    #   ls: cannot access '20230127*': No such file or directory

  }
}

# removed because we only have data for one event
# plot all velocity frequency distributions on one plot
# ggplot(velocity_counts_all, aes(x = vel_bin, y = value, fill = as.factor(event_id))) +
#   geom_bar(stat = "identity", position = 'dodge') +
#   labs(x = "Velocity (m/s)",
#        y = "Frequency") +
#   xlim(0, 5)
#
# ggsave(paste0('figs/snow_survey_periods/parsivel_freq_distributions/velocity/all_dates_velocity_freq_dist.png'),
#        width = 8.5, height = 4)

# plot normalised frequency

# velocity_counts_normalised <- velocity_counts_all |>
#   group_by(event_id) |>
#   mutate(event_total = sum(value),
#          percent = (value / event_total)*100)

# ggplotly()

# ggplot(velocity_counts_normalised, aes(x = vel_bin, y = percent, fill = as.factor(event_id))) +
#   geom_bar(stat = "identity", position = 'dodge') +
#   labs(x = "Velocity (m/s)",
#        y = "Percent (%)") +
#   xlim(0, 5)
#
# # ggplotly()
#
# ggsave(paste0('figs/snow_survey_periods/parsivel_freq_distributions/velocity/all_dates_velocity_norm_freq_dist.png'),
#        width = 8.5, height = 4)
