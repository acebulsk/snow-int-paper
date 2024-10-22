# compute disdrometer particle size velocity distributions for snowfall periods
# these periods were defined in the context of the automated sensors and are
# independent of the lidar and snow survey analysis

library(abind)

source('../../analysis/disdrometer/scripts/00_source_functions.R')

psvd_base_file <- '/media/alex/phd-data/local-usask/field-downloads/disdrometer/raw_data/Parsivel_Data_Until_20231004/Parsivel_Data/'

# get periods updated to not include times where troughs are unloading

event_dates_wide <- read.csv('../../analysis/interception/data/select_storms_datetime_wide_independent_snow_surveys.csv', skip = 1) |>
  filter(quality < 4) |>
  mutate(
    from = as.POSIXct(from, tz = 'Etc/GMT+6'),
    to = as.POSIXct(to, tz = 'Etc/GMT+6'),
    event_id = as.Date(from, tz = 'Etc/GMT+6')) |>
  select(from, to, event_id)

event_dt_long <-
  purrr::pmap_dfr(event_dates_wide |>
                    select(from,
                           to,
                           event_id), to_long_one_minute) |>
  mutate(psvd_file_name = paste0(psvd_base_file,
                                 format(datetime,
                                        '%Y%m%d%H%M%SFORTRESSPW.txt')))

velocity_counts_all <- data.frame()

for(event in event_dates_wide$event_id){
  psvd_files <- event_dt_long$psvd_file_name[event_dt_long$event_id == event]
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

    velocity_counts <- rowSums(psvd_3d_mtx_sum) |>
      as_tibble(rownames = 'vel_bin') |>
      mutate(across(everything(), as.numeric),
             event_id = as.Date(event))

    velocity_counts_all <- rbind(velocity_counts_all, velocity_counts)

    p_vel <- ggplot(velocity_counts, aes(x = vel_bin, y = value)) +
      geom_bar(stat = "identity", fill = "skyblue") +
      xlim(c(0, 15)) +
      labs(title = as.Date(event),
           x = "Velocity (m/s)",
           y = "Frequency")

    ggsave(paste0('figs/automated_snowfall_event_periods/parsivel_freq_distributions/velocity/frequency/', format(as.Date(event), '%Y%m%d'), '_velocity_freq_dist.png'),
           p_vel,
           width = 8.5, height = 4)

    p_vel <- ggplot(velocity_counts, aes(x = vel_bin, y = (value/sum(velocity_counts$value))*100)) +
      geom_bar(stat = "identity", fill = "skyblue") +
      xlim(c(0, 15)) +
      labs(title = as.Date(event),
           x = "Velocity (m/s)",
           y = "Percent (%)")

    ggsave(paste0('figs/automated_snowfall_event_periods/parsivel_freq_distributions/velocity/normalised/', format(as.Date(event), '%Y%m%d'), '_velocity_freq_dist.png'),
           p_vel,
           width = 8.5, height = 4)

    psvd_long <- pivot_psvd_longer(psvd_3d_mtx_sum)
    p <- psvd_plot(
      psvd_long,
      model = c(
        'Rain',
        'P1e - Dendrite',
        'R2b - Densely Rimed Stellar',
        'R4c - Conical Graupel'
      ),
      alt = 2000
    )
    ggsave(paste0('figs/automated_snowfall_event_periods/parsivel_freq_distributions/psvd/', format(as.Date(event), '%Y%m%d'), '_dia_vs_velocity_freq_distributions.png'),
           p,
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

# plot all velocity frequency distributions on one plot
ggplot(velocity_counts_all, aes(x = vel_bin, y = value, fill = as.factor(event_id))) +
  geom_bar(stat = "identity", position = 'dodge') +
  labs(x = "Velocity (m/s)",
       y = "Frequency") +
  xlim(0, 5)

ggsave(paste0('figs/automated_snowfall_event_periods/parsivel_freq_distributions/velocity/all_dates_velocity_freq_dist.png'),
       width = 8.5, height = 4)

ggplotly()

# plot normalised frequency

velocity_counts_normalised <- velocity_counts_all |>
  group_by(event_id) |>
  mutate(event_total = sum(value),
         percent = (value / event_total)*100)

ggplot(velocity_counts_normalised, aes(x = vel_bin, y = percent, fill = as.factor(event_id))) +
  geom_bar(stat = "identity", position = 'dodge') +
  labs(x = "Velocity (m/s)",
       y = "Percent (%)") +
  xlim(0, 8)

ggplotly()

ggsave(paste0('figs/automated_snowfall_event_periods/parsivel_freq_distributions/velocity/all_dates_velocity_norm_freq_dist.png'),
       width = 8.5, height = 4)
