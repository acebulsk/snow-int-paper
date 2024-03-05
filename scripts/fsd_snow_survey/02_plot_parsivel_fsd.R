# plot frequency distribution of parsivel data for each snowfall event
library(abind)

source('../../analysis/disdrometer/scripts/00_source_functions.R')

psvd_base_file <- '/media/alex/phd-data/local-usask/field-downloads/disdrometer/raw_data/Parsivel_Data_Until_20231004/Parsivel_Data/'

event_dt_long <-
  purrr::pmap_dfr(fsd_periods_wide |>
                    select(from = snowfall_start_time,
                           to = fsd_end_time,
                           event_id), to_long) |>
  mutate(psvd_file_name = paste0(psvd_base_file,
                                format(datetime,
                                       '%Y%m%d%H%M%SFORTRESSPW.txt')))
aggregate_over_event_df_all <- NULL

for(event in fsd_periods_wide$event_id){
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
    ggsave(paste0('figs/frequency_distributions/', format(as.Date(event), '%Y%m%d'), '_dia_vs_velocity_freq_distributions.png'),
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
