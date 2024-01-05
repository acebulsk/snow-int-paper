# INTERCEPTION DATA

# bring in state of ablation data when finished analysis and poster is created so we have the saved state of data for reproducability
# file.copy('../../analysis/interception/data', 'data_archives/interception', recursive = T, overwrite = T, copy.date = T)

ip_filter <- 1
vza_select <- 45 # vertical zeneith angle selected based on best angle that had alignment between canopy coverage and LAI and IP
calm_colour <- '#0d0887'
windy_colour <- '#f89540'
ip_y_lims <- c(0.05, 1)

ip_y_ax_lab <- 'Interception Efficiency (-)'
legend_title <- 'Wind Speed (m/s)'
windy_name <- 'High: '
calm_name <- 'Low: '

# snow survey data ----

fsd_storm_summary_met <- readRDS('../../analysis/interception/data/storm_analysis/snow_survey/fsd_ip_met_summary_T1_1_32_T2_1_32.rds')
fsd_all <- readRDS('../../analysis/interception/data/storm_analysis/snow_survey/fsd_w_grounds_ip_all_pts.rds') |>
  mutate(uid = paste0(transect, '_', num, canopy))
fsd_all <- fsd_all[!(fsd_all$transect == 'T1' & fsd_all$num == 12),]

# these FSD dates have suspect IP

outlier_events_fsd <- c(
  '2022-02-14', # very small event accumulation < 5 mm so instrument error probably too high here. but maybe keep FSD since had high precision measurements on crust
  '2022-05-06', # 3 days between surveys here in may with about 5 cm melt observed on the sr50 so erroneously high IP in open an should remove
  '2022-05-17' # very warm in the afternoon resulted in abnormally high IP vals
  # '2023-03-13' # windy one with some unloading observed on tree and troughs mid event
) |> as.Date()

windy_events_fsd <- c(
  '2022-03-31',
  # '2023-02-15', # also highish winds but not seeing strong influence in the data
  '2023-03-13'
)|> as.Date()

warm_events <- c(
  '2023-03-13',
  '2022-05-06',
  '2022-05-17',
  '2023-01-26'
) |> as.Date()

cold_events <- c(
  '2022-04-08',
  '2023-02-15',
  '2022-02-14',
  '2023-03-24') |>
  as.Date()

fsd_all_slim <- fsd_all |>
  filter(!storm_id %in% outlier_events_fsd,
         IP <= ip_filter)

fsd_all_slim_low_wind <- fsd_all |>
  filter(!storm_id %in% c(outlier_events_fsd, windy_events_fsd),
         IP <= ip_filter)

fsd_all_met <- left_join(fsd_all_slim, fsd_storm_summary_met |> rename(avg_IP = IP), by = 'storm_id')
lai_measurements <- read.csv('../../analysis/interception/data/lai/results/2023_compiled_lai_vza_15_60.csv')  |>
  select(-X)
lai_df <- read.csv('../../analysis/interception/data/lai/lai_site_id_2023_05_04.csv') |>
  mutate(id = paste0('DSCN6', img_id_short, '.JPG')) |>
  left_join(lai_measurements, by = 'id', relationship = "many-to-many") |>
  rename(num = old_num) |>
  mutate(vza_label = paste('Max Zenith Angle (deg.):' ,vza))

fsd_avg_wind <- fsd_storm_summary_met |>
  filter(!storm_id %in% outlier_events_fsd)|>
  mutate(wind_class = case_when(
    storm_id %in% windy_events_fsd ~ windy_name,
    TRUE ~ calm_name
  )) |>
  group_by(wind_class) |>
  dplyr::summarise(avg_wind = mean(`Avg. Wind Speed (m/s)`),
                   sd_wind = mean(`St. Dev. Wind Speed (m/s)`),
                   min_wind = min(`Min Wind Speed (m/s)`),
                   max_wind = max(`Peak Wind Speed (m/s)`),
                   min_avg_wind = min(`Avg. Wind Speed (m/s)`),
                   max_avg_wind = max(`Avg. Wind Speed (m/s)`),
                   min_med_wind = min(`Median Wind Speed (m/s)`),
                   max_med_wind = max(`Median Wind Speed (m/s)`))

windy_min_median_wind <- fsd_avg_wind$min_med_wind[fsd_avg_wind$wind_class == windy_name]
windy_max_median_wind <- fsd_avg_wind$max_med_wind[fsd_avg_wind$wind_class == windy_name]
calm_min_median_wind <- fsd_avg_wind$min_med_wind[fsd_avg_wind$wind_class == calm_name]
calm_max_median_wind <- fsd_avg_wind$max_med_wind[fsd_avg_wind$wind_class == calm_name]

windy_name <- paste('High:', windy_min_median_wind, '–', windy_max_median_wind)
calm_name <- paste('Low:', calm_min_median_wind, '–', calm_max_median_wind)

fsd_ip_lai <- left_join(fsd_all_slim, lai_df, by = c('transect', 'num', 'canopy')) |>
  # select(id, transect, num, canopy, cc, Le, IP, vza) |>
  filter(is.na(cc) == F) |>
  mutate(wind_class = case_when(
    storm_id %in% windy_events_fsd ~ windy_name,
    TRUE ~ calm_name
  ),
  temp_class = case_when(
    storm_id %in% warm_events ~ warm_name,
    storm_id %in% cold_events ~ cold_name,
    TRUE ~ NA
  )
  )

fsd_ip_lai$wind_class <- factor(x = fsd_ip_lai$wind_class, levels = c("Low: 0.69 – 1.27", "High: 1.37 – 1.72"))

cc <- fsd_ip_lai |>
  filter(vza == vza_select) |>
  ggplot(aes(cc, IP, colour = wind_class)) +
  geom_point() +
  # geom_smooth(method = 'lm', se = F) +
  stat_cor(aes(colour = wind_class,label = after_stat(rr.label)), geom = "label", show.legend = F) +
  ylab(ip_y_ax_lab) +
  xlab('Canopy Coverage (-)')  +
  ylim(c(0,1)) +
  scale_color_manual(name = legend_title, values = c(calm_colour, windy_colour)) +
  theme(legend.position = 'none')

lai <- fsd_ip_lai |>
  filter(vza == vza_select) |>
  ggplot(aes(Le, IP, colour = wind_class)) +
  geom_point() +
  # geom_smooth(method = 'lm', se = F) +
  stat_cor(aes(colour = wind_class,label = after_stat(rr.label)), geom = "label", show.legend = F) +
  ylab(ip_y_ax_lab) +
  xlab('Leaf Area Index (-)')  +
  ylim(c(0,1)) +
  scale_color_manual(name = legend_title, values = c(calm_colour, windy_colour)) +
  theme(legend.position = 'right')


cowplot::plot_grid(cc, lai, labels = c('A', 'B'), rel_widths = c(.65, 1))

ggsave('figs/interception/forest_ip.png', device = png, width = 9.5, height = 4)

# LAI boxplot like the airtemp and wind speed plots

fsd_all_slim_low_wind_lai <-
  left_join(fsd_all_slim_low_wind, lai_df, by = c('transect', 'num', 'canopy')) |>
  # select(id, transect, num, canopy, cc, Le, IP, vza) |>
  filter(is.na(cc) == F,
         vza == vza_select)

min <- round(min(fsd_all_slim_low_wind_lai$Le, na.rm = T), 3)
max <- round(max(fsd_all_slim_low_wind_lai$Le, na.rm = T), 2)
step <- round((max-min) / 10, 1)

breaks <- seq(
  min,
  max + step,
  step)

labs_seq <- label_bin_fn(bins = breaks)

stopifnot(tail(breaks, 1) > max)
stopifnot(length(labs_seq) + 1 == length(breaks))

fsd_all_slim_low_wind_lai$le_binned <- cut(fsd_all_slim_low_wind_lai[,'Le', drop = TRUE], breaks, include.lowest = T)

fsd_all_slim_low_wind_lai$le_labs <- as.numeric(cut(fsd_all_slim_low_wind_lai[,'Le', drop = TRUE],
                          breaks,
                          labels = labs_seq,
                          include.lowest = T
))

lai_ip_boxplot <- fsd_all_slim_low_wind_lai |>
  ggplot(aes(le_labs, IP, group = le_labs)) +
  geom_boxplot()+
  ylab(ip_y_ax_lab) +
  xlab('LAI bin (-)')+
  theme(plot.margin = margin(0.5, 0.5, 0.5, .75, "cm"))

lai_ip_boxplot

fsd_all_slim_low_wind_lai |>
  ggplot(aes(Le, IP)) +
  geom_point()+
  xlab('LAI (-)')


# CC boxplot like the airtemp and wind speed plots

min <- round(min(fsd_all_slim_low_wind_lai$cc, na.rm = T), 3)
max <- round(max(fsd_all_slim_low_wind_lai$cc, na.rm = T), 3)
step <- round((max-min) / 10, 1)

breaks <- seq(
  min,
  max + step,
  step)

labs_seq <- label_bin_fn(bins = breaks)

stopifnot(tail(breaks, 1) > max)
stopifnot(length(labs_seq) + 1 == length(breaks))

fsd_all_slim_low_wind_lai$cc_binned <- cut(fsd_all_slim_low_wind_lai[,'cc', drop = TRUE], breaks, include.lowest = T)

fsd_all_slim_low_wind_lai$cc_labs <- as.numeric(cut(fsd_all_slim_low_wind_lai[,'cc', drop = TRUE],
                             breaks,
                             labels = labs_seq,
                             include.lowest = T
))

fsd_all_slim_low_wind_lai |>
  ggplot(aes(cc_labs, IP, group = cc_labs)) +
  geom_boxplot()+
  xlab('Canopy Coverage (-)')

fsd_all_slim_low_wind_lai |>
  ggplot(aes(cc, IP)) +
  geom_point()+

  xlab('Canopy Coverage (-)')
