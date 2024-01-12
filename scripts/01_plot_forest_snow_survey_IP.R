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

ggplot(fsd_all_slim, aes(IP)) +
  geom_histogram() +
  facet_wrap(~storm_id)

fsd_all_slim_low_wind <- fsd_all |>
  filter(!storm_id %in% c(outlier_events_fsd, windy_events_fsd),
         IP <= ip_filter)

fsd_storm_summary_met_slim <- fsd_storm_summary_met |>
  filter(!storm_id %in% outlier_events_fsd)

fsd_all_met <- left_join(fsd_all_slim, fsd_storm_summary_met |> rename(avg_trough_IP = IP), by = 'storm_id')

fsd_all_slim_smry <- fsd_all_slim |>
  group_by(storm_id) |>
  summarise(mean_IP = mean(IP),
            mean_I = mean(I),
            sd_IP = sd(IP)) |>
  left_join(fsd_storm_summary_met |> rename(avg_trough_IP = IP), by = 'storm_id')

saveRDS(fsd_all_slim_smry, 'data/fsd_storm_summary_met_no_outlier.rds')

fsd_all_slim_smry |>
  rename(
    `Relative Humidity (%)` = `RH (%)`,
    `Wind Speed (m/s)` = `Avg. Wind Speed (m/s)`,
    `Snowfall (mm)` = `Open Precip. (mm)`,
    `Hydrometeor Diameter (mm)` = part_diam,
         `Hydrometeor Velocity (m/s)` = part_vel) |>
  pivot_longer(
    c(
      `Air Temp. (°C)`,
      `Relative Humidity (%)`,
      # `Median Wind Speed (m/s)`,
      `Wind Speed (m/s)`,
      `Snowfall (mm)`,
      `Hydrometeor Diameter (mm)`,
      `Hydrometeor Velocity (m/s)`
    )
  ) |>
  group_by(name) |>
  mutate(
    err_bar_width = (max(value, na.rm = T) - min(value, na.rm = T)) / 20,
      name = factor(
        name,
        ordered = T,
        levels = c(
          'Snowfall (mm)',
          'Air Temp. (°C)',
          'Relative Humidity (%)',
          'Wind Speed (m/s)',
          'Hydrometeor Diameter (mm)',
          'Hydrometeor Velocity (m/s)'
        ))) |>
  ggplot(aes(value, mean_IP)) +
  geom_point(shape = 1, size = 4) +
  geom_errorbar(aes(ymax = mean_IP + sd_IP, ymin = mean_IP - sd_IP, width = err_bar_width)) +
  # facet_grid(cols = vars(name), scales = 'free')
  facet_wrap(~name, scales = 'free')+
  ylab(ip_y_ax_lab) +
  theme(axis.title.x = element_blank())

ggsave('figs/interception/snow_survey_ip_w_met.png', device = png, width = 8.5, height = 4)


# work on plotting lai vs ip ----

lai_measurements <- read.csv('../../analysis/interception/data/lai/results/2023_compiled_lai_vza_15_60.csv')  |>
  select(-X)
lai_df <- read.csv('../../analysis/interception/data/lai/lai_site_id_2023_05_04.csv') |>
  mutate(id = paste0('DSCN6', img_id_short, '.JPG')) |>
  left_join(lai_measurements, by = 'id', relationship = "many-to-many") |>
  rename(num = old_num) |>
  mutate(vza_label = paste('Max Zenith Angle (deg.):' ,vza))

fsd_storm_summary_met |>
  filter(!storm_id %in% outlier_events_fsd)|>
  mutate(wind_class = case_when(
    storm_id %in% windy_events_fsd ~ windy_name,
    TRUE ~ calm_name
  )
  ) |> View()

fsd_avg_wind <- fsd_storm_summary_met |>
  filter(!storm_id %in% outlier_events_fsd)|>
  mutate(wind_class = case_when(
    storm_id %in% windy_events_fsd ~ windy_name,
    TRUE ~ calm_name
  )) |>
  group_by(wind_class) |>
  dplyr::summarise(avg_wind = mean(`Avg. Wind Speed (m/s)`),
                   sd_wind = mean(`St. Dev. Wind Speed (m/s)`),
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

fsd_ip_lai <- left_join(fsd_all_slim |> filter(!storm_id %in% outlier_events_fsd), lai_df, by = c('transect', 'num', 'canopy')) |>
  # select(id, transect, num, canopy, cc, Le, IP, vza) |>
  filter(is.na(cc) == F) |>
  mutate(wind_class = case_when(
    storm_id %in% windy_events_fsd ~ windy_name,
    TRUE ~ calm_name
  )
  )

ggplot(fsd_ip_lai, aes(Le)) +
  geom_histogram() +
  facet_wrap(~storm_id)

fsd_ip_lai$wind_class <- factor(x = fsd_ip_lai$wind_class, levels = c("Low: 0.69 – 1.27", "High: 1.37 – 1.72"))

cc <- fsd_ip_lai |>
  filter(vza == vza_select) |>
  ggplot(aes(cc, IP, colour = wind_class)) +
  geom_point() +
  # geom_smooth(method = 'lm', se = F) +
  stat_cor(aes(label = paste(..rr.label..,
                             if_else(readr::parse_number(..p.label..) < 0.001,
                                     "p<0.001", ..p.label..), sep = "~`, `~"), colour = wind_class), geom = "label", show.legend = F) +
  ylab(ip_y_ax_lab) +
  xlab('Canopy Coverage (-)')  +
  ylim(c(0,1.25)) +
  scale_color_manual(name = legend_title, values = c(calm_colour, windy_colour)) +
  theme(legend.position = 'none')
cc
lai <- fsd_ip_lai |>
  filter(vza == vza_select) |>
  ggplot(aes(Le, IP, colour = wind_class)) +
  geom_point() +
  # geom_smooth(method = 'lm', se = F) +
  stat_cor(aes(label = paste(..rr.label..,
                             if_else(readr::parse_number(..p.label..) < 0.001,
                                     "p<0.001", ..p.label..), sep = "~`, `~"), colour = wind_class), geom = "label", show.legend = F) +  ylab(ip_y_ax_lab) +
  xlab('Leaf Area Index (-)')  +
  ylim(c(0,1.25)) +
  scale_color_manual(name = legend_title, values = c(calm_colour, windy_colour)) +
  theme(legend.position = 'right')
lai

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

fsd_all_slim_low_wind_lai$le_labs <- cut(fsd_all_slim_low_wind_lai[,'Le', drop = TRUE],
                          breaks,
                          labels = labs_seq,
                          include.lowest = T
)

fsd_all_slim_low_wind_lai$le_labs <- fsd_all_slim_low_wind_lai$le_labs |> as.character() |> as.numeric()


# TODO MAKE NOT BOXPLOT
lai_ip_boxplot <- fsd_all_slim_low_wind_lai |>
  ggplot(aes(le_labs, IP, group = le_labs)) +
  geom_boxplot()+
  ylab(ip_y_ax_lab)+
  xlab(le_ax_lab)+
  theme(plot.margin = margin(0.5, 0.5, 0.5, .75, "cm"))

lai_ip_boxplot

fsd_all_slim_low_wind_lai |>
  ggplot(aes(Le, IP)) +
  geom_point()+
  xlab(le_ax_lab)

le_ip_smry <- fsd_all_slim_low_wind_lai |>
  group_by(le_labs) |>
  # filter(weighed_tree_canopy_load_mm <= 5) |>
  summarise(IP_avg = mean(IP, na.rm = T),
            sd = sd(IP, na.rm = T),
            sd_low = IP_avg - sd,
            sd_hi = IP_avg + sd,
            ci_low = quantile(IP,0.05),
            ci_hi = quantile(IP, 0.95),
            n = n())

le_ip <- fsd_ip_lai |>
  filter(vza == vza_select) |>
  # filter(weighed_tree_canopy_load_mm <= 5) |>
  ggplot() +
  geom_point(aes(x = Le, y = IP, colour = wind_class), size = 0.5)+
  stat_cor(aes(x = Le, y = IP, label = paste(..rr.label..,
                             if_else(readr::parse_number(..p.label..) < 0.001,
                                     "p<0.001", ..p.label..), sep = "~`, `~"), colour = wind_class), label.y = c(1.17, 1.35) , geom = "label", show.legend = F) +
  geom_errorbar(data = le_ip_smry, aes(x = le_labs, ymax = sd_hi, ymin = sd_low), width = .2)  +
  geom_point(data = le_ip_smry, aes(x = le_labs, y = IP_avg), shape = 1, size = 4) +
  ylab(ip_y_ax_lab) +
  xlab(le_ax_lab) +
  scale_color_manual(name = legend_title, values = c(calm_colour, windy_colour)) +
  # scale_fill_viridis_c(option = 'magma')+
  # xlim(NA, 0) +
  ylim(0, 1.4)

le_ip

ggsave('figs/interception/lai_vs_ip.png', device = png, width = 6, height = 4.5)

le_ip <- le_ip + theme(legend.position = 'none', plot.margin = margin(0.5, 0.5, 0.5, .75, "cm"))

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
