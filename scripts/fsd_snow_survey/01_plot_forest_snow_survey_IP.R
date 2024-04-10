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

source('scripts/fsd_snow_survey/00_load_snow_survey_data.R')
source('scripts/fsd_snow_survey/01_avg_met_fsd_periods.R')
source('scripts/fsd_snow_survey/02_fsd_ip_calc.R')

# snow survey data ----

# these FSD dates have suspect IP also filtered some in the .csv

outlier_events_fsd <- c(
  '2022-02-16', # very small event accumulation < 5 mm so instrument error probably too high here. but maybe keep FSD since had high precision measurements on crust
  '2022-05-09', # 3 days between surveys here in may with about 5 cm melt observed on the sr50 so erroneously high IP in open an should remove
  '2022-05-20' # very warm in the afternoon resulted in abnormally high IP vals
) |> as.Date()

fsd_all |> group_by(event_id) |>
  summarise(datetime = min(datetime), n = n())

fsd_all |> group_by(event_id) |>
  summarise(datetime = min(datetime), n = n()) |> pull(n) |> mean()

ggplot(fsd_all, aes(IP)) +
  geom_histogram() +
  facet_wrap(~event_id)


## ip vs met snow surveys ----
fsd_all_avg_event |>
  # filter(!event_id %in% outlier_events_fsd) |> # interpretation doesnt change here if we remove outliers but it does change the LAI / IP / wind calss plot below
  rename(
    `Air Temp. (°C)` = air_temp,
    `Relative Humidity (%)` = rh,
    `Wind Speed (m/s)` = med_u,
    `Snowfall (mm)` = del_sf,
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

ggsave('figs/snow_survey_periods/snow_survey_ip_w_met.png', device = png, width = 8.5, height = 4)


# work on plotting lai vs ip ----

# careful with windy vs not windy as evidence of inc IP w wind is based on 2022-04-06 which had substantial melt between surveys

lai_measurements <- read.csv('../../analysis/interception/data/lai/results/2023_compiled_lai_vza_15_60.csv')  |>
  select(-X)
lai_df <- read.csv('../../analysis/interception/data/lai/lai_site_id_2023_05_04.csv') |>
  mutate(id = paste0('DSCN6', img_id_short, '.JPG')) |>
  left_join(lai_measurements, by = 'id', relationship = "many-to-many") |>
  rename(num = old_num) |>
  mutate(vza_label = paste('Max Zenith Angle (deg.):' ,vza))

fsd_ip_lai <- left_join(fsd_all, lai_df, by = c('transect', 'num', 'canopy')) |>
  left_join(ffr_met_avg_event) |>
  # select(id, transect, num, canopy, cc, Le, IP, vza) |>
  filter(is.na(cc) == F,
         !event_id %in% outlier_events_fsd)

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

ggsave('figs/snow_survey_periods/forest_ip.png', device = png, width = 9.5, height = 4)

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

ggsave('figs/snow_survey_periods/lai_vs_ip.png', device = png, width = 6, height = 4.5)

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
