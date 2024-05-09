# create a site map for the windswept subalpine forest site at fortress

library(tidyverse)
library(terra)
library(tidyterra)
library(sf)
library(tmap)

plot_name_dict <- data.frame(
  name = c('PWL_E', 'FSR_S'),
  plot_name = c('PWL Plot', 'FT Plot')
)

fsr_plots <- st_read('/home/alex/local-usask/analysis/lidar-processing/data/gis/shp/fsr_forest_plots_v_1_0.shp') |>
  filter(name %in%  c('PWL_E', 'FSR_S')) |>
  left_join(plot_name_dict)


lidr_flight_path <- st_read('/home/alex/local-usask/analysis/lidar-processing/data/metadata/drone_trajectory/shp/23_073_all_lidar_trajectory_lines.gpkg')

# detailed path
ss_transect_path <- st_read('/home/alex/local-usask/analysis/lidar-processing/data/gis/shp/23_73_transect.shp')
# rough path
ss_transect_path_rough <- st_read('/home/alex/local-usask/analysis/lidar-processing/data/gis/shp/fsr_snow_survey_transect_polyline.shp')

ss_coords <- sf::read_sf('../../analysis/interception/data/gis/gnss/avg_gnss_coords.gpkg') |>
  mutate(type = 'Snow Survey') |>
  select(type)

bad_names <- c('EC low',
               'SR50',
               'TB1',
               'TB2',
               'TB3',
               'TB4')

scl_name_dict <- data.frame(name = c('SCL 1', 'SCL 2', 'SCL 3'),
                            new_name = c('SCL Med', 'SCL Low', 'SCL High'))

inst_coords <- sf::read_sf('../../analysis/interception/data/lai/instrument_coords.gpkg') |>
  filter(!name %in% bad_names) |>
  st_transform(st_crs(ss_coords)) |>
  mutate(name = gsub('Trough', 'SCL', name)) |>
  left_join(scl_name_dict, by = 'name') |>
  select(name = new_name, type, geometry = geom)

stns <- data.frame(
  name = c('PWL Station', 'FT Station'),
  type = 'Flux Tower',
  x = c(626890.966, 627006.643),
  y = c(5632024.896, 5631995.019)) |>
  st_as_sf(coords = c("x", "y"), crs = st_crs(ss_coords)) |> rbind(inst_coords)

# st_write(stns, 'data/gis/stn_coords.gpkg')

bg <- terra::rast('/media/alex/phd-data/local-usask/analysis/lidar/gis/22_292FT_RGB_resamp_10cm.tif')

bg_resamp <- terra::rast('/media/alex/phd-data/local-usask/analysis/lidar/gis/22_292FT_RGB_resamp_25cm.tif')

bbox <- st_bbox(lidr_flight_path)
# bbox['xmin'] <- bbox['xmin'] - 50
# bbox['ymax'] <- bbox['ymax'] + 50
tm <- tm_shape(bg_resamp, bbox = bbox) +
  tm_rgb(colorNA = NULL)  +
  tm_graticules(
    ticks = TRUE,
    lines = FALSE,
    n.x = 2,
    n.y = 1
  ) +
  tm_shape(fsr_plots) +
  tm_polygons('plot_name', palette = c('salmon', 'dodgerblue'), title = '', alpha = .5) +
  tm_shape(lidr_flight_path) +
  tm_lines(col = 'blue', lty = 'dashed', lwd = 2, legend.col.show = T) +
  tm_shape(ss_transect_path_rough) +
  tm_lines(col = 'orange', lty = 'solid', lwd = 2) +
  tm_shape(stns) +
  tm_symbols(size = 0.5, col = 'name', palette = rev(palette("Okabe-Ito")[1:5]), title.col = '') +
  tm_scale_bar(position = c(0, 0)) +
  tm_compass(position = c(0, 0.1)) +
  tm_layout(
    legend.frame = 'black',
    legend.bg.color = 'antiquewhite',
    legend.position = c('right', 'top')
    # legend.outside = T
  ) +
  # tm_add_legend(type = 'symbol',
  #               shape = 24,
  #               col = palette("Okabe-Ito")[1:2],
  #               labels = stns$name)+
  tm_add_legend(type = 'line',
                lty = 'dashed',
                lwd = 2,
                col = "blue",
                labels = c("UAV Transect"))+
  tm_add_legend(type = 'line',
                col = 'orange',
                labels = c("In-situ Transect"))

# tm

tmap::tmap_save(tm, 'figs/maps/site_map.png', width = 6, height = 4, unit = 'in')
