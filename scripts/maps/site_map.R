# create a site map for the windswept subalpine forest site at fortress

library(tidyverse)
library(terra)
library(tidyterra)
library(sf)
library(tmap)

fsr_plots <- st_read('/home/alex/local-usask/analysis/lidar-processing/data/gis/shp/fsr_forest_plots_v_2_0.shp')

lidr_flight_path <- st_read('/home/alex/local-usask/analysis/lidar-processing/data/metadata/drone_trajectory/shp/23_073_all_lidar_trajectory_lines.gpkg')

ss_transect_path <- st_read('/home/alex/local-usask/analysis/lidar-processing/data/gis/shp/23_73_transect.shp')

ss_coords <- sf::read_sf('../../analysis/interception/data/gis/gnss/avg_gnss_coords.gpkg') |>
  mutate(type = 'Snow Survey') |>
  select(type)

stns <- data.frame(
  name = c('Powerline', 'Forest Tower'),
  type = 'Flux Tower',
  x = c(626890.966, 627006.643),
  y = c(5632024.896, 5631995.019)) |>
  st_as_sf(coords = c("x", "y"), crs = st_crs(ss_coords))

bg <- terra::rast('/media/alex/phd-data/local-usask/analysis/lidar/gis/22_292FT_RGB.tif')

# Resample background image  ----

# bbox <- terra::ext(bg)
#
# # construct raster so cells match up with centre of dots
# template_rast <- terra::rast(
#   resolution = 0.25,
#   xmin = bbox$xmin,
#   xmax = bbox$xmax,
#   ymin = bbox$ymin,
#   ymax = bbox$ymax,
#   vals = NA_real_,
#   crs = "epsg:32611"
# )
#
# # take the median of the cells w/in out coarser template
# bg_resamp <-
#   terra::resample(bg, template_rast)
#
# bg_resamp <- terra::ifel(bg_resamp == 0, NA, bg_resamp)
#
# terra::writeRaster(
#   bg_resamp,
#   '/media/alex/phd-data/local-usask/analysis/lidar/gis/22_292FT_RGB_resamp_25cm.tif',
#   overwrite = T
# )

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
  tm_borders(col = 'red', lwd = 2) +
  tm_shape(lidr_flight_path) +
  tm_lines(col = 'blue', lty = 'dashed', lwd = 2, legend.col.show = T) +
  # tm_shape(ss_transect_path) +
  # tm_lines(col = 'orange', lty = 'dashed', lwd = 2) +
  tm_shape(stns) +
  tm_symbols(size = 0.5, col = 'name', palette = palette("Okabe-Ito"), title.col = '') +
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
                labels = c("RPAS Transect"))+
  tm_add_legend(type = 'line',
                col = 'red',
                labels = c("Forest Plots"))

# tm

tmap::tmap_save(tm, 'figs/maps/site_map.png', width = 6, height = 4, unit = 'in')
