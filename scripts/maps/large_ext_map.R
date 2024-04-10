# make large extent map

library(sf)
library(tidyverse)
library(tmap)
library(ggrepel)

bg <- read_sf('data/gis/provincial_boundary/lpr_000b16a_e.shp') %>% filter(
  PRENAME %in% c('British Columbia', 'Yukon', 'Alberta', 'Northwest Territories', 'Saskatchewan')
)

target_crs <- st_crs(bg)

sites <- data.frame(
  site = c('Wolf Creek', 'Russell Creek', 'Fortress Mountain', 'Marmot Creek'),
  lon = c(-134.95283, -126.3090,-115.1983, -115.155154),
  lat = c(60.596, 50.3710, 50.8269, 50.9255877),
  ele = c(750, 1000, 2100, 1850)
)

sites_sf <- sf::st_as_sf(sites, coords = c('lon', 'lat'), crs = 4326)

sf::write_sf(sites_sf, 'data/gis/site_coords.gpkg')
sf::write_sf(sites_sf, 'data/gis/site_coords.kml')
write.csv(sites, 'data/gis/site_coords.csv', row.names = F)

filter_box <- st_bbox(st_buffer(sites_sf, dist = 55000)) |>
  st_as_sfc()

# canada <- spData::world %>% dplyr::filter(name_long == "Canada")

disp_win_wgs84 <- st_sfc(st_point(c(-130, 43)), st_point(c(-122, 67)),
                         crs = 4326)
disp_win_trans <- st_transform(disp_win_wgs84, crs = target_crs)

disp_win_coord <- st_coordinates(disp_win_trans)

png('figs/site-map-ecozone.jpg', width=6, height=5, units="in", res=300)

sites_sf <- sites_sf |> filter(site %in% c('Fortress Mountain', 'Marmot Creek'))
ggplot(data = bg) +
  geom_sf() +
  geom_sf(data = sites_sf) +
  # coord_sf(xlim = c(bb$xmin, bb$xmax), ylim = c(bb$ymin, bb$ymax)) +
  # coord_sf(xlim = c(-140, -100), ylim = c(0, 20000)) +
  coord_sf(xlim = disp_win_coord[,'X'], ylim = disp_win_coord[,'Y'],
           datum = 4326, expand = FALSE) +
  theme_bw() +
  geom_label_repel(
    data = sites_sf,
    aes(geometry = geometry, label = site, colour = site),
    stat = "sf_coordinates",
    box.padding = unit(2, "lines")
  ) +
  scale_color_manual(values = c('red', 'black')) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = 'none')

# dev.off()

# tmap::tm_shape(bg, bbox = bb) +
#   tm_fill() +
#   tm_borders(col = 'grey') +
#   tm_shape(ecozones) +
#   tm_fill(col = 'ZONE_NAME') +
#   tm_shape(sites_sf) +
#   tm_dots(col = 'red', size = 0.6) +
#   tm_text("site", size = 2, auto.placement = F, just = 'bottom') +
#   tm_graticules(n.x = 3, n.y = 4)

# tmap::tmap_save(map, 'figs/site_map.jpg')

ggsave('figs/maps/site_map_bc_ab_scale.png', device = png,  width = 5, height = 4)
