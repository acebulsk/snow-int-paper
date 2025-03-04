# script that plots the examples shown in the theory section
theory_colours <- c("#30123BFF", "#1AE4B6FF", "#FABA39FF", "#7A0403FF") # viridis::turbo(4)

ip_example <- readRDS('../../analysis/interception/data/interception_modelling/model_example_steady_temperature.rds')

ip_example |>
  pivot_longer(c(HP98, SA09, M15)) |>
  ggplot(aes(precip, value/precip, colour = name, linetype = canopy_coverage)) +
  geom_line() +
  xlab('Event Snowfall (mm)')+
  ylab('Event Interception Efficiency (-)')  +
  xlim(c(0, 51)) +
  ylim(c(0, 0.8)) +
  scale_color_manual(values = theory_colours, name = 'Model') +
  scale_linetype_discrete(name = 'Canopy \nCoverage')

ggsave('figs/examples/example_ip_hp98_sa09_M15.png', device = png, width = fig_width, height = fig_height, units = "in")

# canopy storage capacity ----

S_bar <- 5.9 # species correction for spruce (kg / m2)
LAI <- 3 # example leaf area index (-)

w_max_df <- data.frame(air_temp = seq(-10,2, by = 0.5))
w_max_df$fsd <- 67.92 + 51.25 * exp(w_max_df$air_temp / 2.59)
w_max_df$species_max <- S_bar * (0.27 + (46/w_max_df$fsd))
w_max_df$wmax <- w_max_df$species_max * LAI

w_max_df_hp98 <- w_max_df
w_max_df_hp98 $group <- 'HP98'

# andreadis2009 calc

andreadis_step <- function(air_temp){
  if (air_temp > -1) {
    L_r <- 4.0
  }
  else if(air_temp <= -1 & air_temp > -3){
    L_r <- 1.5*air_temp+5.5
  }
  else if(air_temp<=-3){
    L_r = 1.0
  }
}

w_max_df_sa09 <- w_max_df
L_r <- mapply(andreadis_step, w_max_df$air_temp)
w_max_df_sa09$wmax <- L_r * LAI
w_max_df_sa09$group <- 'SA09'

w_bind <- rbind(w_max_df_hp98, w_max_df_sa09)

saveRDS(w_bind, 'data/example-data/w_bind.rds')

w_bind <- readRDS('data/example-data/w_bind.rds')

ggplot(w_bind, aes(air_temp, wmax, colour = group)) +
  geom_line() +
  ylab('Interception Capacity (mm)') +
  xlab('Air Temperature (°C)') +
  scale_colour_manual(values = c(theory_colours[1], theory_colours[3])) +
  labs(colour = "Model")

ggsave('figs/examples/w_max_example_sa09_hp98.png', device = png, width = fig_width, height = fig_height)

# Snow leaf contact area ----

# HP98 snow-leaf contact area
leaf_covered_area <- function(horizontal_wind,
                              canopy_closure,
                              canopy_height,
                              forest_downwind,
                              fall_velocity) {
  leaf_plane_area_out <-
    canopy_closure/(1-(canopy_closure*horizontal_wind*canopy_height)/
                      (fall_velocity*forest_downwind))

  return(min(c(1, leaf_plane_area_out)))
}

timestep <- 2 # hours
lai <- 3.5
canopy_closure <- 0.29*log(lai)+0.55 # calculate canopy coverage as in CRHM
canopy_height <- 10 # in metres
forest_downwind <- 100 # in metres, for the canopy leaf plane area calculation
fall_velocity <- 0.8 # metres per second, for the canopy leaf plane area calc
c <- 0.678 # (-) unloading coefficient for ~ weekly application from hp98

# Cionco canopy wind flow ----

#' Cionco Canopy Wind Flow
#'
#' @param u horizontal wind speed at the top of the canopy
#' @param a an attenuation coefficient that increases with leaf area and
#'   decreases with distance between trees (Cionco 1972) suggest values of 1.01
#'   and Parviainen & Pomeroy 2000 provide a method to calculate this from their
#'   empirical measurements.
#' @param z_c height above ground of the returned wind speed
#' @param h_c average height of the canopy elements
#'
#' @return
#' @export
#'
#' @examples
cionco_canopy_wind_flow <- function(u, a, z_c, h_c){
  u * exp(a * ((z_c/h_c) - 1))
}

parv_cionco_alpha <- function(n, m, u){
  a <- n + m * exp(-u)
  return(a)
}

# mature site pars from parvi
# n = 2.43
# m = 3.46

# regenerating site pars from parvi
# n = 2.97
# m = 3.2

df_cionco <- data.frame(
  z_c = seq(0, 4.2, by = 0.1),
  u = 1.37,
  a = 1.01,
  a_parv = NA,
  h_c = 4.2,
  u_c = NA)

for (i in 1:nrow(df_cionco)) {
  df_cionco$a_parv <- parv_cionco_alpha(n, m, df_cionco$u[i])
  df_cionco$u_c[i] <-
    cionco_canopy_wind_flow(df_cionco$u[i],
                            df_cionco$a[i],
                            df_cionco$z_c[i],
                            df_cionco$h_c[i])
  df_cionco$u_c_parv[i] <-
    cionco_canopy_wind_flow(df_cionco$u[i],
                            df_cionco$a_parv[i],
                            df_cionco$z_c[i],
                            df_cionco$h_c[i])
}

df_cionco |> pivot_longer(c(u_c, u_c_parv)) |>
ggplot(aes(z_c, value, colour = name)) +
  geom_line() +
  ylab('Wind Speed (m/s)') +
  xlab('Height Above Ground (m)')
plotly::ggplotly()
df_cionco |>
  ggplot(aes(z_c, u_c_parv)) +
  geom_line() +
  ylab('Wind Speed (m/s)') +
  xlab('Height Above Ground (m)')

ggsave('figs/examples/height_above_ground_vs_cionco_parv_mod_wind_speed.png', height = 3, width = 3)
