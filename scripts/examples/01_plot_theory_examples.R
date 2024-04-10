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
