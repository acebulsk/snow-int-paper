library(dplyr)
library(ggplot2)

mcn <- 0:40
IP <- 0.15 * log(mcn) + 0.25

mcnip <- data.frame(mcn, IP)

ggplot(mcnip, aes(mcn, IP)) +
  geom_line()
 # TODO In progress, params above are roughly from lidar-processing analysis script voxrs/03
