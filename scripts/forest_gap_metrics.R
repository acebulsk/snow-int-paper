library(dplyr)

# ideas for how to handle grid cells that have been clear cut partially and
# would not follow observed increase in lca with trajectory

# Coefficient of Variation as used to describe snow drifts @Winstral2014
natural <- rnorm(100, mean = 10, sd = 1)
hist(natural)
artificial <- c(rnorm(50, 10, sd = 1), rep(0, 50))
hist(artificial)

df <- data.frame(tree_height = c(natural, artificial),
                 group = c(rep('natural', 100), rep('artificial', 100)))

df |> group_by(group) |>
  summarise(sd = sd(tree_height),
            mean = mean(tree_height),
            cv = sd/mean)

# Relative Gap Area as in @Moeser2024

relative_gap_area <- (sum(total_gap_area)/pi())/(mean_gap_height/2)

# Other metrics are available in this study
# https://fireecology.springeropen.com/articles/10.1186/s42408-024-00279-7
