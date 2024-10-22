library(quarto)

# Define the vertices of the triangle
vertices <- data.frame(
  x = c(1, 3, 3),
  y = c(3, 1, 3)
)

# Create a data frame for the labels
labels <- data.frame(
  x = 1,
  y = 3,
  label = expression(theta[h])
)

# Plot the triangle
ggplot() +
  geom_polygon(data = vertices, aes(x, y), fill = NA, colour = 'black') +
  annotate("text", x = 0.8, y = 3, label = expression(theta[h]), parse = TRUE) +
  xlim(0.5, 3.5) + ylim(0.5, 3.5) +
  annotate("text", x = 2, y = 3.2, label = expression(x[h])) +
  annotate("text", x = 3.2, y = 2, label = expression(v[h])) +
  # theme_minimal() +
  theme(axis.line = element_blank(),  # Remove axis lines
        axis.text = element_blank(),  # Remove axis text
        axis.title = element_blank(),  # Remove axis titles
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = 'white'),
        # panel.background = element_blank(),  # Remove panel background
        panel.grid.major = element_blank(),  # Remove major gridlines
        panel.grid.minor = element_blank(),  # Remove minor gridlines
        panel.border = element_blank(),  # Remove panel border
        plot.background = element_blank())  # Remove plot background

ggsave('figs/examples/hm_ta_triangle.png', device = png, width = 2, height = 2)
