# example of kmeans and how to adjust plotting
# from: https://www.datanovia.com/en/blog/k-means-clustering-visualization-in-r-step-by-step-guide/
library(ggpubr)
library(factoextra)

n_k <- 4 # based on scree plot below

names(event_df)
ip_df <- event_df |>
  left_join(parsivel) |>
  ungroup() |>
  select(#t,
         # rh,
         #u,
         part_vel,
         part_diam,
         # Qsi,
         #cuml_int_tree,
         IP_tree,
         IP_troughs) |>
  filter(IP_tree >= 0,
         IP_tree <= 1,
         IP_troughs >= 0,
         IP_troughs <= 1,
         is.na(part_diam) == F)


ip_df_tree <- ip_df |> select(-IP_troughs)

# Decide how many clusters to look at
n_clusters <- 10

# Initialize total within sum of squares error: wss
wss <- numeric(n_clusters)

set.seed(123)

# Look over 1 to n possible clusters
for (i in 1:n_clusters) {
  # Fit the model: km.out
  km.out <- kmeans(ip_df_tree, centers = i, nstart = 20)
  # Save the within cluster sum of squares
  wss[i] <- km.out$tot.withinss
}

# Produce a scree plot
wss_df <- tibble(clusters = 1:n_clusters, wss = wss)

scree_plot <- ggplot(wss_df, aes(x = clusters, y = wss, group = 1)) +
  geom_point(size = 4)+
  geom_line() +
  scale_x_continuous(breaks = c(2, 4, 6, 8, 10)) +
  xlab('Number of clusters')
scree_plot

# Compute k-means with k = 3
set.seed(123)
res.km <- kmeans(scale(ip_df_tree), n_k, nstart = 25)
res.km
# K-means clusters showing the group of each individuals
res.km$cluster

fviz_cluster(res.km, data = ip_df_tree,
             palette = palette.colors(palette = "R4"),
             geom = "point",
             ellipse.type = "convex",
             ggtheme = theme_bw()
)

# visualize

ip_df_tree$cluster_id <- factor(res.km$cluster)

ggplot(ip_df_tree, aes(t, IP_tree, colour = cluster_id)) +
  geom_point()

ggplot(ip_df_tree, aes(u, IP_tree, colour = cluster_id)) +
  geom_point()

ggplot(ip_df_tree, aes(cuml_int_tree, IP_tree, colour = cluster_id)) +
  geom_point()

ggplot(ip_df_tree, aes(part_vel, IP_tree, colour = cluster_id)) +
  geom_point()

ggplot(ip_df_tree, aes(part_diam, IP_tree, colour = cluster_id)) +
  geom_point()

# to make changes to the above plot we need to use PCA... and ggscatter

# Dimension reduction using PCA
res.pca <- prcomp(ip_df[, -5],  scale = TRUE)
# Coordinates of individuals
ind.coord <- as.data.frame(get_pca_ind(res.pca)$coord)
# Add clusters obtained using the K-means algorithm
ind.coord$cluster <- factor(res.km$cluster)
# Add Species groups from the original data sett
ind.coord$Species <- ip_df$Species
# Data inspection
head(ind.coord)

# Percentage of variance explained by dimensions
eigenvalue <- round(get_eigenvalue(res.pca), 1)
variance.percent <- eigenvalue$variance.percent
head(eigenvalue)

ggscatter(
  ind.coord, x = "Dim.1", y = "Dim.2",
  color = "cluster", palette = "npg", ellipse = TRUE, ellipse.type = "convex",
  shape = "Species", size = 1.5,  legend = "right", ggtheme = theme_bw(),
  xlab = paste0("Dim 1 (", variance.percent[1], "% )" ),
  ylab = paste0("Dim 2 (", variance.percent[2], "% )" )
) +
  stat_mean(aes(color = cluster), size = 4)
