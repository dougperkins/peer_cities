# Check the pairwise distance distribution of the points to observe if there
# is multimodality, which suggests more than one cluster
distance_distr_check <- function(cities_scaled){
  dist_mtx <- get_dist(cities_scaled)
  dist_vector <- as.vector(dist_mtx)
  ggplot(tibble(distance = dist_vector), aes(x = distance)) +
    geom_density(fill = "lightblue", alpha = 0.5) +
    labs(title = "Pairwise Distance Density",
         x = "Distance",
         y = "Density") +
    theme_minimal()
}
