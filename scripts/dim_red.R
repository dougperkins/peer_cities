p_load(Rtsne, dplyr, ggplot2, factoextra)

get_pca_df <- function(pc, cities_scaled, city_names, clustering){
  pca_df <- tibble(
    pc1 = pc$x[,1],
    pc2 = pc$x[,2],
    pc3 = pc$x[,3],
    name = city_names,
    cluster = clustering$cluster
  )
}

