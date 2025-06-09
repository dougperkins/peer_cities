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

# If most variance is captured in a few dimensions, there may be natural groupings?
# If spread across all components, may be more homogenous

get_pca_df_nc <- function(pc, cities_scaled, city_names, n_pcs = 3) {
  # Get the first `n_pcs` principal components
  pcs <- as.data.frame(pc$x[, 1:n_pcs])
  
  # Rename columns to "pc1", "pc2", etc.
  colnames(pcs) <- paste0("pc", 1:n_pcs)
  
  # Create tibble with PCs, names, and clusters
  pca_df <- tibble::tibble(
    pcs,
    name = city_names
  )
  
  return(pca_df)
}

get_kpca_df_nc <- function(kpca_rotated, city_names, n_kpcs = 3, city_choice = NULL) {
  # Convert KPCA rotated matrix to data frame
  kpca_df <- as_tibble(kpca_rotated[, 1:n_kpcs])
  
  # Rename KPCA columns to kp1, kp2, kp3, etc.
  colnames(kpca_df) <- paste0("kp", 1:n_kpcs)
  
  # Add metadata columns
  kpca_df <- kpca_df %>%
    mutate(
      name = city_names,
      highlight = if (!is.null(city_choice)) {
        ifelse(city_names == city_choice, "highlight", "normal")
      } else {
        "normal"
      }
    )
  
  return(kpca_df)
}
