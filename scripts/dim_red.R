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

grid_search_kpca <- function(data, 
                             degrees = 2:4, 
                             scales = c(1, 2, 5), 
                             offsets = c(1, 2, 5), 
                             n_components = 3, 
                             verbose = TRUE) {
  
  # Ensure numeric matrix input
  mat <- data %>% select(where(is.numeric)) %>% as.matrix()
  
  # Create parameter grid
  param_grid <- expand.grid(degree = degrees, scale = scales, offset = offsets)
  
  results <- param_grid %>% 
    pmap(function(degree, scale, offset) {
      tryCatch({
        model <- kpca(mat,
                      kernel = "polydot",
                      kpar = list(degree = degree, scale = scale, offset = offset))
        
        eigvals <- model@eig
        if (length(eigvals) == 0) stop("No eigenvalues returned.")
        
        # Filter small eigenvalues to avoid noise
        eigvals <- eigvals[eigvals > 1e-6]
        prop_var <- sum(eigvals[1:min(n_components, length(eigvals))]) / sum(eigvals)
        
        tibble(degree = degree, scale = scale, offset = offset,
               variance_explained = round(prop_var, 5),
               model = list(model))
        
      }, error = function(e) {
        if (verbose) message(glue("❌ Failed for degree={degree}, scale={scale}, offset={offset}: {e$message}"))
        NULL
      })
    }) %>% compact() %>% bind_rows()
  
  # Pick best params by variance explained
  best_row <- results %>% arrange(desc(variance_explained)) %>% slice(1)
  
  if (verbose && nrow(results) > 0) {
    cat("✅ Best parameters:\n")
    print(best_row %>% select(degree, scale, offset))
    cat(sprintf("✅ Variance explained by first %d PCs: %.4f\n", n_components, best_row$variance_explained))
  }
  
  list(
    best_model = if (nrow(results) > 0) best_row$model[[1]] else NULL,
    best_params = if (nrow(results) > 0) best_row %>% select(degree, scale, offset) else NULL,
    results_table = results %>% select(-model)
  )
}

# grid_search_kpca_rbf <- function(data, 
#                                  sigmas = c(0.01, 0.05, 0.1, 0.2, 0.5),
#                                  n_components = 3,
#                                  verbose = TRUE) {
#   
#   # Ensure numeric matrix input
#   mat <- data %>% select(where(is.numeric)) %>% as.matrix()
#   
#   results <- map_dfr(sigmas, function(sigma) {
#     tryCatch({
#       model <- kpca(mat,
#                     kernel = "rbfdot",
#                     kpar = list(sigma = sigma))
#       
#       eigvals <- model@eig
#       if (length(eigvals) == 0) stop("No eigenvalues returned.")
#       
#       # Filter small eigenvalues to avoid noise
#       eigvals <- eigvals[eigvals > 1e-6]
#       
#       prop_var <- sum(eigvals[1:min(n_components, length(eigvals))]) / sum(eigvals)
#       
#       tibble(sigma = sigma,
#              variance_explained = round(prop_var, 5),
#              model = list(model))
#       
#     }, error = function(e) {
#       if (verbose) message(glue("❌ Failed for sigma={sigma}: {e$message}"))
#       NULL
#     })
#   }) %>% drop_na()
#   
#   # Pick best params by variance explained
#   best_row <- results %>% arrange(desc(variance_explained)) %>% slice(1)
#   
#   if (verbose && nrow(results) > 0) {
#     cat("✅ Best sigma:\n")
#     print(best_row %>% select(sigma))
#     cat(sprintf("✅ Variance explained by first %d PCs: %.4f\n", 
#                 n_components, best_row$variance_explained))
#   }
#   
#   list(
#     best_model = if (nrow(results) > 0) best_row$model[[1]] else NULL,
#     best_sigma = if (nrow(results) > 0) best_row$sigma else NULL,
#     results_table = results %>% select(-model)
#   )
# }
