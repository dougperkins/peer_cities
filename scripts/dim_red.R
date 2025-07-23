p_load(Rtsne, dplyr, ggplot2, factoextra)

reduce_dimensions <- function(dr_techniques, 
                              data_list, 
                              n_comp = 2,
                              city_names,
                              
                              kpca_poly_degrees = 2:4,
                              kpca_poly_scales = c(1, 2, 5),
                              kpca_poly_offsets = c(1, 2, 5),
                              
                              umap_n_neighbors = c(5, 15, 30),
                              umap_min_dist = c(0.01, 0.1, 0.5),
                              umap_metrics = c("euclidean")) {
  
  dr_methods <- list(
    pca = get_pca_df_nc,
    kpca_rbf = get_kpca_df_nc,
    kpca_poly = grid_search_kpca,
    umap = grid_search_umap
    # Add other methods here:
    # tsne = get_tsne_df_nc,
    # umap = get_umap_df_nc
  )
  
  # Initialize the 'reduced' section of the data_list if it doesn't exist
  if (is.null(data_list$reduced)) {
    data_list$reduced <- list()
  }
  
  # Validate requested techniques
  invalid_techniques <- setdiff(dr_techniques, names(dr_methods))
  if (length(invalid_techniques) > 0) {
    stop(paste("Invalid dimension reduction techniques requested:",
               paste(invalid_techniques, collapse = ", "),
               ". Available techniques are:", paste(names(dr_methods), collapse = ", ")))
  }
  
  message("Starting dimension reduction process...")
  
  # Iterate through each specified dimension reduction technique
  for (dr_name in dr_techniques) {
    dr_func <- dr_methods[[dr_name]] # Get the function for the current technique
    
    message(paste0("Applying '", dr_name, "' to subsets..."))
    
    # Initialize the sub-list for this technique if it doesn't exist
    if (is.null(data_list$reduced[[dr_name]])) {
      data_list$reduced[[dr_name]] <- list()
    }
    
    # Iterate through each subset dataframe in data_list$preprocessed
    if (!is.null(data_list$preprocessed) && length(data_list$preprocessed) > 0) {
      for (subset_name in names(data_list$preprocessed)) {
        subset_df <- data_list$preprocessed[[subset_name]]
        
        if (!is.data.frame(subset_df) & dr_name %in% c("none")) {
          warning(paste("Skipping", subset_name, "for", dr_name, "as it is not a dataframe."))
          next
        }
        
        # Apply the dimension reduction function
        # Special handling for kpca_poly (grid search)
        if (dr_name == "pca"){
          message("Running PCA")
          reduced_df <- dr_func(prcomp(subset_df, scale. = TRUE),
                                city_names,
                                n_comp = 10,
                                city_choice)
        } else if (dr_name == "kpca_rbf"){
          message("Running kPCA RBF")
          reduced_df <- dr_func(rotated(kpca(~., data = as.data.frame(subset_df), kernel = "rbfdot")),
                                city_names,
                                n_comp)
        } else if (dr_name == "kpca_poly") {
          message("Running kPCA Polynomial")
          reduced_result <- dr_func(
            subset_df,
            n_comp = n_comp,
            degrees = kpca_poly_degrees,
            scales = kpca_poly_scales,
            offsets = kpca_poly_offsets,
            city_names = city_names,
            city_choice = city_choice,
            verbose = TRUE
          )
          reduced_df <- reduced_result$best_model
          #reduced_df <- tibble::as_tibble(rotated(reduced_result$best_model))
        } else if (dr_name == "umap") {
          message("Running UMAP (grid search)")
          reduced_result <- dr_func(
            subset_df,
            n_neighbors = umap_n_neighbors,
            min_dist = umap_min_dist,
            metrics = umap_metrics,
            n_comp = n_comp,
            city_names = city_names,
            city_choice = city_choice,
            verbose = TRUE
          )
          reduced_df <- reduced_result$best_model
        } else {
          reduced_df <- dr_func(subset_df, 
                                city_names = city_names,
                                n_comp = n_comp)
        }
        
        # Store the result in the specified structure
        data_list$reduced[[dr_name]][[subset_name]] <- reduced_df
      }
    } else {
      message("No dataframes found in data_list$preprocessed to process.")
    }
  }
  
  message("Dimension reduction process completed.")
  return(data_list)
}



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

get_pca_df_nc <- function(data, city_names, n_comp = 3, city_choice = NULL) {
  # Get the first `n_comp` principal components
  pcs <- as.data.frame(data$x[, 1:n_comp])
  
  # Rename columns to "c1", "c2", etc.
  colnames(pcs) <- paste0("C", 1:n_comp)
  
  # Create tibble with PCs, names, and clusters
  pca_df <- tibble::tibble(
    pcs,
    name = city_names,
    highlight = if (!is.null(city_choice)) {
      ifelse(city_names == city_choice, "highlight", "normal")
    } else {
      "normal"
    }
  )
  
  pca_df %<>% 
    rename(City = name) %>%
    select(City, everything())
  
  return(pca_df)
}

get_kpca_df_nc <- function(data, city_names, n_comp = 3, city_choice = NULL) {
  # Convert KPCA rotated matrix to data frame
  kpca_df <- as_tibble(data[, 1:n_comp])
  
  # Rename KPCA columns to kp1, kp2, kp3, etc.
  colnames(kpca_df) <- paste0("C", 1:n_comp)
  
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
  
  kpca_df %<>% 
    rename(City = name) %>% 
    #rename_with(.fn = ~ gsub("^kp", "C", .x), .cols = starts_with("kp")) %>% 
    select(City, everything())
  
  return(kpca_df)
}

grid_search_kpca <- function(data, 
                             degrees = 2:4, 
                             scales = c(1, 2, 5), 
                             offsets = c(1, 2, 5), 
                             n_comp = 3, 
                             city_names = NULL, 
                             city_choice = NULL,
                             verbose = TRUE) {
  library(tibble)
  library(dplyr)
  library(purrr)
  library(glue)
  library(kernlab)
  library(magrittr)
  
  data %<>% as_tibble()
  
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
        
        eigvals <- eigvals[eigvals > 1e-6]  # Filter noise
        prop_var <- sum(eigvals[1:min(n_comp, length(eigvals))]) / sum(eigvals)
        
        tibble(degree = degree,
               scale = scale,
               offset = offset,
               variance_explained = round(prop_var, 5),
               model = list(model))
        
      }, error = function(e) {
        if (verbose) message(glue("❌ Failed for degree={degree}, scale={scale}, offset={offset}: {e$message}"))
        NULL
      })
    }) %>%
    compact() %>%
    bind_rows()
  
  if (nrow(results) == 0) {
    warning("No successful kpca model found.")
    return(list(
      best_model = NULL,
      best_params = NULL,
      results_table = tibble()
    ))
  }
  
  # Pick best model
  best_row <- results %>% arrange(desc(variance_explained)) %>% slice(1)
  best_model <- best_row$model[[1]]
  
  # Create rotated matrix
  best_rotated <- as_tibble(rotated(best_model)[, 1:n_comp])
  colnames(best_rotated) <- paste0("C", 1:n_comp)
  
  # Add city info if provided
  if (!is.null(city_names)) {
    if (length(city_names) != nrow(best_rotated)) {
      warning("Length of city_names does not match number of rows in rotated matrix.")
      city_names <- rep(NA, nrow(best_rotated))
    }
    
    best_rotated <- best_rotated %>%
      mutate(
        City = city_names,
        highlight = if (!is.null(city_choice)) {
          ifelse(city_names == city_choice, "highlight", "normal")
        } else {
          "normal"
        }
      ) %>%
      select(City, everything())
  }
  
  if (verbose) {
    cat("✅ Best parameters:\n")
    print(best_row %>% select(degree, scale, offset))
    cat(sprintf("✅ Variance explained by first %d PCs: %.4f\n", n_comp, best_row$variance_explained))
  }
  
  list(
    best_model = best_rotated,
    best_params = best_row %>% select(degree, scale, offset),
    results_table = results %>% select(-model)
  )
}

grid_search_umap <- function(data,
                             n_neighbors = c(5, 15, 30),
                             min_dist = c(0.01, 0.1, 0.5),
                             metrics = c("euclidean"),
                             n_comp = 2,
                             city_names = NULL,
                             city_choice = NULL,
                             verbose = TRUE) {
  
  data <- as_tibble(data)
  mat <- data %>% select(where(is.numeric)) %>% as.matrix()
  
  param_grid <- expand.grid(n_neighbors = n_neighbors, min_dist = min_dist, metric = metrics, stringsAsFactors = FALSE)
  
  results <- param_grid %>%
    pmap(function(n_neighbors, min_dist, metric) {
      tryCatch({
        embedding <- uwot::umap(
          mat,
          n_neighbors = n_neighbors,
          min_dist = min_dist,
          metric = metric,
          n_components = n_comp,
          verbose = FALSE,
          ret_model = FALSE
        )
        
        reduced <- as_tibble(embedding, .name_repair = "unique")
        colnames(reduced) <- paste0("C", seq_len(n_comp))
        
        compactness <- mean(dist(reduced))
        
        tibble(
          n_neighbors = n_neighbors,
          min_dist = min_dist,
          metric = metric,
          compactness = compactness,
          embedding = list(reduced)
        )
      }, error = function(e) {
        if (verbose) message(glue("❌ Failed for n_neighbors={n_neighbors}, min_dist={min_dist}, metric={metric}: {e$message}"))
        NULL
      })
    }) %>%
    compact() %>%
    bind_rows()
  
  if (nrow(results) == 0) {
    warning("No successful UMAP model found.")
    return(list(
      best_model = NULL,
      best_params = NULL,
      results_table = tibble()
    ))
  }
  
  best_row <- results %>% arrange(compactness) %>% slice(1)
  best_embedding <- best_row$embedding[[1]]
  
  if (!is.null(city_names)) {
    if (length(city_names) != nrow(best_embedding)) {
      warning("Length of city_names does not match number of rows in reduced matrix.")
      city_names <- rep(NA, nrow(best_embedding))
    }
    
    best_embedding <- best_embedding %>%
      mutate(
        City = city_names,
        highlight = if (!is.null(city_choice)) {
          ifelse(city_names == city_choice, "highlight", "normal")
        } else {
          "normal"
        }
      ) %>%
      select(City, everything())
  }
  
  if (verbose) {
    cat("✅ Best parameters:\n")
    print(best_row %>% select(n_neighbors, min_dist, metric))
    cat(sprintf("✅ Compactness (avg pairwise dist): %.4f\n", best_row$compactness))
  }
  
  list(
    best_model = best_embedding,
    best_params = best_row %>% select(n_neighbors, min_dist, metric),
    results_table = results %>% select(-embedding)
  )
}


# grid_search_kpca <- function(data, 
#                              degrees = 2:4, 
#                              scales = c(1, 2, 5), 
#                              offsets = c(1, 2, 5), 
#                              n_comp = 3, 
#                              verbose = TRUE) {
#   
#   data %<>% as.tibble(data)
#   
#   # Ensure numeric matrix input
#   mat <- data %>% select(where(is.numeric)) %>% as.matrix()
#   
#   # Create parameter grid
#   param_grid <- expand.grid(degree = degrees, scale = scales, offset = offsets)
#   
#   results <- param_grid %>% 
#     pmap(function(degree, scale, offset) {
#       tryCatch({
#         model <- kpca(mat,
#                       kernel = "polydot",
#                       kpar = list(degree = degree, scale = scale, offset = offset))
#         
#         eigvals <- model@eig
#         if (length(eigvals) == 0) stop("No eigenvalues returned.")
#         
#         # Filter small eigenvalues to avoid noise
#         eigvals <- eigvals[eigvals > 1e-6]
#         prop_var <- sum(eigvals[1:min(n_comp, length(eigvals))]) / sum(eigvals)
#         
#         tibble(degree = degree, scale = scale, offset = offset,
#                variance_explained = round(prop_var, 5),
#                model = list(model))
#         
#       }, error = function(e) {
#         if (verbose) message(glue("❌ Failed for degree={degree}, scale={scale}, offset={offset}: {e$message}"))
#         NULL
#       })
#     }) %>% compact() %>% bind_rows()
#   
#   # Pick best params by variance explained
#   best_row <- results %>% arrange(desc(variance_explained)) %>% slice(1)
#   
#   if (verbose && nrow(results) > 0) {
#     cat("✅ Best parameters:\n")
#     print(best_row %>% select(degree, scale, offset))
#     cat(sprintf("✅ Variance explained by first %d PCs: %.4f\n", n_comp, best_row$variance_explained))
#   }
#   
#   list(
#     best_model = if (nrow(results) > 0) as.tibble(rotated(best_row$model[[1]])) %>% rename_with(.fn = ~ gsub("^V", "C", .x), .cols = starts_with("V")) else NULL,
#     best_params = if (nrow(results) > 0) best_row %>% select(degree, scale, offset) else NULL,
#     results_table = results %>% select(-model)
#   )
# }

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
