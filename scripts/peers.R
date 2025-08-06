get_peers <- function(data, chosen_city = NULL, city_col = "City") {
  if (is.null(chosen_city)) stop("Please specify a city")
  
  my_city <- list()
  peers <- list()
  
  # Loop over all dimensionality reduction methods in data$reduced
  for (method in names(data$reduced)) {
    # Access the reduced data frame
    reduced_df <- data$reduced[[method]]$six_vars
    
    # Find the row for the selected city
    my_city$row[[method]] <- reduced_df %>% filter(.data[[city_col]] == chosen_city)
    
    # Extract cluster assignment
    my_city$cluster_assn[[method]] <- my_city$row[[method]] %>% pull(cluster)
    
    # Get all members in the same cluster
    my_city$cluster_members[[method]] <- reduced_df %>%
      filter(cluster == my_city$cluster_assn[[method]])
    
    # Get peers with dissimilarity
    peers[[method]] <- my_city$cluster_members[[method]] %>%
      add_distance_to_chosen(city_col = city_col, reference_city = chosen_city) %>%
      select({{ city_col }}, Dissimilarity) # was working?
      #select(dplyr::all_of(c(city_col, "Dissimilarity")))
  }
  
  return(peers)
}

find_shared_peers <- function(n, dim_reduction_methods, peers_list) {
  k <- n
  repeat {
    # Get top-k cities from each dimension reduction method
    top_cities_lists <- lapply(dim_reduction_methods, function(method) {
      head(dplyr::arrange(peers_list[[method]], Dissimilarity)$City, k)
    })
    
    # Find intersection across all methods
    shared_cities <- Reduce(intersect, top_cities_lists)
    
    if (length(shared_cities) >= n) {
      return(head(shared_cities, n))
    }
    
    # If not enough shared cities, increase k and try again
    k <- k + 1
    
    # Optional: add a safeguard to prevent infinite loops
    if (k > 100) stop("Could not find enough shared cities within 100 top entries.")
  }
}

add_distance_to_chosen <- function(df, city_col = "city", reference_city = "Somerville city, Massachusetts") {
  # Identify numeric columns
  numeric_cols <- df %>%
    select(where(is.numeric)) %>%
    colnames()
  
  # Extract reference row
  chosen_city_vec <- df %>%
    filter(!!sym(city_col) == reference_city) %>%
    select(all_of(numeric_cols)) %>%
    slice(1) %>%   # in case of duplicates
    unlist(use.names = FALSE)
  
  # Compute Euclidean distance to reference
  df <- df %>%
    rowwise() %>%
    mutate(
      Dissimilarity = sqrt(sum((c_across(all_of(numeric_cols)) - chosen_city_vec)^2))
    ) %>%
    ungroup()
  
  return(df)
}

write_peer_csvs <- function(methods){
  for (method in methods) {
    write_csv(peers[[method]] %>% arrange(Dissimilarity),
              paste0("./data/out/peers_", method, ".csv"))
  }
}

write_data_csvs <- function(methods){
  for (method in methods) {
    write_csv(data$reduced[[method]]$six_vars,
              paste0("./data/out/data_", method, ".csv"))
  }
}


get_similar_cities <- function(cities, chosen_city, weight) {
  # Input validation
  if (!"city" %in% colnames(cities)) {
    stop("The dataframe must contain a 'city' column.")
  }
  if (!chosen_city %in% cities$city) {
    stop("The chosen_city is not found in the 'city' column.")
  }
  if (weight %% 2 != 0) {
    stop("Weight must be an even number.")
  }
  
  # Get numeric variables (excluding 'city' column)
  numeric_vars <- sapply(cities, is.numeric)
  numeric_vars["city"] <- FALSE  # exclude city name column
  vars <- names(cities)[numeric_vars]
  
  # Initialize list to hold nearby cities for each variable
  close_city_lists <- list()
  
  for (var in vars) {
    # Sort the dataframe by the current variable
    sorted_df <- cities[order(cities[[var]]), ]
    
    # Find the position of the chosen city
    city_index <- which(sorted_df$city == chosen_city)
    
    # Get half the weight above and below
    half_weight <- weight / 2
    lower_index <- max(city_index - half_weight, 1)
    upper_index <- min(city_index + half_weight, nrow(sorted_df))
    
    # Get the indices above and below (excluding the chosen city itself)
    nearby_indices <- setdiff(seq(floor(lower_index), ceiling(upper_index)), city_index)
    close_cities <- sorted_df$city[nearby_indices]
    
    # Store in the list
    close_city_lists[[var]] <- close_cities
  }
  
  # Find intersection across all lists
  common_cities <- Reduce(intersect, close_city_lists)
  
  return(common_cities)
}

get_peers_wt <- function(cities, chosen_city, weight, ...) {
  # Parse variable-specific weight overrides
  var_weights <- list(...)
  
  if (!"city" %in% colnames(cities)) {
    stop("The dataframe must contain a 'city' column.")
  }
  if (!chosen_city %in% cities$city) {
    stop("The chosen_city is not found in the 'city' column.")
  }
  if (weight %% 2 != 0) {
    stop("Default weight must be an even number.")
  }
  
  # Identify numeric variables (excluding city column)
  numeric_vars <- names(Filter(is.numeric, cities))
  numeric_vars <- setdiff(numeric_vars, "city")
  
  # Initialize list to hold nearby cities for each variable
  close_city_lists <- list()
  
  for (var in numeric_vars) {
    # Determine weight to use for this variable
    var_weight <- if (!is.null(var_weights[[var]])) {
      as.integer(var_weights[[var]])
    } else {
      weight
    }
    
    if (var_weight %% 2 != 0) {
      stop(paste("Weight for", var, "must be even."))
    }
    
    half_weight <- var_weight / 2
    
    # Sort cities by the current variable
    sorted_df <- cities[order(cities[[var]]), ]
    city_index <- which(sorted_df$city == chosen_city)
    
    lower_index <- max(city_index - half_weight, 1)
    upper_index <- min(city_index + half_weight, nrow(sorted_df))
    
    nearby_indices <- setdiff(seq(floor(lower_index), ceiling(upper_index)), city_index)
    close_cities <- sorted_df$city[nearby_indices]
    
    close_city_lists[[var]] <- close_cities
  }
  
  # Return cities common to all lists
  common_cities <- Reduce(intersect, close_city_lists)
  return(common_cities)
}

get_peers_iterative <- function(cities, chosen_city, weight, ...) {
  var_weights <- list(...)
  
  if (!"city" %in% colnames(cities)) {
    stop("The dataframe must contain a 'city' column.")
  }
  if (!chosen_city %in% cities$city) {
    stop("The chosen_city is not found in the 'city' column.")
  }
  if (weight %% 2 != 0) {
    stop("Default weight must be an even number.")
  }
  
  numeric_vars <- names(Filter(is.numeric, cities))
  numeric_vars <- setdiff(numeric_vars, "city")
  close_city_lists <- list()
  
  for (var in numeric_vars) {
    var_weight <- if (!is.null(var_weights[[var]])) {
      as.integer(var_weights[[var]])
    } else {
      weight
    }
    
    if (var_weight %% 2 != 0) {
      stop(paste("Weight for", var, "must be even."))
    }
    
    target_value <- cities[cities$city == chosen_city, var]
    other_cities <- cities[cities$city != chosen_city, c("city", var)]
    
    # Initialize selected peers
    selected_peers <- character()
    
    for (i in 1:(var_weight / 2)) {
      # Filter out already selected peers
      remaining <- other_cities[!(other_cities$city %in% selected_peers), ]
      
      # Get closest city below
      below <- remaining[remaining[[var]] < target_value, ]
      if (nrow(below) > 0) {
        closest_below <- below[which.max(below[[var]]), "city"]
        selected_peers <- c(selected_peers, closest_below)
      }
      
      # Refresh remaining
      remaining <- other_cities[!(other_cities$city %in% selected_peers), ]
      
      # Get closest city above
      above <- remaining[remaining[[var]] > target_value, ]
      if (nrow(above) > 0) {
        closest_above <- above[which.min(above[[var]]), "city"]
        selected_peers <- c(selected_peers, closest_above)
      }
    }
    
    close_city_lists[[var]] <- selected_peers
  }
  
  common_cities <- Reduce(intersect, close_city_lists)
  return(common_cities)
}

combine_similarity_rankings <- function(peers_df_list, method_names = NULL, agg_method = "mean") {
  method_names <- names(peers_df_list)
  
  # Check structure of each peer df
  for (method in method_names) {
    df <- peers_df_list[[method]]
    if (!("City" %in% names(df)) || !("Dissimilarity" %in% names(df))) {
      stop(glue::glue("Each peers${method} must have columns 'City' and 'Dissimilarity'"))
    }
  }
  
  # Normalize each Distance column using z-score standardization (scale)
  norm_dfs <- map(method_names, function(method) {
    df <- peers_df_list[[method]]
    scaled_dist <- as.numeric(scale(df$Dissimilarity))  # scale returns matrix
    tibble(City = df$City, !!method := scaled_dist)
  })
  
  # Merge on City
  combined <- reduce(norm_dfs, full_join, by = "City")
  
  # Aggregate across methods
  combined <- combined %>%
    rowwise() %>%
    mutate(
      Aggregated = case_when(
        agg_method == "mean" ~ mean(c_across(all_of(method_names)), na.rm = TRUE),
        agg_method == "median" ~ median(c_across(all_of(method_names)), na.rm = TRUE),
        TRUE ~ NA_real_
      )
    ) %>%
    ungroup() %>%
    arrange(Aggregated)
  
  return(combined)
}
