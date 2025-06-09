
kmeans_set <- function(cities_scaled, k_range){
  map(k_range, function(k) {
    clustering_kmeans <- kmeans(cities_scaled, 
                                centers = k, 
                                nstart = 25)
  })
}

# This should not be doing the eval: a separate eval function should take
# a clustering in and provide/plot the eval_df with wss, silhoutte, ch, etc.
kmeans_elbow <- function(cities_scaled, k_range){
  kmeans_elbow_result <- map(k_range, function(k) {
    clustering_kmeans <- kmeans(cities_scaled, 
                                centers = k, 
                                nstart = 25)
    
    list(k = k, wss = clustering_kmeans$tot.withinss, model = clustering_kmeans)
  })
  
  # Convert to a tibble
  elbow_df <- tibble(
    k = map_int(kmeans_elbow_result, "k"),
    wss = map_dbl(kmeans_elbow_result, "wss"),
    model = map(kmeans_elbow_result, "model")
  )
  
  # Step 4: Plot elbow curve
  ggplot(elbow_df, aes(x = k, y = wss)) +
    geom_line() +
    geom_point() +
    labs(
      title = "Elbow Method for Optimal k",
      x = "Number of Clusters (k)",
      y = "Total Within-Cluster Sum of Squares"
    ) +
    theme_minimal()
}

kmeans_best_k <- function(cities_scaled, clustering_names, clusterings){
  if (length(clustering_names) != length(clusterings)) {
    stop("Length of clustering_names and clusterings must match.")
  }
  
  dist_matrix <- dist(cities_scaled)
  
  results_list <- lapply(seq_along(clusterings), function(i) {
    clustering <- clusterings[[i]]
    name <- clustering_names[[i]]
    
    # Get cluster assignments
    cluster_assignment <- clustering$cluster
    print(cluster_assignment)
    
    # Internal validation metrics
    km_tot_wcss <- clustering$tot.withinss
    km_bcss <- clustering$betweenss
    
    if (i != 1){
      cstats <- cluster.stats(dist_matrix, cluster_assignment)
      
      wcss <- cstats$within.cluster.ss
      
      # Silhouette metrics
      avg_sil <- cstats$avg.silwidth
      min_sil <- min(cstats$clus.avg.silwidths)
    }
    else {
      wcss <- NA
      avg_sil <- NA
      min_sil <- NA
    }
    
    
    
    
    # # For kmeans objects, extract WSS and BSS
    # tot_within <- if (!is.null(clustering$tot.withinss)) clustering$tot.withinss else NA
    # between <- if (!is.null(clustering$betweenss)) clustering$betweenss else NA
    
    # Return named list
    tibble(
      name = name,
      km_tot_wcss = km_tot_wcss,
      km_bcss = km_bcss,
      wcss = wcss,
      avg_sil = avg_sil,
      min_sil = min_sil,
      ch_index = cstats$ch
    )
  })
  
  bind_rows(results_list)
}

plot_k_eval_sil <- function(k_eval_df, sz = 3){
  ggplot(k_eval_df %>% rename(k = name)) +
    geom_point(aes(x = k, y = avg_sil, color = "Avg. silhouette width"), size = sz) +
    geom_point(aes(x = k, y = min_sil, color = "Min. silhouette width"), size = sz) +
    labs(title = "k-means Clustering Evaluation Metrics", x = "# Clusters (k)", y = "Evaluation Metric") +
    scale_color_discrete(name = "Evaluation Metric")
}

plot_k_eval2 <- function(k_eval_df, sz = 3){
  ggplot(k_eval_df %>% rename(k = name)) +
    geom_point(aes(x = k, y = log10(km_tot_wcss), color = "Total within-cluster SSE"), size = sz) +
    geom_point(aes(x = k, y = log10(ch_index), color = "CH index"), size = sz) +
    labs(title = "k-means Clustering Evaluation Metrics", x = "# Clusters (k)", y = "Evaluation Metric") +
    scale_color_discrete(name = "Evaluation Metric")
}

plot_k_eval <- function(k_eval_df, sz = 3){
  ggplot(k_eval_df %>% rename(k = name)) +
    geom_point(aes(x = k, y = log10(km_tot_wcss), color = "Total within-cluster SSE"), size = sz) +
    geom_point(aes(x = k, y = log10(avg_sil), color = "Avg. silhouette width"), size = sz) +
    geom_point(aes(x = k, y = log10(min_sil), color = "Min. silhouette width"), size = sz) +
    geom_point(aes(x = k, y = log10(ch_index), color = "CH index"), size = sz) +
    labs(title = "k-means Clustering Evaluation Metrics", x = "# Clusters (k)", y = "Evaluation Metric") +
    scale_color_discrete(name = "Evaluation Metric")
}


hcluster <- function(cities_scaled, k, method){
  dist_mtx <- get_dist(cities_scaled)
  hc <- hclust(dist_mtx, method = method)
  #plot(hc, labels = rownames(data), main = "Hierarchical Clustering Dendrogram")
  clusters <- cutree(hc, k = k)
}




#fviz_dist(dist)




# # View cluster assignments
# print(kmeans_result$cluster)
# 
# ggplot(ma_prepped, aes(x = median_home_valueE, y = occupancy_statusE)) +
#     geom_point(aes(color = as.factor(kmeans_result$cluster)), size = 3) +
#     labs(
#         title = "K-means Clustering of Massachusetts ACS Data",
#         x = "Median Home Value",
#         y = "Occupancy Status",
#         color = "Cluster"
#     ) +
#     theme_minimal()

