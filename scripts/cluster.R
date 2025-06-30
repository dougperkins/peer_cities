
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

# get_best_k <- function(data, distance = "euclidean", min_nc = 2, 
#                        max_nc = 10, method = "ward.D2", index = "all") {
#   
#   nb <- NbClust(data,
#                 distance = distance,
#                 min.nc = min_nc,
#                 max.nc = max_nc,
#                 method = method,
#                 index = index)
#   
#   best_k <- as.numeric(names(which.max(table(nb$Best.nc[1, ]))))
#   
#   best_k
# }

# get_best_k_multi_index <- function(data, indices,
#                                        distance = "euclidean",
#                                        min.nc = 2, max.nc = 10,
#                                        method = "kmeans") {
#   best_ks <- numeric(length(indices))
#   names(best_ks) <- indices
#   
#   for (i in seq_along(indices)) {
#     idx <- indices[i]
#     message("Running NbClust with index: ", idx)
#     res <- tryCatch({
#       NbClust(data, distance = distance, min.nc = min.nc, max.nc = max.nc,
#               method = method, index = idx)
#     }, error = function(e) {
#       warning("Failed for index ", idx, ": ", e$message)
#       return(NULL)
#     })
#     
#     if (!is.null(res$Best.nc) && length(res$Best.nc) >= 1 && !is.na(res$Best.nc[1]) && res$Best.nc[1] != "") {
#       best_ks[i] <- as.numeric(res$Best.nc[1])
#     } else {
#       best_ks[i] <- NA  # or some other flag value
#       warning("Index '", idx, "' did not return a valid Best.nc[1]. Set to NA.")
#     }
#   }
#   
#   return(best_ks)
# }

get_best_k_multi_index <- function(data, indices,
                                   distance = "euclidean",
                                   min_nc = 2, max_nc = 10,
                                   method = "kmeans") {
  best_ks <- numeric(length(indices))
  names(best_ks) <- indices
  
  for (i in seq_along(indices)) {
    idx <- indices[i]
    message("Running NbClust with index: ", idx)
    res <- tryCatch({
      NbClust(data, distance = distance, min.nc = min_nc, max.nc = max_nc,
              method = method, index = idx)
    }, error = function(e) {
      warning("Failed for index ", idx, ": ", e$message)
      return(NULL)
    })
    
    if (!is.null(res$Best.nc) && length(res$Best.nc) >= 1 && !is.na(res$Best.nc[1]) && res$Best.nc[1] != "") {
      best_ks[i] <- as.numeric(res$Best.nc[1])
    } else {
      best_ks[i] <- NA
      warning("Index '", idx, "' did not return a valid Best.nc[1]. Set to NA.")
    }
  }
  
  # Remove NAs before majority vote
  valid_ks <- best_ks[!is.na(best_ks)]
  
  if (length(valid_ks) == 0) {
    warning("No valid best k found from any index. Returning NA.")
    return(NA)
  }
  
  # Majority vote: most frequent best_k
  best_k_overall <- as.numeric(names(sort(table(valid_ks), decreasing = TRUE))[1])
  message("Best k overall: ", best_k_overall)
  return(best_k_overall)
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

grid_search_hdbscan_votes <- function(data, 
                                      minPts_values = seq(5, 50, by = 5),
                                      distance_matrix = NULL,
                                      verbose = TRUE) {
  
  data %<>% 
    dplyr::select(where(is.numeric)) %>% 
    as.matrix()
  
  if (is.null(distance_matrix)) {
    distance_matrix <- dist(data)
  }
  
  metrics <- c(
    "ch", "wb.ratio", "dunn", "dunn2", "sindex",
    "avg.silwidth", "min.clus.silwidth", "clus.avg.silwidths",
    "pearsongamma", #"g2", "g3",
    "entropy", "within.cluster.ss"
  )
  
  higher_better <- setdiff(metrics, "within.cluster.ss")
  lower_better <- "within.cluster.ss"
  
  results <- map_dfr(minPts_values, function(minPts) {
    tryCatch({
      model <- dbscan::hdbscan(data, minPts = minPts)
      clusters <- model$cluster
      
      # Skip cases with no clusters (all noise or 1 cluster)
      if (length(unique(clusters[clusters != 0])) <= 1) {
        if (verbose) message(glue::glue("⚠️ Skipping minPts = {minPts}: Only one cluster or all noise."))
        return(NULL)
      }
      
      stats <- fpc::cluster.stats(distance_matrix, clusters)
      
      # Summarize indices
      summary_vals <- list(
        ch                 = stats$ch,
        wb.ratio           = stats$wb.ratio,
        dunn              = stats$dunn,
        dunn2             = stats$dunn2,
        sindex            = stats$sindex,
        avg.silwidth      = stats$avg.silwidth,
        min.clus.silwidth = min(stats$clus.avg.silwidths),
        clus.avg.silwidths = mean(stats$clus.avg.silwidths),
        pearsongamma      = stats$pearsongamma,
        #g2                = stats$g2,
        #g3                = stats$g3,
        entropy           = stats$entropy,
        within.cluster.ss = stats$within.cluster.ss
      )
      
      tibble(minPts = minPts, 
             model = list(model),
             clusters = list(clusters),
             !!!summary_vals)
      
    }, error = function(e) {
      if (verbose) message(glue::glue("❌ Failed for minPts = {minPts}: {e$message}"))
      NULL
    })
  })
  
  if (nrow(results) == 0) stop("All HDBSCAN runs failed or returned invalid clusterings.")
  
  # Voting logic
  vote_table <- metrics %>%
    map_dfr(function(metric) {
      vals <- results[[metric]]
      if (all(is.na(vals))) return(tibble())  # skip NA metrics
      
      if (metric %in% higher_better) {
        winner <- results$minPts[which.max(vals)]
      } else {
        winner <- results$minPts[which.min(vals)]
      }
      tibble(metric = metric, winner = winner)
    })
  
  vote_count <- vote_table %>%
    count(winner, sort = TRUE, name = "votes")
  
  best_minPts <- vote_count$winner[1]
  best_result <- results %>% filter(minPts == best_minPts)
  
  if (verbose) {
    cat("🏆 Voting results:\n")
    print(vote_count)
    cat(glue::glue("\n✅ Best minPts = {best_minPts} with {vote_count$votes[1]} winning metrics.\n"))
  }
  
  num_clusters = if (!is.null(best_result$clusters[[1]])) {
    length(unique(best_result$clusters[[1]][best_result$clusters[[1]] != 0]))
  } else {
    0
  }
  
  # list(
  #   best_model = best_result$model[[1]],
  #   best_clusters = best_result$clusters[[1]],
  #   best_params = best_result %>% select(minPts, all_of(metrics)),
  #   results_table = results %>% select(-model, -clusters),
  #   vote_table = vote_table,
  #   vote_count = vote_count,
  #   
  # )
  
  list(
    n = num_clusters,
    assn = best_result$clusters[[1]]
  )
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


# res <- NbClust(data$reduced$pca[,1:3], 
#                distance = "euclidean", 
#                min.nc = 2, 
#                max.nc = 10, 
#                method = "kmeans", 
#                index = "all") # 2
# 
# get_vector_mode(as.vector(res$Best.nc))
# 
# res <- NbClust(kpca_rbf_df[,1:3], 
#                distance = "euclidean", 
#                min.nc = 2, 
#                max.nc = 10, 
#                method = "kmeans", 
#                index = "all") # 4
# 
# 
# ### hclust
# res <- NbClust(data$pcit$scaled, 
#                distance = "euclidean", 
#                min.nc = 2, 
#                max.nc = 10, 
#                method = "ward.D", 
#                index = "all") # 2
# 
# res <- NbClust(pca_df_full[,1:3], 
#                distance = "euclidean", 
#                min.nc = 2, 
#                max.nc = 10, 
#                method = "ward.D", 
#                index = "all") # 5
# 
# res <- NbClust(kpca_rbf_df[,1:3], 
#                distance = "euclidean", 
#                min.nc = 2, 
#                max.nc = 10, 
#                method = "ward.D", 
#                index = "all") # 4
# 
# res <- NbClust(data$pcit$scaled,
#                distance = "euclidean",
#                min.nc = 2,
#                max.nc = 10,
#                method = "ward.D2",
#                index = "all") # 2
# 
# res <- NbClust(pca_df_full[,1:3],
#                distance = "euclidean",
#                min.nc = 2,
#                max.nc = 10,
#                method = "ward.D2",
#                index = "all") # 2
# 
# res <- NbClust(kpca_rbf_df[,1:3],
#                distance = "euclidean",
#                min.nc = 2,
#                max.nc = 10,
#                method = "ward.D2",
#                index = "all") # 4
# 
# 
# res <- NbClust(pca_df_full[,1:3], 
#                distance = "euclidean", 
#                min.nc = 2, 
#                max.nc = 10, 
#                method = "single", 
#                index = "all") # 2
# 
# res <- NbClust(kpca_rbf_df[,1:3], 
#                distance = "euclidean", 
#                min.nc = 2, 
#                max.nc = 10, 
#                method = "single", 
#                index = "all") # 2
# 
# res <- NbClust(pca_df_full[,1:3], 
#                distance = "euclidean", 
#                min.nc = 2, 
#                max.nc = 10, 
#                method = "average", 
#                index = "all") # 2
# 
# res <- NbClust(kpca_rbf_df[,1:3], 
#                distance = "euclidean", 
#                min.nc = 2, 
#                max.nc = 10, 
#                method = "average", 
#                index = "all") # 5
# 
# res <- NbClust(pca_df_full[,1:3], 
#                distance = "euclidean", 
#                min.nc = 2, 
#                max.nc = 10, 
#                method = "complete", 
#                index = "all") # 2
# 
# res <- NbClust(kpca_rbf_df[,1:3], 
#                distance = "euclidean", 
#                min.nc = 2, 
#                max.nc = 10, 
#                method = "complete", 
#                index = "all") # 3

# PCA suggestions: 2, 5, / 2, 2, 2, 2
# kPCA suggestions: 4, 4, / 4, 2, 5, 3
# 6-var: ?, ? / 2, 2
