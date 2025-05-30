
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

hcluster <- function(cities_scaled){
  dist_mtx <- get_dist(cities_scaled)
  hc <- hclust(dist_mtx, method = "ward.D")
  #plot(hc, labels = rownames(data), main = "Hierarchical Clustering Dendrogram")
  clusters <- cutree(hc, k = 3)
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

