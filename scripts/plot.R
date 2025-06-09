plot_clustering <- function(pca_df){
  ggplot(pca_df, aes(x=pc1, y=pc2, color=cluster)) +
    geom_point(size=3) +
    theme_minimal() +
    labs(title = "K-Means Clustering with Name Labels (PCA projection)")
}

plot_pca_3d <- function(city_choice, pca_out, clusters){
  pca_3d <- tibble(pc1 = pca_out$x[,1],
                   pc2 = pca_out$x[,2],
                   pc3 = pca_out$x[,3])
  pca_3d$label <- city_names
  pca_3d$cluster <- as.factor(clusters)
  
  pca_3d %<>%
    mutate(highlight = case_when(label == city_choice ~ "highlight",
                                 .default = "normal"))
  
  p <- plot_ly(pca_3d %>% filter(highlight == "normal"),
               x = ~pc1, y = ~pc2, z = ~pc3,
               type = "scatter3d",
               mode = "markers",
               color = ~cluster,
               text = ~label,
               marker = list(size=4)
  ) %>%
    layout(title = "Interactive 3d PCA Plot")
  
  p <- add_trace(p, data = pca_3d %>% filter(highlight == "highlight"),
                 x = ~pc1, y = ~pc2, z = ~pc3,
                 type = "scatter3d",
                 mode = "markers",
                 text = ~label,
                 marker = list(size=10, color="yellow"))
  
  p %>% layout(title = "3d PCA Plot with Highlight")
}

plot_kpca_3d <- function(city_choice, df, clusters){
  df$label <- city_names
  df$cluster <- as.factor(clusters)
  
  df_norm <- df %>% filter(highlight == "normal")
  
  
  df_norm %<>%
    mutate(highlight = case_when(label == city_choice ~ "highlight",
                                 .default = "normal"))
  
  #df_hi <- df %>% filter(highlight == "highlight")
  
  p <- plot_ly(df_norm,
               x = df_norm[[1]], y = df_norm[[2]], z = df_norm[[3]],
               type = "scatter3d",
               mode = "markers",
               color = ~cluster,
               text = ~label,
               marker = list(size=4)
  ) %>%
    layout(title = "Interactive 3d PCA Plot")
  
  p <- add_trace(p, data = df_hi,
                 x = df_hi[[1]], y = df_hi[[2]], z = df_hi[[3]],
                 type = "scatter3d",
                 mode = "markers",
                 text = ~label,
                 marker = list(size=10, color="yellow"))
  
  p %>% layout(title = "3d PCA Plot with Highlight")
}
