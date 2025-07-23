plot_clustering <- function(pca_df){
  ggplot(pca_df, aes(x=pc1, y=pc2, color=cluster)) +
    geom_point(size=3) +
    theme_minimal() +
    labs(title = "K-Means Clustering with Name Labels (PCA projection)")
}

# plot_pca_3d <- function(city_choice, pca_out, clusters){
#   pca_3d <- tibble(pc1 = pca_out$pc1,
#                    pc2 = pca_out$pc2,
#                    pc3 = pca_out$pc3)
#   pca_3d$label <- city_names
#   pca_3d$cluster <- as.factor(clusters)
#   
#   pca_3d %<>%
#     mutate(highlight = case_when(label == city_choice ~ "highlight",
#                                  .default = "normal"))
#   
#   p <- plot_ly(pca_3d %>% filter(highlight == "normal"),
#                x = ~pc1, y = ~pc2, z = ~pc3,
#                type = "scatter3d",
#                mode = "markers",
#                color = ~cluster,
#                text = ~label,
#                marker = list(size=4)
#   ) %>%
#     layout(title = "Interactive 3d PCA Plot")
#   
#   p <- add_trace(p, data = pca_3d %>% filter(highlight == "highlight"),
#                  x = ~pc1, y = ~pc2, z = ~pc3,
#                  type = "scatter3d",
#                  mode = "markers",
#                  text = ~label,
#                  marker = list(size=10, color="yellow"))
#   
#   p %>% layout(title = "3d PCA Plot with Highlight")
# }

plot_pca_3d <- function(city_choice, pca_out, clusters, city_names, save_dir = NULL) {
  # Prepare data
  pca_3d <- tibble(
    pc1 = pca_out$C1,
    pc2 = pca_out$C2,
    pc3 = pca_out$C3,
    label = city_names,
    cluster = as.factor(clusters)
  ) %>%
    mutate(highlight = ifelse(label == city_choice, "highlight", "normal"))
  
  # Plot "normal" points
  p <- plot_ly(pca_3d %>% filter(highlight == "normal"),
               x = ~pc1, y = ~pc2, z = ~pc3,
               type = "scatter3d", mode = "markers",
               color = ~cluster, text = ~label,
               marker = list(size = 4)) %>%
    layout(title = "Interactive 3D PCA Plot")
  
  # Add highlight points
  p <- add_trace(p, data = pca_3d %>% filter(highlight == "highlight"),
                 x = ~pc1, y = ~pc2, z = ~pc3,
                 type = "scatter3d", mode = "markers",
                 text = ~label,
                 marker = list(size = 10, color = "yellow")) %>%
    layout(title = "3D PCA Plot with Highlight")
  
  # Print to RStudio Viewer
  print(p)
  
  dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)
  save_path <- file.path(save_dir, "pca_best_3d.html")
  
  # Save to HTML if a path is provided
  if (!is.null(save_dir)) {
    saveWidget(p, file = save_path, selfcontained = TRUE)
    message("Plot saved to: ", save_path)
  }
  
  # Return the plot object in case it's needed later
  invisible(p)
}


# rgl_plot_pca_3d <- function(city_choice, pca_out, clusters, city_names) {
#   # Prepare data
#   pca_3d <- data.frame(
#     pc1 = pca_out$pc1,
#     pc2 = pca_out$pc2,
#     pc3 = pca_out$pc3,
#     label = city_names,
#     cluster = as.factor(clusters)
#   )
#   
#   # Highlighting
#   pca_3d$highlight <- ifelse(pca_3d$label == city_choice, "highlight", "normal")
#   
#   # Set colors
#   cluster_colors <- rainbow(length(unique(pca_3d$cluster)))
#   names(cluster_colors) <- levels(pca_3d$cluster)
#   pca_3d$color <- cluster_colors[pca_3d$cluster]
#   pca_3d$size <- ifelse(pca_3d$highlight == "highlight", 5, 2)
#   pca_3d$color[pca_3d$highlight == "highlight"] <- "yellow"
#   
#   # Open new 3D window
#   open3d()
#   plot3d(
#     x = pca_3d$pc1,
#     y = pca_3d$pc2,
#     z = pca_3d$pc3,
#     col = pca_3d$color,
#     size = pca_3d$size,
#     type = "s",
#     xlab = "PC1", ylab = "PC2", zlab = "PC3"
#   )
#   
#   # Add legend (optional)
#   legend3d("topright", legend = levels(pca_3d$cluster),
#            col = cluster_colors, pch = 16, cex = 1)
# }

# Generates spinning 3d plot of dimension-reduced space.
# If points aren't plotting, update GPU drivers.
make_pca_3d_gif <- function(city_choice, pca_out, clusters, city_names, gif_dir) {
  # Load required packages
  if (!requireNamespace("rgl", quietly = TRUE)) stop("Package 'rgl' is required.")
  if (!requireNamespace("gifski", quietly = TRUE)) stop("Package 'gifski' is required.")
  if (!requireNamespace("magrittr", quietly = TRUE)) install.packages("magrittr")
  
  library(rgl)
  library(magrittr)
  
  # Prepare data
  pca_3d <- data.frame(
    pc1 = pca_out$C1,
    pc2 = pca_out$C2,
    pc3 = pca_out$C3,
    label = city_names,
    cluster = as.factor(clusters)
  )
  
  pca_3d$highlight <- ifelse(pca_3d$label == city_choice, "highlight", "normal")
  
  # Assign cluster colors
  cluster_colors <- rainbow(length(unique(pca_3d$cluster)))
  names(cluster_colors) <- levels(pca_3d$cluster)
  pca_3d$color <- cluster_colors[pca_3d$cluster]
  pca_3d$size <- ifelse(pca_3d$highlight == "highlight", 5, 2)
  pca_3d$color[pca_3d$highlight == "highlight"] <- "yellow"
  
  # print(head(pca_3d))
  # cat("nrow(df):", nrow(pca_3d), "\n")
  # summary(pca_3d)
  # table(pca_3d$color, useNA = "always")
  
  # Open RGL device
  #rgl.useNULL(FALSE)
  #options(rgl.useNULL = FALSE)
  # options(rgl.printRglwidget = TRUE)
  # rglwidget(minimal =
  #             FALSE) 
  
  open3d()
  plot3d(
    x = pca_3d$pc1,
    y = pca_3d$pc2,
    z = pca_3d$pc3,
    col = pca_3d$color,
    size = pca_3d$size,
    type = "s",
    xlab = "PC1", ylab = "PC2", zlab = "PC3"
  )
  
  
  
  
  # Optional: Add legend
  legend3d("topright", legend = levels(pca_3d$cluster),
           col = cluster_colors, pch = 16, cex = 1)
  
  # === Output Paths ===
  dir.create(gif_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Spin and save PNG frames (without ImageMagick)
  movie3d(
    spin3d(axis = c(0, 0, 1)),
    duration = 5,
    fps = 10,
    movie = "3d_spin_z",
    dir = gif_dir,
    type = "png",   # Create PNGs, not a GIF directly
    convert = FALSE # Don't use ImageMagick
  )
  
  # Collect frames
  png_files <- list.files(gif_dir, pattern = "3d_spin_z.*\\.png$", full.names = TRUE)
  gif_file <- file.path(gif_dir, "3d_spin_z.gif")
  
  # Create the GIF
  gifski::gifski(
    png_files,
    gif_file = gif_file,
    width = 800,
    height = 800,
    delay = 1 / 10
  )
  
  message("GIF saved to: ", gif_file)
  
}


# plot_kpca_3d <- function(city_choice, df, clusters){
#   df$label <- city_names
#   df$cluster <- as.factor(clusters)
#   
#   df_norm <- df %>% filter(highlight == "normal")
#   
#   
#   df_norm %<>%
#     mutate(highlight = case_when(label == city_choice ~ "highlight",
#                                  .default = "normal"))
#   
#   #df_hi <- df %>% filter(highlight == "highlight")
#   
#   p <- plot_ly(df_norm,
#                x = df_norm[[1]], y = df_norm[[2]], z = df_norm[[3]],
#                type = "scatter3d",
#                mode = "markers",
#                color = ~cluster,
#                text = ~label,
#                marker = list(size=4)
#   ) %>%
#     layout(title = "Interactive 3d PCA Plot")
#   
#   p <- add_trace(p, data = df_hi,
#                  x = df_hi[[1]], y = df_hi[[2]], z = df_hi[[3]],
#                  type = "scatter3d",
#                  mode = "markers",
#                  text = ~label,
#                  marker = list(size=10, color="yellow"))
#   
#   p %>% layout(title = "3d PCA Plot with Highlight")
# }

plot_kpca_3d <- function(df,
                         pc_cols = c("C1", "C2", "C3"),
                         cluster_col = "cluster",
                         text_col = "NAME",
                         highlight_col = "highlight",
                         highlight_value = "highlight",
                         title = "3D kPCA Plot with Highlight",
                         save_path = NULL) {
  
  # Check column existence
  if (!all(pc_cols %in% colnames(df))) stop("Some PC columns not found.")
  if (!all(c(cluster_col, text_col, highlight_col) %in% colnames(df))) stop("Some metadata columns not found.")
  
  # Extract x, y, z, color, and text vectors for main plot
  x <- df[[pc_cols[1]]]
  y <- df[[pc_cols[2]]]
  z <- df[[pc_cols[3]]]
  color <- as.factor(df[[cluster_col]])
  text <- df[[text_col]]
  
  # Base scatter plot
  p <- plot_ly(x = x, y = y, z = z,
               type = "scatter3d",
               mode = "markers",
               color = color,
               text = text,
               marker = list(size = 4),
               showlegend = FALSE)
  
  # Add highlighted points
  highlight_df <- df %>% filter(.data[[highlight_col]] == highlight_value)
  if (nrow(highlight_df) > 0) {
    p <- add_trace(p,
                   x = highlight_df[[pc_cols[1]]],
                   y = highlight_df[[pc_cols[2]]],
                   z = highlight_df[[pc_cols[3]]],
                   type = "scatter3d",
                   mode = "markers",
                   text = highlight_df[[text_col]],
                   marker = list(size = 10, color = "yellow"),
                   showlegend = FALSE)
  }
  
  p %>% layout(title = title)
  
  # Print to RStudio Viewer
  print(p)
  
  # Save to HTML if a path is provided
  if (!is.null(save_path)) {
    saveWidget(p, file = save_path, selfcontained = TRUE)
    message("Plot saved to: ", save_path)
  }
  
  # Return the plot object in case it's needed later
  invisible(p)
}

make_kpca_3d_gif <- function(df,
                             pc_cols = c("C1", "C2", "C3"),
                             cluster_col = "cluster",
                             text_col = "NAME",
                             highlight_col = "highlight",
                             highlight_value = "highlight",
                             gif_dir = "./gifs/kpca",
                             gif_name = "3d_spin_z.gif",
                             duration = 5,
                             fps = 10) {
  
  # Load required packages
  if (!requireNamespace("rgl", quietly = TRUE)) stop("Package 'rgl' is required.")
  if (!requireNamespace("gifski", quietly = TRUE)) stop("Package 'gifski' is required.")
  if (!requireNamespace("magrittr", quietly = TRUE)) install.packages("magrittr")
  
  library(rgl)
  library(magrittr)
  library(dplyr)
  
  # Ensure output directory exists
  dir.create(gif_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Validate column names
  if (!all(pc_cols %in% colnames(df))) stop("Some PC columns not found.")
  if (!all(c(cluster_col, text_col, highlight_col) %in% colnames(df))) stop("Some metadata columns not found.")
  
  # Extract relevant columns
  df <- df %>%
    mutate(
      x = .data[[pc_cols[1]]],
      y = .data[[pc_cols[2]]],
      z = .data[[pc_cols[3]]],
      cluster = as.factor(.data[[cluster_col]]),
      label = .data[[text_col]],
      highlight = ifelse(.data[[highlight_col]] == highlight_value, "highlight", "normal")
    )
  
  # Colors and sizes
  cluster_colors <- rainbow(length(unique(df$cluster)))
  names(cluster_colors) <- levels(df$cluster)
  df$color <- cluster_colors[df$cluster]
  df$size <- ifelse(df$highlight == "highlight", 5, 2)
  df$color[df$highlight == "highlight"] <- "yellow"
  
  # Open 3D device
  options(rgl.useNULL = FALSE)
  open3d()
  plot3d(
    x = df$x,
    y = df$y,
    z = df$z,
    col = df$color,
    size = df$size,
    type = "s",
    xlab = pc_cols[1], ylab = pc_cols[2], zlab = pc_cols[3]
  )
  
  # Add legend
  legend3d("topright", legend = levels(df$cluster),
           col = cluster_colors, pch = 16, cex = 1)
  
  # Generate spinning PNG frames
  movie3d(
    spin3d(axis = c(0, 0, 1)),
    duration = duration,
    fps = fps,
    movie = tools::file_path_sans_ext(gif_name),
    dir = gif_dir,
    type = "png",
    convert = FALSE
  )
  
  # Convert PNG frames to GIF
  png_files <- list.files(gif_dir, pattern = paste0(tools::file_path_sans_ext(gif_name), ".*\\.png$"), full.names = TRUE)
  gif_file <- file.path(gif_dir, gif_name)
  
  gifski::gifski(
    png_files,
    gif_file = gif_file,
    width = 800,
    height = 800,
    delay = 1 / fps
  )
  
  message("GIF saved to: ", gif_file)
}


add_highlight <- function(df, name_col = "City", city_choice){
  col_sym <- sym(name_col)
  
  df %<>%
    mutate(highlight = case_when(!!col_sym == city_choice ~ "highlight",
                                 .default = "normal"))
  
  df
}
