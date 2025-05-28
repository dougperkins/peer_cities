ma <- acs_data_ests %>%
    filter(State == "Massachusetts")

ma_prepped <- ma %>%
    select(median_home_valueE, occupancy_statusE) %>%
    select(where(is.numeric)) %>% # nolint: indentation_linter.
    na.omit() %>%
    scale()

# Perform k-means clustering on ma_prepped
set.seed(123) # for reproducibility
k <- 3 # choose the number of clusters

kmeans_result <- map(1:10, function(k) {
    k_means_model <- kmeans(df_scaled, centers = k, nstart = 25)
    list(k = k, wss = k_means_model$tot.withinss, model = k_means_model)
})

# Convert to a tibble
elbow_df <- tibble(
    k = map_int(kmeans_result, "k"),
    wss = map_dbl(kmeans_result, "wss"),
    model = map(kmeans_result, "model")
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

# View cluster assignments
print(kmeans_result$cluster)

ggplot(ma_prepped, aes(x = median_home_valueE, y = occupancy_statusE)) +
    geom_point(aes(color = as.factor(kmeans_result$cluster)), size = 3) +
    labs(
        title = "K-means Clustering of Massachusetts ACS Data",
        x = "Median Home Value",
        y = "Occupancy Status",
        color = "Cluster"
    ) +
    theme_minimal()

# Generate a correlation matrix for the 6 features in acs_data_ests
cor_matrix <- ma_prepped %>%
    cor(use = "complete.obs")

print(cor_matrix)

# Visualize the correlation matrix using corrplot
corrplot(cor_matrix,
    method = "color", type = "upper",
    addCoef.col = "black", tl.col = "black", tl.srt = 45,
    title = "Correlation Matrix", mar = c(0, 0, 2, 0)
)

# Make a pairs plot (scatterplot matrix) for the variables in acs_data_ests
pairs(
    ma_prepped,
    main = "Pairs Plot of ACS Variables",
    pch = 21,
    bg = "lightblue"
)
