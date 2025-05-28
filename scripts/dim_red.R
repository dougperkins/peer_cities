p_

# Load libraries
library(Rtsne)
library(dplyr)
library(ggplot2)

# Example: create a numeric data frame
set.seed(42)
df <- data.frame(
  var1 = rnorm(100),
  var2 = rnorm(100),
  var3 = rnorm(100),
  var4 = rnorm(100)
)

# Step 1: Ensure only numeric columns and no missing values
df_numeric <- acs_data_ests %>%
  select(where(is.numeric)) %>%
  na.omit()

# Step 2: Optional: scale (z-score standardize) the data
df_scaled <- scale(df_numeric)

# Step 3: Run t-SNE
tsne_result <- Rtsne(df_scaled, dims = 2, perplexity = 30, verbose = TRUE, max_iter = 500)

# Step 4: Add t-SNE results to a new data frame
tsne_df <- as.data.frame(tsne_result$Y)
colnames(tsne_df) <- c("Dim1", "Dim2")

kmeans_5 <- elbow_
tsne_df$cluster <- factor(kmeans_result[[5]]$model$cluster)

# Step 5: Plot
ggplot(tsne_df, aes(x = Dim1, y = Dim2)) +
  geom_point() +
  labs(title = "t-SNE Visualization", x = "t-SNE 1", y = "t-SNE 2") +
  theme_minimal()
