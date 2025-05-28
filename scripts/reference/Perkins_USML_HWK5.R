############################
## Clustering Part 2      ##
############################
## Doug Perkins 10/20/2024 ##
## CS5230 Unsupervised ML ##
## Northeastern Univ.     ##
############################

#####################################################################
#####################################################################
# PREP LIBRARIES ###################################################
####################################################################
# install.packages('fpc')
library(tidyverse)
library(cluster)
library(fpc)


#####################################################################
#####################################################################
# INITIALIZE DATA ###################################################
#####################################################################

df <- read_csv("data/measurementPoint.csv",
  # delim=" ",
  col_names = T
)

df <- df %>%
  separate_wider_delim(cols = "M1 M2", delim = " ", names = c("n", "M1", "M2")) %>%
  mutate(M1 = as.numeric(M1), M2 = as.numeric(M2)) %>%
  rename(Pt = n)

ggplot(df, aes(x = M1, y = M2)) +
  geom_point() +
  labs(title = "M2 as a function of M1")

df <- df %>% select(-Pt)

#####################################################################
#####################################################################
# INITIALIZE PARAMETERS #############################################
#####################################################################

restarts <- 100
max_iter <- 100

#####################################################################
#####################################################################
# k-MEANS CLUSTERING ################################################
#####################################################################
kmin <- 1
kmax <- 5

km <- lapply(kmin:kmax, function(k) kmeans(df, centers = k, max_iter, restarts))

km$withinss
km$betweenss
km$cluster

km_tot.withinss <- lapply(kmin:kmax, function(x) km[[x]]$tot.withinss)
km_betweenss <- lapply(kmin:kmax, function(x) km[[x]]$betweenss)
km_cluster <- lapply(kmin:kmax, function(x) km[[x]]$withinss)
df_km_color <- lapply(kmin:kmax, function(x) {
  df %>% mutate(cluster = as.factor(km[[x]]$cluster))
})

ggplot(df_km_color[[1]], aes(x = M1, y = M2, color = cluster)) +
  geom_point() +
  labs(title = "Clustering with kmeans, k=1")
ggplot(df_km_color[[2]], aes(x = M1, y = M2, color = cluster)) +
  geom_point() +
  labs(title = "Clustering with kmeans, k=2")
ggplot(df_km_color[[3]], aes(x = M1, y = M2, color = cluster)) +
  geom_point() +
  labs(title = "Clustering with kmeans, k=3")
ggplot(df_km_color[[4]], aes(x = M1, y = M2, color = cluster)) +
  geom_point() +
  labs(title = "Clustering with kmeans, k=4")
ggplot(df_km_color[[5]], aes(x = M1, y = M2, color = cluster)) +
  geom_point() +
  labs(title = "Clustering with kmeans, k=5")

#####################################################################
#####################################################################
# SILHOUETTE PLOT ###################################################
#####################################################################

dists <- dist(df)

sils <- lapply((kmin + 1):kmax, function(k) silhouette(x = km_cluster[[k]], dist = dists))

plot(sils[[1]], main = "Silhouette plot, kmeans clustering, k=2")
plot(sils[[2]], main = "Silhouette plot, kmeans clustering, k=3")
plot(sils[[3]], main = "Silhouette plot, kmeans clustering, k=4")
plot(sils[[4]], main = "Silhouette plot, kmeans clustering, k=5")

sil_overall_avg_width <- c(0.81, 0.7, 0.52, 0.42)
sil_min_per_cluster_width <- c(0.80, 0.63, 0.35, 0.35)

#####################################################################
#####################################################################
# CH INDEX ##########################################################
#####################################################################
cstats <- lapply(((kmin + 1):kmax), function(k) cluster.stats(d = dists, clustering = km_cluster[[k]]))

ch_values <- unlist(lapply(1:4, function(x) cstats[[x]]$ch))
ch_values

#####################################################################
#####################################################################
# EVAL METRICS PLOT #################################################
#####################################################################

km_evaluation_df <- tibble(
  k = c(1:5),
  tot_within_ss = unlist(km_tot.withinss)[1:5],
  between_ss = unlist(km_betweenss)[1:5],
  sil_overall_avg_width = c(0, sil_overall_avg_width),
  sil_min_per_cluster_width = c(0, sil_min_per_cluster_width),
  ch_values = c(0, ch_values)
)

sz <- 3
ggplot(km_evaluation_df) +
  geom_point(aes(x = k, y = log10(tot_within_ss), color = "Total within-cluster SSE"), size = sz) +
  geom_point(aes(x = k, y = log10(sil_overall_avg_width), color = "Avg. silhouette width"), size = sz) +
  geom_point(aes(x = k, y = log10(sil_min_per_cluster_width), color = "Min. silhouette width"), size = sz) +
  geom_point(aes(x = k, y = log10(ch_values), color = "CH index"), size = sz) +
  labs(title = "k-means Clustering Evaluation Metrics", x = "# Clusters (k)", y = "Evaluation Metric") +
  scale_color_discrete(name = "Evaluation Metric")


ggplot(km_evaluation_df) +
  geom_point(aes(x = k, y = tot_within_ss, color = "Total within-cluster SSE"), size = sz)
