# LIBRARIES ====== ####
if (!requireNamespace("pacman")) {
  install.packages("pacman")
}

library(pacman)
p_load(
  tidycensus, dplyr,
  languageserver, pacman,
  corrplot, validate,
  tidyverse, skimr,
  magrittr, tigris,
  factoextra, ggfortify, # factoextra: fviz, ggfortify: autoplot
  plotly, htmlwidgets, # plotly: plot_ly 3d
  reticulate, sf,
  tictoc, furrr,
  progressr, dbscan,
  cluster, fpc,
  hopkins, kernlab,
  Rdimtools, NbClust,
  caret, GGally
)

options(tigris_use_cache = TRUE)

# Set your Census API key for getting ACS data
# census_api_key(API_KEY, install = TRUE, overwrite = TRUE)

# SOURCE ======== ####
source("./scripts/constants.R")
source("./scripts/utils.R")
source("./scripts/acs.R")
source("./scripts/alfin.R")
source("./scripts/fips.R")
source("./scripts/cluster.R")
source("./scripts/dim_red.R")
source("./scripts/plot.R")
source("./scripts/peers.R")
source("./scripts/preprocess.R")

# LOAD DATA ====== ####
## PCIT ####
pcit_df <- read_csv("./data/pcit/2024-peer-city-data-full.csv") %>%
  mutate(proj_uid = capitalize_after_underscore(proj_uid))

# missing <- pcit_df %>%
#   filter(!(proj_uid %in% acs_in_pcit$pcit))

## City Areas ####
# TODO: Only 861 cities have area here; leads to NAs in housing density analysis
# Need to get updated area data
areas <- read_csv("./data/area/2010/cities_size.csv") %>% 
  rename(NAME = City, area_sqmi = `Land Area`)

## 2023 Metro Area Populations ####
# Metro area populations do not seem to be available for 2018 or 2013
metro_area_pops <- read_csv("./data/metro_areas/metro_area_pops_pcit_960.csv")
metro_area_pops_full <- read_csv("./data/metro_areas/metro_area_pops_acs_places_23969.csv") # From the 32323 places in ACS 5y 2023


## ACS ####
# TODO: save 2010-2023 ACS tables?
# First, for 5y comparison: 2023, 2018, 2013

# # Load ACS variable descriptions for 2022 ACS 5-year
# acs_var_lookup_22 <- load_variables(2022, "acs5", cache = TRUE) # For comparison with existing tools
# acs_var_lookup_23 <- load_variables(2023, "acs5", cache = TRUE)
# acs_var_lookup_18 <- load_variables(2018, "acs5", cache = TRUE)
# acs_var_lookup_13 <- load_variables(2013, "acs5", cache = TRUE)

# # Check if the variable numbers are present in each year
# TODO Check that they also however need to have the same labels over the years
# acs_vars_housing
# check_acs_vars(acs_vars_housing, list(acs_var_lookup_23, acs_var_lookup_18, acs_var_lookup_13))

# Years
# The ACS doesn't recommend comparing overlapping 5-year datasets
years <- c("2013", "2018", "2023")

# Gets places, counties, townships, and metro areas (total pop only) 
# from ACS 5y 2013, 2018, and 2023
# Takes a few minutes to run.
acs <- map(years, function(y) load_acs_housing(y))

# Get the PCIT dataframe city names from the place, county, and township names
# Takes a minute or two to run
places <- map(acs, function(df_list){get_pcit_places(df_list[[1]])})
counties <- map(acs, function(df_list){get_pcit_counties(df_list[[2]])})
townships <- map(acs, function(df_list){get_pcit_townships(df_list[[3]])})
#metros <- map(acs, function(df_list){get_pcit_townships(df_list[[4]])})

# Filter down to the 960 PCIT cities
# TODO: Not ignore metros?
all_acs <- pmap(
  list(places, counties, townships),
  function(pl, co, tw) load_acs_pcit_960(pl, co, tw)
)

# Get only the estimates, drop all margin of error columns
all_acs_ests <- map(all_acs, ~ get_acs_estimates(.x))

# Get list of shared cities between the years
# TODO: Somehow only 958 cities shared between the years
shared_cities <- base::intersect(base::intersect(unique(all_acs[[1]]$NAME),
                                unique(all_acs[[2]]$NAME)),
                                unique(all_acs[[3]]$NAME))

# Filter to only the shared cities
for (i in 1:3){
  all_acs_ests[[i]] %<>%
    filter(NAME %in% shared_cities) %>%
    left_join(areas, by="NAME")
}

# Get area (sqmi) and metro area pops on 2023 df.
# Metro area population does not seem to be available for 2018 & 2013
all_acs_ests[[3]] %<>% st_drop_geometry()
all_acs_ests[[3]] %<>% 
  left_join(metro_area_pops %>% select(NAME, NAME.1, metro_popE, pct_metro_area_pop), by="NAME") %>%
  rename(metro = NAME.1)




  



## ALFIN (Finance) ####
# alfin_agg <- load_alfin_state_agg(codes = FALSE)
# 
# alfin_pid <- load_alfin_pid()
# alfin_idu <- load_alfin_idu()
# 
# alfin_idu_in_pcit <- alfin_idu %>%
#   filter(fips_st_pl %in% pcit_df$fips)

# # Which variables have a lot of data for the PCIT cities?
# alfin_idu_in_pcit %>% count(item_desc) %>% View()





# FEAT ENG ======= ####
#cities %<>% derive_features()

#acs_pcit_ests <- metro_parallel

# Derive static ACS variables from existing ones
all_acs_ests <- map(all_acs_ests, function(acs_df){derive_acs_static(acs_df)})

# Tag each dataset's columns with year
col_suffixes <- paste0("_acs5_", years)

all_acs_ests <- map2(all_acs_ests, col_suffixes, function(df, col_suffix) {
  colnames(df) <- paste0(colnames(df), col_suffix)
  df %<>% rename(city = paste0("NAME", col_suffix))
  df
})

# Prep one dataset with all of the year-variables
cities <- reduce(all_acs_ests, ~ left_join(.x, .y, by="city"))

# Derive temporal difference variables for each static variable 
cities <- derive_acs_temporal_all(cities)


# cities <- acs_pcit_ests

# PREPROCESS ==== ####
## Cleaning ####
# Drop zero-variance
cities %<>%
  select(-area_sqmi_acs5_diff_2018_2013, -area_sqmi_acs5_diff_2023_2013, -area_sqmi_acs5_diff_2023_2018)

# Drop area columns due to missingness
# TODO fix
cities_no_area_cols <- cities %>%
  select(-area_sqmi_acs5_2013, 
         -area_sqmi_acs5_2018, 
         -area_sqmi_acs5_2023, 
         -housing_units_per_sqmi_acs5_2013, 
         -housing_units_per_sqmi_acs5_2018, 
         -housing_units_per_sqmi_acs5_2023,
         -housing_units_per_sqmi_acs5_diff_2018_2013,
         -housing_units_per_sqmi_acs5_diff_2023_2013,
         -housing_units_per_sqmi_acs5_diff_2023_2018)

# Maybe instead, drop cities with missing areas
# cities_w_areas <- cities %>%
#   filter(!is.na(area_sqmi_acs5_2023))

# Also drop cities with other missingness
cities_no_area_cols_no_med_yr_moved <- cities_no_area_cols %>%
  filter(!is.na(median_year_moved_in_renterE_acs5_diff_2023_2018)) %>%
  filter(!is.na(median_year_moved_in_renterE_acs5_2018)) %>%
  filter(!is.na(median_year_moved_in_renterE_acs5_2023)) %>%
  filter(!is.na(median_year_moved_in_renterE_acs5_diff_2018_2013)) %>%
  filter(!is.na(median_year_moved_in_renterE_acs5_diff_2023_2013)) 


## Transform Skewed ####
# # TODO: Try other transformations?
# cities %<>% 
#   select(NAME, all_of(features_somerstat_housing_2))
# 
# # Home Value to Income: right-skewed
# hist(cities$home_value_to_income) 
# cities %<>% mutate(log_home_val_to_income = log(home_value_to_income))
# hist(cities$log_home_val_to_income) 
# 
# hist(cities$homeownership_rate)
# 
# hist(cities$pct_rent_gt30)
# 
# # Percent of Metro-Area Population: right-skewed
# hist(cities$pct_metro_area_pop)
# cities %<>% mutate(log_pct_metro_area_pop+1 = log(pct_metro_area_pop))
# hist(cities$log_pct_metro_area_pop)
# 
# hist(cities$median_home_valueE)
# cities %<>% mutate(log_median_home_value = log(median_home_valueE))
# hist(cities$log_median_home_value)
# 
# hist(cities$housing_units_per_sqmi)
# cities %<>% mutate(log_housing_units_per_sqmi = log(housing_units_per_sqmi))
# hist(cities$log_housing_units_per_sqmi)

## Scale ####
cities <- cities_no_area_cols_no_med_yr_moved

# Which columns have 0 variance? Should be none
zero_var_cols <- names(cities)[sapply(cities, function(x) is.numeric(x) && var(x, na.rm = TRUE) == 0)]

city_names <- cities$city

# Drop the 2 nonnumeric cols (city, metro_acs5_2023)
cities <- cities %>% select(where(is.numeric))

# Scale the data
cities_scaled <- cities %>%
  #select(-city, -metro_acs5_2023) %>%
  # select(-city, -home_value_to_income, -pct_metro_area_pop, 
  #        -median_home_valueE, -housing_units_per_sqmi) %>%
  scale()



# FEAT SEL. ======= ####
## Correlation ####
#feature_corr(as_tibble(cities_scaled) %>% mutate(NAME = city_names))

# cor_matrix <- cor(cities_scaled)
# too_correlated <- findCorrelation(cor_matrix, cutoff = 0.99)  # caret
# cities_scaled <- cities_scaled[, -too_correlated]



# ## Variance ####
# top_n_var <- 100
# #threshold_var <- 
# cities_top_n_var <- cities %>%
#   summarise(across(everything(), var)) %>%
#   pivot_longer(everything(), names_to = "feature", values_to = "variance") %>%
#   arrange(desc(variance)) %>%
#   slice_head(n = top_n_var) %>%
#   pull(feature)
# 
# cities_top_n_var_scaled <- cities %>%
#   select(all_of(cities_top_n_var)) %>%
#   mutate(across(everything(), scale))
# 
# cities_scaled <- cities_top_n_var_scaled


## Dimension Reduction ####
### PCA ####
pca_out <- prcomp(cities_scaled, scale. = TRUE)
pca_df_full <- get_pca_df_nc(pca_out, cities_scaled, city_names, 10)

#### Scree ####
fviz_eig(pca_out)

#### Low-Var? ####
ggpairs(pca_df_full[,1:10])

#pca_df <- get_pca_df(pca_out, cities_scaled, city_names, clustering_kmeans)
#fviz_pca_var(pca_out)



### kPCA ####
#### rbfdot ####
kpca_rbf_results <- kpca(~., data = as.data.frame(cities_scaled), kernel = "rbfdot")
kpca_rbf_rotated <- rotated(kpca_rbf_results)
kpca_rbf_df <- get_kpca_df_nc(kpca_rbf_rotated, city_names, 10)
ggpairs(kpca_rbf_df[,1:10])

# Distance Matrix ####
city_dists <- get_dist(cities_scaled)

# PRE-CLUST. ====== ####
## PCA Variance Check ####
# Low intrinsic dimensionality, better for clustering
# TODO
fviz_eig(pca_out)
cumsum((pca_out$sdev^2)/sum((pca_out$sdev^2))*100)

## Pairwise Distance Distribution ####
distance_distr_check(cities_scaled)

## Hopkins Statistic ####
hopkins::hopkins(cities_scaled)


# CLUSTER ======== ####
## Number of Clusters ####
### k-means ####
res <- NbClust(pca_df_full[,1:3], 
               distance = "euclidean", 
               min.nc = 2, 
               max.nc = 10, 
               method = "kmeans", 
               index = "all") # 2

res <- NbClust(kpca_rbf_df[,1:3], 
               distance = "euclidean", 
               min.nc = 2, 
               max.nc = 10, 
               method = "kmeans", 
               index = "all") # 4


### hclust ####
res <- NbClust(pca_df_full[,1:3], 
               distance = "euclidean", 
               min.nc = 2, 
               max.nc = 10, 
               method = "ward.D", 
               index = "all") # 5

res <- NbClust(kpca_rbf_df[,1:3], 
               distance = "euclidean", 
               min.nc = 2, 
               max.nc = 10, 
               method = "ward.D", 
               index = "all") # 4

res <- NbClust(pca_df_full[,1:3], 
               distance = "euclidean", 
               min.nc = 2, 
               max.nc = 10, 
               method = "ward.D2", 
               index = "all") # 2

res <- NbClust(kpca_rbf_df[,1:3], 
               distance = "euclidean", 
               min.nc = 2, 
               max.nc = 10, 
               method = "ward.D2", 
               index = "all") # 4


res <- NbClust(pca_df_full[,1:3], 
               distance = "euclidean", 
               min.nc = 2, 
               max.nc = 10, 
               method = "single", 
               index = "all") # 2

res <- NbClust(kpca_rbf_df[,1:3], 
               distance = "euclidean", 
               min.nc = 2, 
               max.nc = 10, 
               method = "single", 
               index = "all") # 2

res <- NbClust(pca_df_full[,1:3], 
               distance = "euclidean", 
               min.nc = 2, 
               max.nc = 10, 
               method = "average", 
               index = "all") # 2

res <- NbClust(kpca_rbf_df[,1:3], 
               distance = "euclidean", 
               min.nc = 2, 
               max.nc = 10, 
               method = "average", 
               index = "all") # 5

res <- NbClust(pca_df_full[,1:3], 
               distance = "euclidean", 
               min.nc = 2, 
               max.nc = 10, 
               method = "complete", 
               index = "all") # 2

res <- NbClust(kpca_rbf_df[,1:3], 
               distance = "euclidean", 
               min.nc = 2, 
               max.nc = 10, 
               method = "complete", 
               index = "all") # 3

# PCA suggestions: 2, 5, / 2, 2, 2, 2
# kPCA suggestions: 4, 4, / 4, 2, 5, 3

## k-means ####

### PCA-Space ####
# TODO: Plot multiple silhouette plots side by side for different k to
# also get the view of the (ideally) cleaver-shaped plot for the best k

set.seed(2025)
k <- 2
clustering_kmeans <- kmeans(pca_df_full[,1:3],
                          centers = k,
                          nstart = 25)

clustered_data <- tibble(
  Name = city_names,
  Cluster = as.factor(clustering_kmeans$cluster) # optionally include original data
)

### kPCA-Space (rbf) ####
set.seed(2025)
k <- 4
clustering_kmeans_kpca <- kmeans(kpca_rbf_df[,1:3],
                            centers = k,
                            nstart = 25)

clustered_data_kpca <- tibble(
  Name = city_names,
  Cluster = as.factor(clustering_kmeans_kpca$cluster) # optionally include original data
)

# # Silhouette
# dev.off()
# plot(silhouette(clustering_kmeans$cluster, get_dist(cities_scaled)), col=1:k, border=NA)
# sil_k <- silhouette(clustering_kmeans$cluster, get_dist(cities_scaled))

## hclust ####
### PCA-Space ####
hclusters <- hcluster(pca_df_full[,1:3], 2, "ward.D")

clustered_data_hc_pca <- tibble(
  Name = city_names,
  Cluster = as.factor(hclusters) # optionally include original data
)

#plot(silhouette(hclusters, dist(cities_scaled)), col=1:2, border=NA)

### kPCA-Space (rbf) ####
hclusters_kpca <- hcluster(kpca_rbf_df[,1:3], 4, "ward.D")

clustered_data_hc_kpca <- tibble(
  Name = city_names,
  Cluster = as.factor(hclusters) # optionally include original data
)


## dbscan ####
### PCA-Space ####

db <- dbscan(pca_df_full[,1:3], eps = 2, MinPts = 12)

## HDBScan ####
### PCA-Space ####
minPts_values <- 3:20
scores <- data.frame(minPts = numeric(), num_clusters = numeric())

for (minPts in minPts_values) {
  hdb <- hdbscan(pca_df_full[,1:3], minPts = minPts)
  scores <- rbind(scores, data.frame(minPts = minPts, num_clusters = max(hdb$cluster)))
}

hdb <- hdbscan(pca_df_full[,1:3], minPts = 9)

#### kPCA-Space (rbf) ####
minPts_values <- 3:20
scores <- data.frame(minPts = numeric(), num_clusters = numeric())

for (minPts in minPts_values) {
  hdb <- hdbscan(kpca_rbf_df[,1:3], minPts = minPts)
  scores <- rbind(scores, data.frame(minPts = minPts, num_clusters = max(hdb$cluster)))
}

hdb_kpca <- hdbscan(kpca_rbf_df[,1:3], minPts = 9)

# EVALUATE ======= ####
## Internal ####
### Silhouette ####
#### PCA-Space ####
dev.off()
plot(silhouette(clustering_kmeans$cluster, get_dist(pca_df_full[,1:3])), col=1:2, border=NA) # 2
dev.off()
plot(silhouette(hclusters, get_dist(pca_df_full[,1:3])), col=1:2, border=NA) # 2
dev.off()
plot(silhouette(hdb$cluster, get_dist(pca_df_full[,1:3])), col=1:3, border=NA) # 3?
dev.off()

#### kPCA-Space (rbf) ####
dev.off()
plot(silhouette(clustering_kmeans_kpca$cluster, get_dist(kpca_rbf_df[,1:3])), col=1:4, border=NA) # 4
dev.off()
plot(silhouette(hclusters_kpca, get_dist(kpca_rbf_df[,1:3])), col=1:4, border=NA) # 4
dev.off()
plot(silhouette(hdb_kpca$cluster, get_dist(kpca_rbf_df[,1:3])), col=1:7, border=NA) # 6?
dev.off()

# plot(silhouette(hclusters, dist(cities_scaled)), col=1:3, border=NA)
# plot(silhouette(db$cluster, get_dist(cities_scaled)), , col=1:max(db$cluster), border=NA)

## Stability ####

# COMPARE ======= ####

# VISUALIZE ======= ####
city_choice = "Somerville city, Massachusetts"

## PCA-Space ####
plot_pca_3d(city_choice, pca_out, clustering_kmeans$cluster)
plot_pca_3d(city_choice, pca_out, hclusters)
plot_pca_3d(city_choice, pca_out, hdb_kpca$cluster)

# For Streamlit
# fig_kmeans_pca_3d <- plot_pca_3d(city_choice, pca_out, clustering_kmeans$cluster)
# saveWidget(fig_kmeans_pca_3d, "./html/kmeans_pca_3d.html", selfcontained = TRUE)
# plot_clustering(pca_df)

## kPCA-Space ####


kpca_rbf_df %<>%
  mutate(highlight = case_when(name == city_choice ~ "highlight",
                               .default = "normal"))

### k-means ####
# TODO: Split this into function
kpca_rbf_df['cluster'] <- clustering_kmeans_kpca$cluster

kp <- plot_ly(kpca_rbf_df, x = ~kp1, y = ~kp2, z = ~kp3,
              type = "scatter3d", mode = "markers",
              color = ~as.factor(cluster), text = ~name, marker = list(size=4))

kp <- add_trace(kp, data = kpca_rbf_df %>% filter(highlight == "highlight"),
                x = ~kp1, y = ~kp2, z = ~kp3,
                type = "scatter3d",
                mode = "markers",
                text = ~name,
                marker = list(size=10, color="yellow"))

kp %>% layout(title = "3d kPCA Plot with Highlight")
kp

### hclust ####
# TODO: Fix colors
kpca_rbf_df['cluster'] <- hclusters

kp <- plot_ly(kpca_rbf_df, x = ~kp1, y = ~kp2, z = ~kp3,
              type = "scatter3d", mode = "markers",
              color = ~as.factor(cluster), text = ~name, marker = list(size=4))

kp <- add_trace(kp, data = kpca_rbf_df %>% filter(highlight == "highlight"),
                x = ~kp1, y = ~kp2, z = ~kp3,
                type = "scatter3d",
                mode = "markers",
                text = ~name,
                marker = list(size=10, color="yellow"))

kp %>% layout(title = "3d kPCA Plot with Highlight")
kp

### hdbscan ####
kpca_rbf_df['cluster'] <- hdb_kpca$cluster

kp <- plot_ly(kpca_rbf_df, x = ~kp1, y = ~kp2, z = ~kp3,
              type = "scatter3d", mode = "markers",
              color = ~as.factor(cluster), text = ~name, marker = list(size=4))

kp <- add_trace(kp, data = kpca_rbf_df %>% filter(highlight == "highlight"),
                x = ~kp1, y = ~kp2, z = ~kp3,
                type = "scatter3d",
                mode = "markers",
                text = ~name,
                marker = list(size=10, color="yellow"))

kp %>% layout(title = "3d kPCA Plot with Highlight")
kp

# PEERS ========== ####
## PCA-Space ####

pca_df_full %<>% mutate(cluster_km = clustering_kmeans$cluster,
                        cluster_hc = hclusters,
                        cluster_hdb = hdb$cluster)

chosen_city_coords <- pca_df_full %>% filter(name == city_choice)

### k-means ####
city_cluster_km <- pca_df_full %>% filter(name == city_choice) %>% pull(cluster_km)
peer_cluster_km <- pca_df_full %>% filter(cluster_km == city_cluster_km)


pca_peer_cluster <- peer_cluster_km %>%
  rowwise() %>%
  mutate(pca_dist_to_chosen = sqrt(
    (pc1 - chosen_city_coords$pc1)^2 +
      (pc2 - chosen_city_coords$pc2)^2 +
      (pc3 - chosen_city_coords$pc3)^2
  )) %>%
  ungroup()

peer_test <- peer_cluster_km %>% add_distance_to_chosen(city_col = "name")
View(peer_test)

### hclust ####
city_cluster_hc <- pca_df_full %>% filter(name == city_choice) %>% pull(cluster_hc)
peer_cluster_hc <- pca_df_full %>% filter(cluster_hc == city_cluster_hc)


pca_peer_cluster_hc <- peer_cluster_hc %>%
  rowwise() %>%
  mutate(pca_dist_to_chosen = sqrt(
    (pc1 - chosen_city_coords$pc1)^2 +
      (pc2 - chosen_city_coords$pc2)^2 +
      (pc3 - chosen_city_coords$pc3)^2
  )) %>%
  ungroup()

peer_test <- peer_cluster_km %>% add_distance_to_chosen(city_col = "name")
View(peer_test)

### hdb ####
city_cluster_hdb <- pca_df_full %>% filter(name == city_choice) %>% pull(cluster_hdb)
peer_cluster_hdb <- pca_df_full %>% filter(cluster_hdb == city_cluster_hdb)


pca_peer_cluster_hdb <- peer_cluster_hdb %>%
  rowwise() %>%
  mutate(pca_dist_to_chosen = sqrt(
    (pc1 - chosen_city_coords$pc1)^2 +
      (pc2 - chosen_city_coords$pc2)^2 +
      (pc3 - chosen_city_coords$pc3)^2
  )) %>%
  ungroup()

peer_test <- peer_cluster_km %>% add_distance_to_chosen(city_col = "name")
View(peer_test)




## kPCA-Space ####
kpca_rbf_df %<>% mutate(cluster_km = clustering_kmeans_kpca$cluster,
                        cluster_hc = hclusters_kpca,
                        cluster_hdb = hdb_kpca$cluster) %>%
  select(-cluster)

kp_chosen_city_coords <- kpca_rbf_df %>% filter(name == city_choice)

### k-means ####
city_cluster_km_kpca <- kpca_rbf_df %>% filter(name == city_choice) %>% pull(cluster_km)
peer_cluster_km_kpca <- kpca_rbf_df %>% filter(cluster_km == city_cluster_km_kpca)

kpca_peer_cluster_km <- peer_cluster_km_kpca %>%
  rowwise() %>%
  mutate(kpca_dist_to_chosen = sqrt(
    (kp1 - kp_chosen_city_coords$kp1)^2 +
      (kp2 - kp_chosen_city_coords$kp2)^2 +
      (kp3 - kp_chosen_city_coords$kp3)^2
  )) %>%
  ungroup()

peer_test <- kpca_peer_cluster_km %>% add_distance_to_chosen(city_col = "name")
View(peer_test)

### hclust ####
city_cluster_hc_kpca <- kpca_rbf_df %>% filter(name == city_choice) %>% pull(cluster_hc)
peer_cluster_hc_kpca <- kpca_rbf_df %>% filter(cluster_hc == city_cluster_hc_kpca)

kpca_peer_cluster_hc <- peer_cluster_hc_kpca %>%
  rowwise() %>%
  mutate(kpca_dist_to_chosen = sqrt(
    (kp1 - kp_chosen_city_coords$kp1)^2 +
      (kp2 - kp_chosen_city_coords$kp2)^2 +
      (kp3 - kp_chosen_city_coords$kp3)^2
  )) %>%
  ungroup()

peer_test <- kpca_peer_cluster_hc %>% add_distance_to_chosen(city_col = "name")
View(peer_test)

### hdbscan ####
city_cluster_hdb_kpca <- kpca_rbf_df %>% filter(name == city_choice) %>% pull(cluster_hdb)
peer_cluster_hdb_kpca <- kpca_rbf_df %>% filter(cluster_hdb == city_cluster_hdb_kpca)

kpca_peer_cluster_hdb <- peer_cluster_hdb_kpca %>%
  rowwise() %>%
  mutate(kpca_dist_to_chosen = sqrt(
    (kp1 - kp_chosen_city_coords$kp1)^2 +
      (kp2 - kp_chosen_city_coords$kp2)^2 +
      (kp3 - kp_chosen_city_coords$kp3)^2
  )) %>%
  ungroup()

peer_test <- kpca_peer_cluster_hdb %>% add_distance_to_chosen(city_col = "name")
View(peer_test)

#kpca_space_df <- kpca_df %>% select(kp1, kp2, kp3, name, cluster)



# ## Combining Lists ####
# In each case so far, other than kPCA HDBScan, 
# the peer cluster contains nearly all of the data, so it is not yet aiding in filtering
pca_km_top_25 <- pca_peer_cluster %>% arrange(pca_dist_to_chosen) %>% slice(1:25) %>% select(name, pca_dist_to_chosen) #pca_peer_cluster[1:25,] %>% select(name, pca_dist_to_chosen)
pca_hc_top_25 <- pca_peer_cluster_hc %>% arrange(pca_dist_to_chosen) %>% slice(1:25) %>% select(name, pca_dist_to_chosen)
pca_hdb_top_25 <- pca_peer_cluster_hdb %>% arrange(pca_dist_to_chosen) %>% slice(1:25) %>% select(name, pca_dist_to_chosen)

# KPCA
kpca_km_top_25 <- kpca_peer_cluster_km %>% arrange(kpca_dist_to_chosen) %>% slice(1:25) %>% select(name, kpca_dist_to_chosen) #pca_peer_cluster[1:25,] %>% select(name, pca_dist_to_chosen)
kpca_hc_top_25 <- kpca_peer_cluster_hc %>% arrange(kpca_dist_to_chosen) %>% slice(1:25) %>% select(name, kpca_dist_to_chosen)
kpca_hdb_top_25 <- kpca_peer_cluster_hdb %>% arrange(kpca_dist_to_chosen) %>% slice(1:25) %>% select(name, kpca_dist_to_chosen)


peers <- c(pca_km_top_25$name, pca_hc_top_25$name, pca_hdb_top_25$name, kpca_km_top_25$name, kpca_hc_top_25$name, kpca_hdb_top_25$name)
length(unique(peers))

peer_intersection <- Reduce(intersect, list(pca_km_top_25$name, 
                                            pca_hc_top_25$name, 
                                            pca_hdb_top_25$name, 
                                            kpca_km_top_25$name, 
                                            kpca_hc_top_25$name, 
                                            kpca_hdb_top_25$name))

peer_intersection

# RUN APP =========== ####
# You need to set your_streamlit_path to the absolute path of your installation of streamlit
# source("./scripts/streamlit.R")
# py_require("streamlit")
# py_run_string("
# import os
# import streamlit
# 
# os.system('streamlit run r_plot_streamlit.py')
# ")









