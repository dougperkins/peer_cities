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
  plotly, htmlwidgets, 
  reticulate, sf,
  tictoc, furrr,
  progressr, dbscan,
  cluster, fpc,
  hopkins, kernlab,
  Rdimtools, NbClust,
  caret, GGally,
  zeallot, lwgeom,
  rmapshaper, testthat,
  fs, htmltools,
  gifski, webshot
)

# Global settings
progressr::handlers(global = TRUE)
options(tigris_use_cache = TRUE)
options(scipen = 999)
set.seed(2025)
# Set your Census API key for getting ACS data
#census_api_key(API_KEY, install = TRUE)




city_choice = "Somerville city, Massachusetts"

# ========================================================================= #
# SOURCE ======== ####
source("./scripts/constants.R")
load_scripts()

# ========================================================================= #
# LOAD DATA ====== ####
data <- list()
dists <- list()
clust <- list()
corrs <- list()

## PCIT ####
data$raw$pcit <- load_pcit_df()

## ACS ####
# The ACS doesn't recommend comparing overlapping 5-year datasets
years <- c("2013", "2018", "2023")

# Load ACS data across years
data$raw$acs <- load_acs_data_by_year(
  years = years,
  variable_list = all_variables,
  vars_metro = metro_vars
)

# Pull metros into a different list
data <- data %>% restructure_metros_list()

# City names from ACS -> PCIT format
data$raw$pre_pcit <- get_pcit_names(data$raw$acs)

# Filter down to the 960 PCIT cities
data$pcit$all_geogs <- get_pcit_cities_df(places_df_list = data$raw$pre_pcit$places,
                                          counties_df_list = data$raw$pre_pcit$counties,
                                          townships_df_list = data$raw$pre_pcit$townships)

# Get only the variable estimates; drop all margin of error columns
data$pcit$all_geogs <- map(data$pcit$all_geogs, ~ get_acs_estimates(.x))

# Get list of shared cities across all years (to drop ones not always present)
shared_pcit_geoids <- Reduce(intersect, lapply(data$pcit$all_geogs, function(df) unique(df$GEOID)))

# Get only SHARED PCIT cities for each year to avoid any missing data issues
data <- filter_all_geogs_by_geoids(data, shared_pcit_geoids)

## Tigris (city areas) ####
geogs <- c("places", "counties", "countysubs")
fips$states <- unique(fips_codes$state)
tigris_data <- load_tigris_layers(years = years,
                                  geogs = geogs, 
                                  states = fips$states, 
                                  dir = "./data/tigris")

data$pcit$tigris <- build_all_tigris_pcit(build_ready_input(tigris_data,
                                                            data$pcit$all_geogs,
                                                            years))


## Join ACS to tigris (City Areas)
data <- join_areas_to_data(data)

# ========================================================================= #
# FEAT ENG ======= ####

## % of Metro Area ####
data <- make_spatial(data)
data$pcit$all_geogs <- get_metro_pops_parallel(data$pcit$all_geogs, data$raw$acs_metros)

## Static ACS Vars ####
data$pcit$all_geogs <- derive_acs_static_all_years(data$pcit$all_geogs) %>%
  drop_geometry()
 
data$pcit$all_geogs_six_vars <- use_features(data$pcit$all_geogs, feats$housing$somerstat_2)

# Tag each dataset's columns with year
#data$pcit$all_geogs %<>% add_col_year_suffixes(years)
data$pcit$all_geogs_six_vars %<>% add_col_year_suffixes(years)

## Temporal ACS Vars ####
# Prep one dataset with all of the year-variables
#data$pcit$all_years <- combine_year_dfs(data$pcit$all_geogs)
data$pcit$all_years_six_vars <- combine_year_dfs(data$pcit$all_geogs_six_vars)


# Get difference variables (ie 2023 - 2018)
#data$pcit$all_years <- derive_acs_temporal_all(data$pcit$all_years)
data$pcit$all_years_six_vars <- derive_acs_temporal_all(data$pcit$all_years_six_vars)

# ========================================================================= #
# PREPROCESS ==== ####
## Scale ####
# Which columns have 0 variance? Should be none
#names(data$pcit$all_years)[sapply(data$pcit$all_years, function(x) is.numeric(x) && var(x, na.rm = TRUE) == 0)]
names(data$pcit$all_years)[sapply(data$pcit$all_years_six_vars, function(x) is.numeric(x) && var(x, na.rm = TRUE) == 0)]


city_names <- data$pcit$all_years_six_vars$NAME

# Drop the 2013 and 2018 city names, keep 2023 (some towns became cities)
data$pcit$all_years_six_vars %<>% select(-NAME.x, -NAME.y)

# Scale the data
#data$pcit$scaled <- data$pcit$all_years %>% select(where(is.numeric)) %>% scale()
data$pcit$scaled_six_vars <- data$pcit$all_years_six_vars %>% select(where(is.numeric)) %>% scale()

# ========================================================================= #
# FEAT SEL. ======= ####
## Dimension Reduction ####
### PCA ####
#data$reduced$prcomp$all <- prcomp(data$pcit$scaled, scale. = TRUE)
# data$reduced$pca$all <- get_pca_df_nc(data$reduced$prcomp$all, 
#                                   data$pcit$scaled, city_names, n_pcs = 10)

data$misc$prcomp$six_vars <- prcomp(data$pcit$scaled_six_vars, scale. = TRUE)
data$reduced$pca$six_vars <- get_pca_df_nc(data$misc$prcomp$six_vars, 
                                           data$pcit$scaled_six_vars, city_names, n_pcs = 10)
#### Scree ####
#fviz_eig(data$reduced$prcomp$all)
fviz_eig(data$misc$prcomp$six_vars)

#### Does it cluster on the low-variance PCs?
#ggpairs(data$reduced$pca$all[,1:6])
ggpairs(data$reduced$pca$six_vars[,1:6])


### kPCA ####
#### rbfdot ####
# data$reduced$kpca$all <- get_kpca_df_nc(rotated(kpca(~., data = as.data.frame(data$pcit$scaled), 
#                                                  kernel = "rbfdot")), 
#                                     city_names, 
#                                     n_kpcs = 10, 
#                                     city_choice = city_choice)
# 
# ggpairs(data$reduced$kpca$all[,1:6])

data$reduced$kpca$six_vars <- get_kpca_df_nc(rotated(kpca(~., data = as.data.frame(data$pcit$scaled_six_vars), 
                                                     kernel = "rbfdot")), 
                                        city_names, 
                                        n_kpcs = 10, 
                                        city_choice = city_choice)

ggpairs(data$reduced$kpca$six_vars[,1:6])

#### polynomial ####
data$pcit$scaled_six_vars_df <- as.tibble(data$pcit$scaled_six_vars)
data$pcit$scaled_six_vars_df$NAME <- data$pcit$all_years_six_vars$NAME

data$reduced$kpca_poly$six_vars <- grid_search_kpca(data$pcit$scaled_six_vars_df, 
                                                    degrees = 2:4,
                                                    scales = c(0.5, 1, 2),
                                                    offsets = 0:2,
                                                    n_components = 3,
                                                    verbose = TRUE)

data$reduced$kpca_poly$six_vars <- as.tibble(rotated(data$reduced$kpca_poly$six_vars$best_model))
ggpairs(data$reduced$kpca_poly$six_vars[,1:6])
data$reduced$kpca_poly$six_vars$NAME <- data$pcit$all_years_six_vars$NAME

data$reduced$kpca_poly$six_vars %<>% select(NAME, V1, V2, V3)

# Distance Matrix ####
# dists$scaled <- get_dist(data$pcit$scaled)
dists$scaled_six_vars <- get_dist(data$pcit$scaled_six_vars)
dists$pca <- get_dist(data$reduced$pca$six_vars %>% select(-name))
dists$kpca <- get_dist(data$reduced$kpca$six_vars)
dists$kpca_poly <- get_dist(data$reduced$kpca_poly$six_vars)

# ========================================================================= #
# PRE-CLUST. ====== ####
## Correlation Plot ##
corrs$six_vars <- cor(data$pcit$all_geogs_six_vars$`2023` %>% select(where(is.numeric)), use = "pairwise.complete.obs", method = "pearson")
corrs$scaled_six_vars <- cor(data$pcit$scaled_six_vars_df %>% select(where(is.numeric)), use = "pairwise.complete.obs", method = "pearson")
corrs$pca <- cor(data$reduced$pca$six_vars %>% select(where(is.numeric)), use = "pairwise.complete.obs", method = "pearson")
corrs$kpca <- cor(data$reduced$kpca$six_vars %>% select(where(is.numeric)), use = "pairwise.complete.obs", method = "pearson")
corrs$kpca_poly <- cor(data$reduced$kpca_poly$six_vars %>% select(where(is.numeric)), use = "pairwise.complete.obs", method = "pearson")

# Create the correlation plot
corrplot(corrs$pca, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45, tl.cex = 0.5)

## PCA Var ####
# Low intrinsic dimensionality, better for clustering
# How much variance do the top n components explain cumulatively?
# cumsum((data$reduced$prcomp$all$sdev^2)/sum((data$reduced$prcomp$all$sdev^2))*100)[1:10]
cumsum((data$misc$prcomp$six_vars$sdev^2)/sum((data$misc$prcomp$six_vars$sdev^2))*100)[1:10]

## Pairwise Dist ####
# distance_distr_check(data$pcit$scaled)
distance_distr_check(data$pcit$scaled_six_vars)
#distance_distr_check(data$reduced$pca$all[,1:3])
distance_distr_check(data$reduced$pca$six_vars[,1:10])
distance_distr_check(data$reduced$kpca$six_vars[,1:3])
distance_distr_check(data$reduced$kpca_poly$six_vars[,2:4])


## Hopkins ####
# hopkins::hopkins(as_tibble(data$pcit$scaled)) 
# hopkins::hopkins(data$reduced$pca$all[,1:3])
# hopkins::hopkins(data$reduced$kpca$all[,1:3])

hopkins::hopkins(as_tibble(data$pcit$scaled_six_vars)) 
hopkins::hopkins(data$reduced$pca$six_vars[,1:3])
hopkins::hopkins(data$reduced$kpca$six_vars[,1:3])
hopkins::hopkins(data$reduced$kpca_poly$six_vars[,2:4])

# ========================================================================= #
# CLUSTER ======== ####
## Number of Clusters ####
indices <- list()
indices$all <- c("kl", "ch", "hartigan", "ccc", "scott", "marriot", "trcovw", "tracew",
  "friedman", "rubin", "cindex", "db", "silhouette", "duda", "pseudot2",
  "beale", "ratkowsky", "ball", "ptbiserial", "gap", "frey", "mcclain",
  "gamma", "gplus", "tau", "dunn", "hubert", "sdindex", "dindex", "sdbw")
indices$fast <- setdiff(indices$all, c("gamma", "gplus", "tau", "hubert", "dindex"))
#indices$safe_kmeans <- c("silhouette", "ch", "hartigan", "db", "ratkowsky")

### k-means ####
clust$pca$kmeans$n <- get_best_k_multi_index(data$reduced$pca$six_vars[1:10],
                                             indices = indices$fast,
                                             distance = "euclidean",
                                             min_nc = 2,
                                             max_nc = 10,
                                             method = "kmeans") # 2

clust$kpca_rbf$kmeans$n <- get_best_k_multi_index(data$reduced$kpca$six_vars[1:3],
                                                   indices = indices$fast,
                                                   distance = "euclidean",
                                                   min_nc = 2,
                                                   max_nc = 10,
                                                   method = "kmeans") # 2

clust$kpca_poly$kmeans$n <- get_best_k_multi_index(data$reduced$kpca_poly$six_vars[2:4],
                                                   indices = indices$fast,
                                                   distance = "euclidean",
                                                   min_nc = 2,
                                                   max_nc = 10,
                                                   method = "kmeans") # 5


### hclust ####
clust$pca$hc$n <- get_best_k_multi_index(data$reduced$pca$six_vars[,1:10],
                                         indices = indices$fast,
                                         distance = "euclidean",
                                         min_nc = 2,
                                         max_nc = 10,
                                         method = "ward.D2") # 2 (tested 2:40)

clust$kpca_rbf$hc$n <- get_best_k_multi_index(data$reduced$kpca$six_vars[1:3],
                                              indices = indices$fast,
                                              distance = "euclidean",
                                              min_nc = 2,
                                              max_nc = 10,
                                              method = "ward.D2") # 4 (inst. 7), 3 with 2:40

clust$kpca_poly$hc$n <- get_best_k_multi_index(data$reduced$kpca_poly$six_vars[2:4],
                                               indices = indices$fast,
                                                distance = "euclidean",
                                                min_nc = 2,
                                                max_nc = 10,
                                                method = "ward.D2") # 7, 3 with 2:40


## Run Clustering ####
### kmeans ####
clust$pca$kmeans$assn <- kmeans(data$reduced$pca$six_vars[,1:10],
                                clust$pca$kmeans$n,
                                nstart = 25)$cluster

clust$kpca_rbf$kmeans$assn <- kmeans(data$reduced$kpca$six_vars[,1:3],
                                clust$kpca_rbf$kmeans$n,
                                nstart = 25)$cluster

clust$kpca_poly$kmeans$assn <- kmeans(data$reduced$kpca_poly$six_vars[,2:4],
                                     clust$kpca_poly$kmeans$n,
                                     nstart = 25)$cluster

### hclust ####
#clust$raw$hc$assn <- hcluster(data$pcit$scaled_six_vars, 2, "ward.D2")
clust$pca$hc$assn <- hcluster(data$reduced$pca$six_vars[,1:3], clust$pca$hc$n, "ward.D2")
clust$kpca_rbf$hc$assn  <- hcluster(data$reduced$kpca$six_vars[,1:3], clust$kpca_rbf$hc$n, "ward.D2")
clust$kpca_poly$hc$assn <- hcluster(data$reduced$kpca_poly$six_vars[,2:4], clust$kpca_poly$hc$n, "ward.D2")


### HDBScan ####
clust$pca$hdb <- grid_search_hdbscan_votes(data$reduced$pca$six_vars,
                                           minPts_values = seq(5, 30, by = 1))

clust$kpca_rbf$hdb <- grid_search_hdbscan_votes(data$reduced$kpca$six_vars,
                                                minPts_values = seq(5, 30, by = 1))

clust$kpca_poly$hdb <- grid_search_hdbscan_votes(data$reduced$kpca_poly$six_vars,
                                                minPts_values = seq(5, 30, by = 1))



# ========================================================================= #
# EVALUATE ======= ####

clusterings <- list()

clusterings$pca <- list("kmeans" = clust$pca$kmeans$assn, 
                        "hclust" = clust$pca$hc$assn,
                        "hdb" = clust$pca$hdb$assn)

clusterings$kpca_rbf <- list("kmeans" = clust$kpca_rbf$kmeans$assn, 
                             "hclust" = clust$kpca_rbf$hc$assn,
                             "hdb" = clust$kpca_rbf$hdb$assn)

clusterings$kpca_poly <- list("kmeans" = clust$kpca_poly$kmeans$assn, 
                             "hclust" = clust$kpca_poly$hc$assn,
                             "hdb" = clust$kpca_poly$hdb$assn)


evals <- list()

# Cohesion: measures within-cluster compactness
evals$compact_sep_ratios <- c("ch", "wb.ratio", "dunn", "dunn2", "sindex")
evals$silhouette <- c("avg.silwidth", "min.clus.silwidth", "clus.avg.silwidths")
evals$dist_correlation <- c("pearsongamma", "g2", "g3")
evals$info_theory <- c("entropy")
evals$within_ss <- c("within.cluster.ss")

evals$all <- unname(unlist(evals[c("compact_sep_ratios", "silhouette", "dist_correlation", "info_theory", "within_ss")]))

clust$best$pca <- compare_clusterings_majority_vote(data$reduced$pca$six_vars[,1:3],
                                                     clusterings$pca,
                                                     evals$all)

clust$best$kpca_rbf <- compare_clusterings_majority_vote(data$reduced$kpca$six_vars[,1:3],
                                                         clusterings$kpca_rbf,
                                                         evals$all)


clust$best$kpca_poly <- compare_clusterings_majority_vote(data$reduced$kpca_poly$six_vars[,2:4],
                                                         clusterings$kpca_poly,
                                                         evals$all)


# ========================================================================= #
# VISUALIZE ======= ####
## PCA-Space ####
data$reduced$pca$six_vars['cluster'] <- as.character(clust$best$pca$assn)

plot_pca_3d(city_choice = city_choice, 
            pca_out = data$reduced$pca$six_vars, 
            clusters = clust$best$pca$assn, 
            city_names, 
            save_dir = "./html")

make_pca_3d_gif(city_choice = city_choice,
                pca_out = data$reduced$pca$six_vars,
                clusters = clust$best$pca$assn,
                city_names = city_names,
                gif_dir = "./gifs/pca")

## kPCA-RBF-Space ####
data$reduced$kpca$six_vars %<>% add_highlight(name_col = "name", city_choice = city_choice)
data$reduced$kpca$six_vars['cluster'] <- as.character(clust$best$kpca_rbf$assn)

plot_kpca_3d(data$reduced$kpca$six_vars,
             pc_cols = c("kp1", "kp2", "kp3"),
             cluster_col = "cluster", 
             text_col = "name",
             highlight_col = "highlight",
             highlight_value = "highlight",
             save_path = "./html/kpca_best_3d.html")

make_kpca_3d_gif(data$reduced$kpca$six_vars,
                 pc_cols = c("kp1", "kp2", "kp3"),
                 cluster_col = "cluster",
                 text_col = "name",
                 highlight_col = "highlight",
                 highlight_value = "highlight",
                 gif_dir = "./gifs/kpca")



## kPCA-Poly-Space ####
data$reduced$kpca_poly$six_vars['cluster'] <- as.character(clust$best$kpca_poly$assn)
data$reduced$kpca_poly$six_vars %<>% add_highlight(city_choice = city_choice)

plot_kpca_3d(data$reduced$kpca_poly$six_vars,
             pc_cols = c("V1", "V2", "V3"),
             cluster_col = "cluster", 
             text_col = "NAME",
             highlight_col = "highlight",
             highlight_value = "highlight",
             save_path = "./html/kpca_poly_best_3d.html")

make_kpca_3d_gif(data$reduced$kpca_poly$six_vars,
                 pc_cols = c("V1", "V2", "V3"),
                 cluster_col = "cluster",
                 text_col = "NAME",
                 highlight_col = "highlight",
                 highlight_value = "highlight",
                 gif_dir = "./gifs/kpca_poly")


# ========================================================================= #
# PEERS ========== ####
my_city <- list()
peers <- list()

## PCA-Space ####
my_city$row$pca <- data$reduced$pca$six_vars %>% filter(name == city_choice)
my_city$cluster_assn$pca <- my_city$row$pca %>% pull(cluster)
my_city$cluster_members$pca <- data$reduced$pca$six_vars %>% filter(cluster == my_city$cluster_assn$pca)
peers$pca <- my_city$cluster_members$pca %>% add_distance_to_chosen(city_col = "name") %>% select(name, distance_to_chosen)

## kPCA-Space (rbf) ####
my_city$row$kpca_rbf <- data$reduced$kpca$six_vars %>% filter(name == city_choice)
my_city$cluster_assn$kpca_rbf <- my_city$row$kpca_rbf %>% pull(cluster)
my_city$cluster_members$kpca_rbf <- data$reduced$kpca$six_vars %>% filter(cluster == my_city$cluster_assn$kpca_rbf)
peers$kpca_rbf <- my_city$cluster_members$kpca_rbf %>% add_distance_to_chosen(city_col = "name") %>% select(name, distance_to_chosen)

## kPCA-Space (poly) ####
my_city$row$kpca_poly <- data$reduced$kpca_poly$six_vars %>% filter(NAME == city_choice)
my_city$cluster_assn$kpca_poly <- my_city$row$kpca_poly %>% pull(cluster)
my_city$cluster_members$kpca_poly <- data$reduced$kpca_poly$six_vars %>% filter(cluster == my_city$cluster_assn$kpca_poly)
peers$kpca_poly <- my_city$cluster_members$kpca_poly %>% add_distance_to_chosen(city_col = "NAME") %>% select(NAME, distance_to_chosen)

# ## Combining Lists ####
top_n_of_ea <- 55
intersect_3 <- Reduce(intersect, list(
  head(dplyr::arrange(peers$pca, distance_to_chosen)$name, top_n_of_ea),
  head(dplyr::arrange(peers$kpca_rbf, distance_to_chosen)$name, top_n_of_ea),
  head(dplyr::arrange(peers$kpca_poly, distance_to_chosen)$NAME, top_n_of_ea)
))
intersect_3

dir.create("./data/out")
write_csv(peers$pca %>% arrange(distance_to_chosen), "./data/out/peers_pca.csv")
write_csv(peers$kpca_rbf %>% arrange(distance_to_chosen), "./data/out/peers_kpca_rbf.csv")
write_csv(peers$kpca_poly %>% arrange(distance_to_chosen), "./data/out/peers_kpca_poly.csv")

# ========================================================================= #
# RUN APP =========== ####
# You need to set your_streamlit_path to the absolute path of your installation of streamlit
source("./scripts/streamlit.R")
py_require("streamlit")
py_require("pandas")
py_run_string("
import os
import streamlit

os.system('streamlit run ./scripts/r_plot_streamlit.py')
")









