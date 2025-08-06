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
  gifski, webshot,
  crayon, uwot,
  clusterCrit
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
# data$raw$pre_pcit <- get_pcit_names(data$raw$acs)

# Filter down to the 960 PCIT cities
# data$pcit$all_geogs <- get_pcit_cities_df(places_df_list = data$raw$pre_pcit$places,
#                                           counties_df_list = data$raw$pre_pcit$counties,
#                                           townships_df_list = data$raw$pre_pcit$townships)

# OR: Don't filter down to the 960!
# these are going to be different sets of 1000 places in each year set - instead need a constant 
# list from one of them to be used in the others

pcit_city_set <- read_csv(file = "./data/city_sets/pcit_960.csv") %>% rename(NAME = City)

top_pop_cities <- data$raw$acs$`2013`$places %>% arrange(-tot_popE)
top_pop_cities <- top_pop_cities[1:1920,] %>% select(NAME)

full_city_set <- bind_rows(pcit_city_set, top_pop_cities) %>% distinct()

data$pcit$all_geogs$`2013` <- list(data$raw$acs$`2013`$places %>% filter(NAME %in% full_city_set$NAME),
                                data$raw$acs$`2013`$counties %>% filter(NAME %in% missing_counties),
                                data$raw$acs$`2013`$townships %>% filter(NAME %in% missing_townships)) %>% 
  reduce(bind_rows)

data$pcit$all_geogs$`2018` <- list(data$raw$acs$`2018`$places %>% filter(GEOID %in% data$pcit$all_geogs$`2013`$GEOID),
                                   data$raw$acs$`2018`$counties %>% filter(NAME %in% missing_counties),
                                   data$raw$acs$`2018`$townships %>% filter(NAME %in% missing_townships)) %>% 
  reduce(bind_rows)

data$pcit$all_geogs$`2023` <- list(data$raw$acs$`2023`$places %>% filter(GEOID %in% data$pcit$all_geogs$`2013`$GEOID),
                                   data$raw$acs$`2023`$counties %>% filter(NAME %in% missing_counties),
                                   data$raw$acs$`2023`$townships %>% filter(NAME %in% missing_townships)) %>% 
  reduce(bind_rows)


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

write_csv(data$pcit$all_geogs$`2023`, "./data/incomplete/partial_1957_all_geogs_2023.csv")

## Static ACS Vars ####
data$pcit$all_geogs <- derive_acs_static_all_years(data$pcit$all_geogs) %>%
  drop_geometry()

## Drop cities & features with NAs ####
# Cities if any of the core six variables are NA, cols if they have any NAs
# Otherwise they will break the dimension reduction
# Only needed to be introduced when trying non-960 cities
# Only for the core columns currently being used in the tool
data$pcit$all_geogs$`2013` %<>% filter(if_all(c(pct_rent_gt30, 
                                                pct_metro_area_pop, 
                                                median_home_valueE, 
                                                homeownership_rate, 
                                                home_value_to_income), ~ !is.na(.))) %>%
  select(where(~ !anyNA(.)))

data$pcit$all_geogs$`2018` %<>% filter(if_all(c(pct_rent_gt30, 
                                                pct_metro_area_pop, 
                                                median_home_valueE, 
                                                homeownership_rate, 
                                                home_value_to_income), ~ !is.na(.))) %>%
  select(where(~ !anyNA(.)))

data$pcit$all_geogs$`2023` %<>% filter(if_all(c(pct_rent_gt30, 
                                                pct_metro_area_pop, 
                                                median_home_valueE, 
                                                homeownership_rate, 
                                                home_value_to_income), ~ !is.na(.))) %>%
  select(where(~ !anyNA(.)))

# Use subset of features
data$pcit$all_geogs_six_vars <- use_features(data$pcit$all_geogs, feats$housing$somerstat_2)

# Tag each dataset's columns with year
#data$pcit$all_geogs %<>% add_col_year_suffixes(years)
data$pcit$all_geogs_six_vars %<>% add_col_year_suffixes(years)

## Temporal ACS Vars ####
# Prep one dataset with all of the year-variables
#data$pcit$all_years <- combine_year_dfs(data$pcit$all_geogs)
data$pcit$all_years_six_vars <- combine_year_dfs(data$pcit$all_geogs_six_vars)

# Drop rows with NAs again
# (Some cities are in each year but missing data for some years)
# Only introduced post-960 cities
data$pcit$all_years_six_vars %<>% 
  select(-NAME, -NAME.y) %>% 
  rename(NAME = NAME.x) %>%
  filter(if_all(everything(), ~ !is.na(.)))

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
# Comment out if using 960
# data$pcit$all_years_six_vars %<>% select(-NAME.x, -NAME.y)

# Scale the data
#data$pcit$scaled <- data$pcit$all_years %>% select(where(is.numeric)) %>% scale()
data$pcit$scaled_six_vars <- data$pcit$all_years_six_vars %>% select(where(is.numeric)) %>% scale()

data$preprocessed <- list()

data$preprocessed$six_vars <- data$pcit$scaled_six_vars

# ========================================================================= #
# FEAT SEL. ======= ####
## Dimension Reduction ####
data <- reduce_dimensions(dr_techniques = c("pca", "kpca_rbf", "kpca_poly", "umap"), 
                          data, 
                          n_comp = 3, 
                          city_names = city_names)

# ========================================================================= #
# PRE-CLUST. ====== ####
## Correlation Plot ##
# corrs$six_vars <- cor(data$pcit$all_geogs_six_vars$`2023` %>% select(where(is.numeric)), use = "pairwise.complete.obs", method = "pearson")
# corrs$scaled_six_vars <- cor(data$pcit$scaled_six_vars_df %>% select(where(is.numeric)), use = "pairwise.complete.obs", method = "pearson")
# corrs$pca <- cor(data$reduced$pca$six_vars %>% select(where(is.numeric)), use = "pairwise.complete.obs", method = "pearson")
# corrs$kpca <- cor(data$reduced$kpca_rbf$six_vars %>% select(where(is.numeric)), use = "pairwise.complete.obs", method = "pearson")
# corrs$kpca_poly <- cor(data$reduced$kpca_poly$six_vars %>% select(where(is.numeric)), use = "pairwise.complete.obs", method = "pearson")

# Create the correlation plot
# corrplot(corrs$six_vars, method = "color", type = "upper", 
#          tl.col = "black", tl.srt = 45, tl.cex = 0.5)
# 
# corrplot(corrs$scaled_six_vars, method = "color", type = "upper", 
#          tl.col = "black", tl.srt = 45, tl.cex = 0.5)
# 
# corrplot(corrs$pca, method = "color", type = "upper", 
#          tl.col = "black", tl.srt = 45, tl.cex = 0.5)
# 
# corrplot(corrs$kpca_poly, method = "color", type = "upper", 
#          tl.col = "black", tl.srt = 45, tl.cex = 0.5)

## PCA Var ####
# Low intrinsic dimensionality, better for clustering
# How much variance do the top n components explain cumulatively?
# cumsum((data$reduced$prcomp$all$sdev^2)/sum((data$reduced$prcomp$all$sdev^2))*100)[1:10]
cumsum((data$misc$prcomp$six_vars$sdev^2)/sum((data$misc$prcomp$six_vars$sdev^2))*100)[1:10]

## Pairwise Dist ####
# distance_distr_check(data$pcit$scaled)
# distance_distr_check(data$pcit$scaled_six_vars)
# #distance_distr_check(data$reduced$pca$all[,1:3])
# distance_distr_check(data$reduced$pca$six_vars[,1:10])
# distance_distr_check(data$reduced$kpca_rbf$six_vars[,1:3])
# distance_distr_check(data$reduced$kpca_poly$six_vars[,2:4])


## Hopkins ####
# hopkins::hopkins(as_tibble(data$pcit$scaled)) 
# hopkins::hopkins(data$reduced$pca$all[,1:3])
# hopkins::hopkins(data$reduced$kpca_rbf$all[,1:3])

# hopkins::hopkins(as_tibble(data$pcit$scaled_six_vars)) 
# hopkins::hopkins(data$reduced$pca$six_vars[,1:3])
# hopkins::hopkins(data$reduced$kpca_rbf$six_vars[,1:3])
# hopkins::hopkins(data$reduced$kpca_poly$six_vars[,2:4])

# ========================================================================= #
# CLUSTERING ======== ####
## k (# clusters) ####

indices <- list()
indices$all <- c("kl", "ch", "hartigan", "ccc", "scott", "marriot", "trcovw", "tracew",
  "friedman", "beale", "rubin", "cindex", "db", "silhouette", "duda", "pseudot2", "ratkowsky", "ball", "ptbiserial", "gap", "frey", "mcclain",
  "gamma", "gplus", "tau", "dunn", "hubert", "sdindex", "dindex", "sdbw")
# beale used to work but now doesn't?

indices$fast <- setdiff(indices$all, c("gamma", "gplus", "tau", "hubert", "dindex"))
#indices$safe_kmeans <- c("silhouette", "ch", "hartigan", "db", "ratkowsky")

# Get k for each clustering method in each dimension-reduced space using
# indices$fast

p_load(mclust)

clust <- get_k(c("kmeans", "hc", "hdbscan", "gmm"), 
              data_list = data, 
              indices$fast,
              min_nc = 2,
              max_nc = 10,
              distance = "euclidean")


## Cluster ####
clust <- cluster(data_reduced = data$reduced, 
                 clust = clust, 
                 cluster_methods = c("kmeans", "hc", "hdbscan", "gmm"), 
                 subset_name = "six_vars")

detach("package:fpc", unload = TRUE, character.only = TRUE)
#detach("package:mclust", unload = TRUE, character.only = TRUE)


# ========================================================================= #
# EVALUATE ======= ####

# Get clustering assignments in a slightly different format
clusterings <- build_clusterings(clust)

# clusterings$pca <- list("kmeans" = clust$pca$kmeans$assn, 
#                         "hclust" = clust$pca$hc$assn,
#                         "hdb" = clust$pca$hdb$assn,
#                         "gmm" = clust$pca$gmm$assn)
# 
# clusterings$kpca_rbf <- list("kmeans" = clust$kpca_rbf$kmeans$assn, 
#                              "hclust" = clust$kpca_rbf$hc$assn,
#                              "hdb" = clust$kpca_rbf$hdb$assn,
#                              "gmm" = clust$kpca_rbf$gmm$assn)
# 
# clusterings$kpca_poly <- list("kmeans" = clust$kpca_poly$kmeans$assn, 
#                              "hclust" = clust$kpca_poly$hc$assn,
#                              "hdb" = clust$kpca_poly$hdb$assn,
#                              "gmm" = clust$kpca_poly$gmm$assn)
# 
# clusterings$umap <- list("kmeans" = clust$umap$kmeans$assn, 
#                               "hclust" = clust$umap$hc$assn,
#                               "hdb" = clust$umap$hdb$assn,
#                               "gmm" = clust$umap$gmm$assn)

evals <- list()

# Cohesion: measures within-cluster compactness
# evals$compact_sep_ratios <- c("ch", "wb.ratio", "dunn", "dunn2", "sindex")
# evals$silhouette <- c("avg.silwidth", "min.clus.silwidth", "clus.avg.silwidths")
# evals$dist_correlation <- c("pearsongamma", "g2", "g3")
# evals$info_theory <- c("entropy")
# evals$within_ss <- c("within.cluster.ss")
# 
# evals$all <- unname(unlist(evals[c("compact_sep_ratios", "silhouette", "dist_correlation", "info_theory", "within_ss")]))
# 
# # data$reduced$pca$six_vars %<>% select(where(is.numeric))
# # data$reduced$kpca_rbf$six_vars %<>% select(where(is.numeric))
# # data$reduced$kpca_poly$six_vars %<>% select(where(is.numeric))
# 
# p_load(fpc)
# clust$best <- compare_clusterings(data$reduced, clusterings, evals$all)

# Removed due to NaNs in GMM (Scott Symons, s_dbw) and hclust (Scott Symons)
# Removed due to NaN in kpca poly kmeans: log det ratio (from doing log of negative)
# also: br (-Inf), sil (NaN)
# Removed due to -Infs in kpca_rbf: gamma
# Removed because I don't want to overweight Dunn: GDI-everything. 
# Removed because of error: sep
# Removed because of NA: g2, g3
# Slow (per clusterCrit): 
# Maybe I can set the -infs to -99999 and call it a day? But then the scaling is off? Or not because itll be 0
# Maybe I set them to min-1?
# Why do none of GDI21, 31, 32 match Dunn2?
# What is the "sil" from clusterCrit?

indices$newcomp <- c("avg_sil", "min_avg_sil", "sil", "s_index", # silhouette-related
                     "ch", "dunn", "dunn2",  "ball_hall", "br", "wcss", 
                     "entropy", "c", "db", "det_ratio", "gamma", "gplus", "ksq_detw",
                     "log_det_ratio", "log_ss_ratio", "mcr", "pbm", "ptbi",
                     "rt", "rl", "sd_scat", "sd_dis", 
                     "tau", "trace_w", "trace_wib", "wb_ratio", "wg", "xb")

# results <- evaluate_clusterings(data$reduced$pca$six_vars, clusterings$pca, indices$newcomp)
# normed <- normalize_eval_df(results)
# judge <- normed %>%
#   rowwise() %>%
#   mutate(sum = sum(c_across(everything()), na.rm = TRUE)) %>%
#   mutate(avg = mean(c_across(everything()), na.rm = TRUE)) %>%
#   mutate(median = median(c_across(everything()), na.rm = TRUE)) %>%
#   ungroup() %>%
#   mutate(clustering = rownames(results)) %>%
#   select(clustering, sum, avg, median)

#detach("package:fpc", unload = TRUE, character.only = TRUE)
detach("package:mclust", unload = TRUE, character.only = TRUE)
eval_results <- evaluate_all_clusterings(data$reduced, clusterings, indices$newcomp)
eval_normalized <- normalize_all_eval_results(eval_results)
eval_aggregates <- aggregate_normalized_eval_indices(eval_normalized)
clust$best <- get_best_clusterings(eval_aggregates, clusterings, use = "median")

# eval_pca <- prcomp(normed, scale. = FALSE)
# pca$rotation[, 1]
# fviz_eig(pca)
# fviz_pca_biplot(pca)

# clust$best$pca <- compare_clusterings_majority_vote(data$reduced$pca$six_vars,
#                                                      clusterings$pca,
#                                                      evals$all)
# 
# clust$best$kpca_rbf <- compare_clusterings_majority_vote(data$reduced$kpca_rbf$six_vars,
#                                                          clusterings$kpca_rbf,
#                                                          evals$all)
# 
# 
# clust$best$kpca_poly <- compare_clusterings_majority_vote(data$reduced$kpca_poly$six_vars,
#                                                          clusterings$kpca_poly,
#                                                          evals$all)


# ========================================================================= #
# VISUALIZE ======= ####
## PCA-Space ####
data$reduced$pca$six_vars['cluster'] <- as.character(clust$best$pca$assn)

# plot_pca_3d(city_choice = city_choice, 
#             pca_out = data$reduced$pca$six_vars, 
#             clusters = clust$best$pca$assn, 
#             city_names, 
#             save_dir = "./html")
# 
# make_pca_3d_gif(city_choice = city_choice,
#                 pca_out = data$reduced$pca$six_vars,
#                 clusters = clust$best$pca$assn,
#                 city_names = city_names,
#                 gif_dir = "./gifs/pca")

## kPCA-RBF-Space ####
data$reduced$kpca_rbf$six_vars %<>% add_highlight(city_choice = city_choice)
data$reduced$kpca_rbf$six_vars['cluster'] <- as.character(clust$best$kpca_rbf$assn)

# plot_kpca_3d(data$reduced$kpca_rbf$six_vars,
#              #pc_cols = c("kp1", "kp2", "kp3"),
#              cluster_col = "cluster", 
#              text_col = "City",
#              highlight_col = "highlight",
#              highlight_value = "highlight",
#              save_path = "./html/kpca_best_3d.html")
# 
# make_kpca_3d_gif(data$reduced$kpca_rbf$six_vars,
#                  #pc_cols = c("kp1", "kp2", "kp3"),
#                  cluster_col = "cluster",
#                  text_col = "City",
#                  highlight_col = "highlight",
#                  highlight_value = "highlight",
#                  gif_dir = "./gifs/kpca")

## kPCA-Poly-Space ####
data$reduced$kpca_poly$six_vars['cluster'] <- as.character(clust$best$kpca_poly$assn)
data$reduced$kpca_poly$six_vars %<>% add_highlight(city_choice = city_choice)

# plot_kpca_3d(data$reduced$kpca_poly$six_vars,
#              #pc_cols = c("V1", "V2", "V3"),
#              cluster_col = "cluster", 
#              text_col = "City",
#              highlight_col = "highlight",
#              highlight_value = "highlight",
#              save_path = "./html/kpca_poly_best_3d.html")
# 
# make_kpca_3d_gif(data$reduced$kpca_poly$six_vars,
#                  #pc_cols = c("V1", "V2", "V3"),
#                  cluster_col = "cluster",
#                  text_col = "City",
#                  highlight_col = "highlight",
#                  highlight_value = "highlight",
#                  gif_dir = "./gifs/kpca_poly")

## UMAP-Space ####
data$reduced$umap$six_vars['cluster'] <- as.character(clust$best$umap$assn)

# plot_kpca_3d(data$reduced$umap$six_vars,
#              #pc_cols = c("V1", "V2", "V3"),
#              cluster_col = "cluster", 
#              text_col = "City",
#              highlight_col = "highlight",
#              highlight_value = "highlight",
#              save_path = "./html/umap_best_3d.html")
# 
# make_kpca_3d_gif(data$reduced$umap$six_vars,
#                  #pc_cols = c("V1", "V2", "V3"),
#                  cluster_col = "cluster",
#                  text_col = "City",
#                  highlight_col = "highlight",
#                  highlight_value = "highlight",
#                  gif_dir = "./gifs/umap")

# ========================================================================= #
# PEERS ========= ####

my_city <- list()
peers <- list()

peers <- get_peers(data = data, chosen_city = "Somerville city, Massachusetts")
#peers <- get_peers(data = data, chosen_city = "Jersey City city, New Jersey")
#peers <- get_peers(data = data, chosen_city = "Bartlett city, Texas")

## PCA-Space ####
# my_city$row$pca <- data$reduced$pca$six_vars %>% filter(City == city_choice)
# my_city$cluster_assn$pca <- my_city$row$pca %>% pull(cluster)
# my_city$cluster_members$pca <- data$reduced$pca$six_vars %>% filter(cluster == my_city$cluster_assn$pca)
# peers$pca <- my_city$cluster_members$pca %>% add_distance_to_chosen(city_col = "City") %>% select(City, Dissimilarity)
# 
# ## kPCA-Space (rbf) ####
# my_city$row$kpca_rbf <- data$reduced$kpca_rbf$six_vars %>% filter(City == city_choice)
# my_city$cluster_assn$kpca_rbf <- my_city$row$kpca_rbf %>% pull(cluster)
# my_city$cluster_members$kpca_rbf <- data$reduced$kpca_rbf$six_vars %>% filter(cluster == my_city$cluster_assn$kpca_rbf)
# peers$kpca_rbf <- my_city$cluster_members$kpca_rbf %>% add_distance_to_chosen(city_col = "City") %>% select(City, Dissimilarity)
# 
# ## kPCA-Space (poly) ####
# my_city$row$kpca_poly <- data$reduced$kpca_poly$six_vars %>% filter(City == city_choice)
# my_city$cluster_assn$kpca_poly <- my_city$row$kpca_poly %>% pull(cluster)
# my_city$cluster_members$kpca_poly <- data$reduced$kpca_poly$six_vars %>% filter(cluster == my_city$cluster_assn$kpca_poly)
# peers$kpca_poly <- my_city$cluster_members$kpca_poly %>% add_distance_to_chosen(city_col = "City") %>% select(City, Dissimilarity)
# 
# ## UMAP ####
# my_city$row$umap <- data$reduced$umap$six_vars %>% filter(City == city_choice)
# my_city$cluster_assn$umap <- my_city$row$umap %>% pull(cluster)
# my_city$cluster_members$umap <- data$reduced$umap$six_vars %>% filter(cluster == my_city$cluster_assn$umap)
# peers$umap <- my_city$cluster_members$umap %>% add_distance_to_chosen(city_col = "City") %>% select(City, Dissimilarity)


# ## Combining Lists ####
# top_n_of_ea <- 55
# intersect_3 <- Reduce(intersect, list(
#   head(dplyr::arrange(peers$pca, Dissimilarity)$City, top_n_of_ea),
#   head(dplyr::arrange(peers$kpca_rbf, Dissimilarity)$City, top_n_of_ea),
#   head(dplyr::arrange(peers$kpca_poly, Dissimilarity)$City, top_n_of_ea),
#   head(dplyr::arrange(peers$umap, Dissimilarity)$City, top_n_of_ea)
# ))
# intersect_3

top_n <- find_shared_peers(10, c("pca", "kpca_rbf", "kpca_poly", "umap"), peers)

#detach("package:fpc", unload = TRUE, character.only = TRUE)
#detach("package:mclust", unload = TRUE, character.only = TRUE)

dissimilarities <- combine_similarity_rankings(peers, 
                            method_names = c("pca", "kpca_rbf", "kpca_poly", "umap"), 
                            agg_method = "mean")



# Roughly what the final table should be, but somehow condense the full info?
# Also tables are a bad way for this to be presented.
#View(data$pcit$all_geogs_six_vars$`2023` %>% select(-GEOID) %>% filter(NAME %in% intersect_3))

dir.create("./data/out")
write_peer_csvs(c("pca", "kpca_rbf", "kpca_poly", "umap"))
# write_csv(peers$pca %>% arrange(Dissimilarity), "./data/out/peers_pca.csv")
# write_csv(peers$kpca_rbf %>% arrange(Dissimilarity), "./data/out/peers_kpca_rbf.csv")
# write_csv(peers$kpca_poly %>% arrange(Dissimilarity), "./data/out/peers_kpca_poly.csv")
# write_csv(peers$umap %>% arrange(Dissimilarity), "./data/out/peers_umap.csv")
write_csv(dissimilarities %>% arrange(Aggregated), "./data/out/dissim.csv")

write_data_csvs(c("pca", "kpca_rbf", "kpca_poly", "umap"))
# write_csv(data$reduced$pca$six_vars, "./data/out/data_pca.csv")
# write_csv(data$reduced$kpca_rbf$six_vars, "./data/out/data_kpca_rbf.csv")
# write_csv(data$reduced$kpca_poly$six_vars, "./data/out/data_kpca_poly.csv")
# write_csv(data$reduced$umap$six_vars, "./data/out/data_umap.csv")

# ========================================================================= #
# RUN APP =========== ####
# You need to set your_streamlit_path to the absolute path of your installation of streamlit
source("./scripts/streamlit.R")
py_require("streamlit")
py_require("pandas")
py_require("plotly")
py_require("scipy")
py_run_string("
import os
import streamlit

os.system('streamlit run ./scripts/app.py')
")










