# Libraries ####
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
  plotly # plotly: plot_ly 3d
  )

# Load functions & constants ####
source("./scripts/constants.R")
source("./scripts/utils.R")
source("./scripts/acs.R")
source("./scripts/alfin.R")
source("./scripts/fips.R")


# Load Data ####
## PCIT ####
pcit_df <- read_csv("./data/pcit/2024-peer-city-data-full.csv") %>%
  mutate(proj_uid = capitalize_after_underscore(proj_uid))

missing <- pcit_df %>%
  filter(!(proj_uid %in% acs_in_pcit$pcit))

## City Areas ####
areas <- read_csv("./data/area/2010/cities_size.csv") %>%
  rename(NAME = City, area_sqmi = `Land Area`)

## ACS ####
# TODO: save 2010-2023 ACS tables

# Load ACS variable descriptions for 2022 ACS 5-year
acs_var_lookup <- load_variables(2022, "acs5", cache = TRUE)
acs_var_lookup_23 <- load_variables(2023, "acs5", cache = TRUE)
# #

# Load ACS data
acs_data <- load_acs(acs_vars_housing, 2023)
acs_data_counties <- load_acs_county(acs_vars_housing, 2023)
acs_data_townships <- load_acs_townships(acs_vars_housing, 2023)
acs_in_pcit <- load_acs_pcit_960()

acs_pcit_ests <- get_acs_estimates(acs_in_pcit)

acs_pcit_ests %<>%
  left_join(areas, by="NAME")

### Derive Features ####
#cities %<>% derive_features()

acs_pcit_ests %<>% mutate(
  
  home_value_to_income = median_home_valueE/median_household_incomeE,
  
  vacancy_rate = (vacant_unitsE/housing_unitsE)*100,
  
  pct_rent_gt30 = ((rent_percent_30_34p9E + 
                  rent_percent_35_39p9E +
                  rent_percent_40_49p9E +
                  rent_percent_50_100E)/rent_percent_totE)*100,
  
  pct_built_pre_1980 = ((yr_str_built_1939_earlierE +
                        yr_str_built_1940_1949E +
                        yr_str_built_1950_1959E +
                        yr_str_built_1960_1969E +
                        yr_str_built_1970_1979E)/yr_str_built_totE)*100,
  
  homeownership_rate = (occupied_ownerE/housing_unitsE)*100,
  
  housing_units_per_sqmi = (housing_unitsE/area_sqmi)
    
  
  )


cities <- acs_pcit_ests


## ALFIN (Finance) ####
alfin_agg <- load_alfin_state_agg(codes = FALSE)

alfin_pid <- load_alfin_pid()
alfin_idu <- load_alfin_idu()

alfin_idu_in_pcit <- alfin_idu %>%
  filter(fips_st_pl %in% pcit_df$fips)


# Which variables have a lot of data for the PCIT cities?
alfin_idu_in_pcit %>% count(item_desc) %>% View()



# Feature Selection ####
cities %<>% 
  select(NAME, all_of(features_pcit_housing))

city_names <- cities$NAME

## Correlation ####
feature_corr(cities)

## Dimension Reduction ####
### PCA ####
pca_out <- prcomp(cities_scaled)
pca_df <- get_pca_df(pca_out, cities_scaled, city_names, clustering_kmeans)
fviz_eig(pca_out)
fviz_pca_var(pca_out)

### tSNE ####
# Creates a matrix, not a df
cities_scaled <- cities %>%
  select(-NAME) %>%
  scale()

# Choose City ####
city_choice <- "Somerville city, Massachusetts"

# Clustering ####
## k-means ####
### Elbow Method (select k) ####
kmeans_elbow(cities_scaled, 1:10)

set.seed(2025)
k <- 3
clustering_kmeans <- kmeans(cities_scaled,
                          centers = k,
                          nstart = 25)

clustered_data <- tibble(
  Name = city_names,
  Cluster = as.factor(clustering_kmeans$cluster) # optionally include original data
)

pca_out <- prcomp(cities_scaled)

pca_df <- get_pca_df(pca_out, cities_scaled, city_names, clustering_kmeans)

fviz_eig(pca_out)
fviz_pca_biplot(pca_out)
fviz_pca_ind(pca_out)
fviz_pca_var(pca_out)

autoplot(pca_out, colour = pca_df$cluster)

plot(prcomp(cities_scaled),
     type = "l", main = "Scree Plot")

plot_pca_3d(city_choice, pca_out, clustering_kmeans$cluster)

plot_clustering(pca_df)


## hclust ####
pca_df$cluster <- hcluster(cities_scaled)
plot_clustering(pca_df)
plot_pca_3d(city_choice, pca_out, pca_df$cluster)













