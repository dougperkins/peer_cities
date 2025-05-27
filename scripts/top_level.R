if (!requireNamespace("pacman")) {
  install.packages("pacman")
}

library(pacman)
p_load(tidycensus, dplyr, 
       languageserver, pacman, 
       corrplot, validate, 
       tidyverse, skimr,
       magrittr)


pcit_df <- read_csv("./data/2024-peer-city-data-full.csv")

source("./scripts/constants.R")
source("./scripts/utils.R")

# naive_pcit_city_list <- lapply(pcit_df$proj_uid, function(x) convert_city_pcit_to_acs(x, "city"))
# naive_pcit_town_list <- lapply(pcit_df$proj_uid, function(x) convert_city_name(x, "town"))
# naive_pcit_municipality_list <- lapply(pcit_df$proj_uid, function(x) convert_city_name(x, "municipality"))
# naive_pcit_village_list <- lapply(pcit_df$proj_uid, function(x) convert_city_name(x, "village"))

# sum(naive_pcit_city_list %in% acs_data$NAME) # 634/950
# sum(naive_pcit_town_list %in% acs_data$NAME) # 8/950
# sum(naive_pcit_municipality_list %in% acs_data$NAME) # 1/950
# sum(naive_pcit_village_list %in% acs_data$NAME) # 12/950

acs_data <- load_acs()
acs_data_counties <- load_acs_county()
acs_data_townships <- load_acs_townships()

acs_data %<>% mutate(geo_flag = "Place")
acs_data_counties %<>% mutate(geo_flag = "County")
acs_data_townships %<>% mutate(geo_flag = "Township")
sum(acs_data$pcit %in% pcit_df$proj_uid) # 920/960

acs_in_pcit <- acs_data %>%
  filter(pcit %in% pcit_df$proj_uid) # 920/960

missing <- pcit_df %>%
  filter(!(proj_uid %in% acs_in_pcit$pcit))

# missing_checker <- missing %>% select(proj_uid, TotalPopulation, geo_flag) %>%
#   filter(geo_flag == "Place")

# Double check the population numbers from PCIT are the 
#county ones, and not the weird city-county ones
missing_acs_data <- acs_data %>%
  filter(NAME %in% missing_places)

missing_counties_data <- acs_data_counties %>%
  filter(NAME %in% missing_counties)

missing_townships_data <- acs_data_townships %>%
  filter(NAME %in% missing_townships)

acs_in_pcit %<>%
  rbind(missing_acs_data) %>%
  rbind(missing_counties_data) %>%
  rbind(missing_townships_data)

setdiff(acs_in_pcit$pcit, pcit_df_upperfix$proj_uid)
setdiff(pcit_df_upperfix$proj_uid, acs_in_pcit$pcit)

acs_in_pcit %<>%
  mutate(pcit = gsub("township", "", pcit)) %>%
  mutate(pcit = gsub("borough", "", pcit)) %>%
  mutate(pcit = gsub("city\\(balance\\)", "", pcit)) %>%
  mutate(pcit = gsub("township", "", pcit)) %>%
  mutate(pcit = gsub("-", "", pcit)) %>%
  mutate(pcit = gsub("'", "", pcit)) %>%
  mutate(pcit = gsub("st\\.", "saint", pcit)) %>%
  mutate(pcit = case_when(pcit == "irvine_CA" ~ "irving_CA",
                          pcit == "sanbuenaventura(ventura)_CA" ~ "ventura_CA",
                          pcit == "boisecity_ID" ~ "boise_ID",
                          pcit == "weymouthtown_MA" ~ "weymouth_MA",
                          pcit == "carson_NV" ~ "carsoncity_NV",
                          pcit == "valleystream_NY" ~ "valleystream_NJ", # is PCIT correct about this? seems to be NY
                          pcit == "nashvilledavidsonmetropolitangovernment(balance)_TN" ~ "nashville_TN",
                          pcit == "sandy_UT" ~ "sandycity_UT",
                          pcit == "bibb_GA" ~ "macon_GA",
                          pcit == "clarke_GA" ~ "athens_GA",
                          pcit == "richmond_GA" ~ "augusta_GA",
                          pcit == "fayette_KY" ~ "lexington_KY",
                          pcit == "jefferson_KY" ~ "louisville_KY",
                          pcit == "silverbow_MT" ~ "butte_MT",
                          pcit == "cityoforange_NJ" ~ "orange_NJ",
                          .default = pcit))

acs_in_pcit %>%
  group_by(pcit) %>%
  filter(n()>1) %>%
  ungroup() %>%
  View()

pcit_checker <- pcit_df_upperfix %>%
  select(proj_uid, TotalPopulation)

acs_in_pcit %<>%
  filter(!(NAME %in% c("Anchorage municipality, Alaska",
                       "Arlington CDP, Virginia",
                       "Burbank CDP, California",
                       "El Cerrito CDP, California",
                       "Mountain View CDP, California",
                       "Plantation CDP, Florida",
                       "Marion CDP, Indiana",
                       "Mesquite CDP, Texas",
                       "Superior village, Wisconsin",
                       "Waukesha village, Wisconsin")
           )
         ) %>%
         
         filter(!(GEOID %in% c("5176432")))

pcit_df_upperfix <- pcit_df %>%
  mutate(proj_uid = capitalize_after_underscore(proj_uid))

length(pcit_cities_acs_format) # 655/950


# Checking with their own guidelines - one of these criteria: 
# Cities with 50k+ population by 2010
# Cities with 25k+ population by the 1960 census 

# 7 deincorporated by 2010 (all NJ?)
# 9 annexed all or nearly all of their county; PCIT uses county boundaries
acs_data_50k <- acs_data %>% filter(tot_popE >= 50000)






























# Check feature correlation
cor_matrix <- acs_data_ests %>%
  select(-NAME) %>%
  cor(use = "complete.obs")

print(cor_matrix)

corrplot(cor_matrix,
         method = "color", type = "upper",
         addCoef.col = "black", tl.col = "black", tl.srt = 45,
         title = "Correlation Matrix", mar = c(0, 0, 2, 0)
)

corrplot(cor_matrix)

# Make a pairs plot (scatterplot matrix) for the variables in acs_data_ests
pair <- pairs(
  acs_data_ests %>% select(-NAME),
  main = "Pairs Plot of ACS Variables",
  pch = 21,
  bg = "lightblue"
)
