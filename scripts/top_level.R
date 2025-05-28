if (!requireNamespace("pacman")) {
  install.packages("pacman")
}

library(pacman)
p_load(
  tidycensus, dplyr,
  languageserver, pacman,
  corrplot, validate,
  tidyverse, skimr,
  magrittr
)

pcit_df <- read_csv("./data/2024-peer-city-data-full.csv") %>%
  mutate(proj_uid = capitalize_after_underscore(proj_uid))

source("./scripts/constants.R")
source("./scripts/utils.R")

acs_2023 <- load_acs(acs_vars_housing, 2023)
acs_data <- load_acs(acs_vars_housing, 2022)
acs_data_counties <- load_acs_county(acs_vars_housing, 2022)
acs_data_townships <- load_acs_townships(acs_vars_housing, 2022)
acs_in_pcit <- load_acs_pcit_960()

# Todo: save 2010-2023 ACS tables

missing <- pcit_df %>%
  filter(!(proj_uid %in% acs_in_pcit$pcit))































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
