if (!requireNamespace("pacman")) {
    install.packages("pacman")
}

library(pacman)
p_load(tidycensus, dplyr, languageserver, pacman, corrplot, validate, tidyverse, skimr)

# Set your Census API key
# census_api_key(API_KEY, install = TRUE)

# List of ACS variables
acs_vars <- c(
    median_home_value = "B25077_001", # (dollars)
    median_household_income = "B19013_001", # last 12mo (dollars)
    occupancy_status = "B25002_001", # occupancy status?
    housing_units = "B25001_001", # (housing units)
    gross_rent_as_pct_income = "B25070_001", # last 12mo (pct household income)
    tot_pop = "B01003_001" # (persons)
)

# Load ACS variable descriptions for 2022 ACS 5-year
# acs_var_lookup <- load_variables(2022, "acs5", cache = TRUE)

# View the variables you used in acs_vars
acs_var_lookup %>%
    filter(name %in% c(
        "B25077_001", "B19013_001",
        "B25002_001", "B25001_001",
        "B25070_001", "B01003_001"
    )) %>%
    select(name, label, concept) %>%
    View()

# List of city FIPS codes (example: add your cities here)
# city_fips <- c("16000US3651000", "16000US0644000") # New York, Los Angeles

# Pull ACS data for cities
acs_data <- get_acs(
    geography = "place",
    variables = acs_vars,
    year = 2022,
    survey = "acs5",
    output = "wide"
)

View(acs_data)

# %>%
#   filter(GEOID %in% city_fips) %>%

# acs_data <- acs_data %>%
#     select(
#         # city = NAME,
#         median_home_value = median_home_valueE,
#         median_household_income = median_household_incomeE,
#         occupancy_status = occupancy_statusE,
#         housing_units = housing_unitsE,
#         gross_rent_as_pct_income = gross_rent_as_pct_incomeE,
#         tot_pop = tot_popE
#     )

acs_data_ests <- acs_data %>%
    select(
        NAME,
        median_home_valueE,
        median_household_incomeE,
        occupancy_statusE,
        housing_unitsE,
        gross_rent_as_pct_incomeE,
        tot_popE
    )

View(acs_data_ests)

acs_data_ests %>%
    count(median_home_valueE) %>%
    arrange(-n)
acs_data_ests %>%
    filter(median_home_valueE == "Invalid Number") %>%
    View()
acs_data_ests[acs_data_ests$NAME == "Banks town, Alabama", ]$median_home_valueE

# Add a State column by extracting the state name after the last comma in the NAME column
acs_data_ests <- acs_data_ests %>%
    mutate(State = trimws(sub(".*,\\s*([^,]+)$", "\\1", NAME)))

# Create a data validation object to check for missing values
rules <- validator(
    median_home_valueE_complete = !is.na(median_home_valueE),
    median_household_incomeE_complete = !is.na(median_household_incomeE),
    occupancy_statusE_complete = !is.na(occupancy_statusE),
    housing_unitsE_complete = !is.na(housing_unitsE),
    gross_rent_as_pct_incomeE_complete = !is.na(gross_rent_as_pct_incomeE),
    tot_popE_complete = !is.na(tot_popE)
)

# Perform the validation
v <- confront(acs_data_ests, rules)

# Plot the amount of missingness for each variable
png("./figures/missingness_per_variable.png", width = 800, height = 800)
plot(confront(acs_data_ests, rules), main = "Missingness per Variable")
dev.off()


# ============================#


# Generate a correlation matrix for the 6 features in acs_data_ests
cor_matrix <- acs_data_ests %>%
    select(-NAME) %>%
    cor(use = "complete.obs")

print(cor_matrix)

# Visualize the correlation matrix using corrplot
corrplot(cor_matrix,
    method = "color", type = "upper",
    addCoef.col = "black", tl.col = "black", tl.srt = 45,
    title = "Correlation Matrix", mar = c(0, 0, 2, 0)
)

# Make a pairs plot (scatterplot matrix) for the variables in acs_data_ests
pair <- pairs(
    acs_data_ests %>% select(-NAME),
    main = "Pairs Plot of ACS Variables",
    pch = 21,
    bg = "lightblue"
)

Somerville_data <- acs_data %>%
    filter(city == "Somerville city, Massachusetts")
