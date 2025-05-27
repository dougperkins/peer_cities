

# Set your Census API key
#census_api_key(API_KEY, install = TRUE, overwrite = TRUE)

load_acs <- function(){
  # List of ACS variables
  acs_vars_housing <- c(
    median_home_value = "B25077_001", # (dollars)
    median_household_income = "B19013_001", # last 12mo (dollars)
    occupancy_status = "B25002_001", # occupancy status?
    housing_units = "B25001_001", # (housing units)
    gross_rent_as_pct_income = "B25070_001", # last 12mo (pct household income)
    tot_pop = "B01003_001" # (persons)
  )
  
  # Pull ACS data for cities
  acs_data <- get_acs(
    geography = "place",
    variables = acs_vars_housing,
    year = 2023,
    survey = "acs5",
    output = "wide"
  )
  
  acs_data <- acs_data %>%
    filter(!(NAME %in% c("Islamorada, Village of Islands village, Florida",
                         "Lynchburg, Moore County metropolitan government, Tennessee"))) %>%
    rowwise() %>%
    mutate(pcit = convert_city_acs_to_pcit(NAME)) %>%
    ungroup()
}


load_acs_county <- function(){
  acs_data_counties <- get_acs(
    geography = "county",
    variables = acs_vars_housing,
    year = 2023,
    survey = "acs5",
    output = "wide"
  )
  
  acs_data_counties %<>%
    rowwise() %>%
    mutate(pcit = convert_city_acs_to_pcit(NAME, county=TRUE)) %>%
    ungroup()
}

load_acs_townships <- function(){
  acs_data_townships <- get_acs(
    geography = "county subdivision",
    state = "NJ",
    variables = acs_vars_housing,
    year = 2023,
    survey = "acs5",
    output = "wide"
  )

  acs_data_townships %<>%
    rowwise() %>%
    mutate(pcit = convert_county_acs_to_pcit(NAME)) %>%
    ungroup()
}

load_acs_pcit_960 <- function(){
  missing_acs <- acs_data %>%
    filter(NAME %in% missing_places)
  
  missing_counties <- acs_data_counties %>%
    filter(NAME %in% missing_counties)
  
  missing_townships <- acs_data_townships %>%
    filter(NAME %in% missing_townships)
}


# Load ACS variable descriptions for 2022 ACS 5-year
acs_var_lookup <- load_variables(2022, "acs5", cache = TRUE)
acs_var_lookup_23 <- load_variables(2023, "acs5", cache = TRUE)


# View the variables you used in acs_vars
acs_var_lookup_23 %>%
    filter(name %in% c(acs_vars_housing)) %>%
    select(name, label, concept, ) %>%
    View()

# List of city FIPS codes (example: add your cities here)
# city_fips <- c("16000US3651000", "16000US0644000") # New York, Los Angeles







census_2010 <- get_decennial(geography = "place")

years <- 2010:2023
acs_data_years <- get_acs_years(years, acs_vars_housing)

View(acs_data)

# ENDS WITH capital E
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

# ============================#



Somerville_data <- acs_data %>%
    filter(city == "Somerville city, Massachusetts")
