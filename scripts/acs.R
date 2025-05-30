# Set your Census API key
# census_api_key(API_KEY, install = TRUE, overwrite = TRUE)

load_acs <- function(vars, year) {
  # Pull ACS data for cities
  
  acs_data <- get_acs(
    geography = "place",
    variables = vars,
    year = year,
    survey = "acs5",
    output = "wide"
  )

  acs_data <- acs_data %>%
    filter(!(NAME %in% c(
      "Islamorada, Village of Islands village, Florida",
      "Lynchburg, Moore County metropolitan government, Tennessee"
    ))) %>%
    rowwise() %>%
    mutate(pcit = convert_city_acs_to_pcit(NAME)) %>%
    ungroup() %>%
    mutate(geo_flag = "Place")
}


load_acs_county <- function(vars, year) {
  acs_data_counties <- get_acs(
    geography = "county",
    variables = vars,
    year = year,
    survey = "acs5",
    output = "wide"
  )

  acs_data_counties %<>%
    rowwise() %>%
    mutate(pcit = convert_city_acs_to_pcit(NAME, county = TRUE)) %>%
    ungroup() %>%
    mutate(geo_flag = "County")
}

load_acs_townships <- function(vars, year) {
  acs_data_townships <- get_acs(
    geography = "county subdivision",
    state = "NJ",
    variables = vars,
    year = year,
    survey = "acs5",
    output = "wide"
  )

  acs_data_townships %<>%
    rowwise() %>%
    mutate(pcit = convert_county_acs_to_pcit(NAME)) %>%
    ungroup() %>%
    mutate(geo_flag = "Township")
}

# todo: Double check the population numbers from PCIT are the
# county ones, and not the weird city-county ones
load_acs_pcit_960 <- function() {
  acs_in_pcit <- acs_data %>%
    filter(pcit %in% pcit_df$proj_uid) # 920/960

  missing_acs_data <- acs_data %>%
    filter(NAME %in% missing_places)

  missing_counties_data <- acs_data_counties %>%
    filter(NAME %in% missing_counties)

  missing_townships_data <- acs_data_townships %>%
    filter(NAME %in% missing_townships)

  acs_in_pcit <- acs_in_pcit %>%
    rbind(missing_acs_data) %>%
    rbind(missing_counties_data) %>%
    rbind(missing_townships_data)

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
      pcit == "valleystream_NY" ~ "valleystream_NJ", # is PCIT correct about this? seems to be NY // # nolint: line_length_linter.
      pcit == "nashvilledavidsonmetropolitangovernment(balance)_TN" ~ "nashville_TN", # nolint: line_length_linter.
      pcit == "sandy_UT" ~ "sandycity_UT",
      pcit == "bibb_GA" ~ "macon_GA",
      pcit == "clarke_GA" ~ "athens_GA",
      pcit == "richmond_GA" ~ "augusta_GA",
      pcit == "fayette_KY" ~ "lexington_KY",
      pcit == "jefferson_KY" ~ "louisville_KY",
      pcit == "silverbow_MT" ~ "butte_MT",
      pcit == "cityoforange_NJ" ~ "orange_NJ",
      .default = pcit
    )) %>%
    filter(!(NAME %in% c(
      "Anchorage municipality, Alaska",
      "Arlington CDP, Virginia",
      "Burbank CDP, California",
      "El Cerrito CDP, California",
      "Mountain View CDP, California",
      "Plantation CDP, Florida",
      "Marion CDP, Indiana",
      "Mesquite CDP, Texas",
      "Superior village, Wisconsin",
      "Waukesha village, Wisconsin"
    ))) %>%
    filter(!(GEOID %in% c("5176432"))) %>%
    distinct()

  acs_in_pcit
}

get_acs_estimates <- function(acs){
  acs %<>% select(NAME,
                  ends_with("E"))
}

# load_acs_subset <- function(year, pop_cutoff) {
#   # Checking with their own guidelines - one of these criteria:
#   # Cities with 50k+ population by 2010
#   # Cities with 25k+ population by the 1960 census
# 
#   # 7 deincorporated by 2010 (all NJ?)
#   # 9 annexed all or nearly all of their county; PCIT uses county boundaries
#   # acs_data_50k <- acs_data %>% filter(tot_popE >= 50000)
# }




# View the variables you used in acs_vars
# acs_var_lookup_23 %>%
#   filter(name %in% c(acs_vars_housing)) %>%
#   select(name, label, concept, ) %>%
#   View()

# List of city FIPS codes (example: add your cities here)
# city_fips <- c("16000US3651000", "16000US0644000") # New York, Los Angeles







# census_2010 <- get_decennial(geography = "place")

# years <- 2010:2023
# acs_data_years <- get_acs_years(years, acs_vars_housing)
# 
# View(acs_data)



# View(acs_data_ests)

# acs_data_ests %>%
#   count(median_home_valueE) %>%
#   arrange(-n)
# acs_data_ests %>%
#   filter(median_home_valueE == "Invalid Number") %>%
#   View()
# acs_data_ests[acs_data_ests$NAME == "Banks town, Alabama", ]$median_home_valueE

# Add a State column by extracting the state name after the last comma in the NAME column
acs_data_ests <- acs_data_ests %>%
  mutate(State = trimws(sub(".*,\\s*([^,]+)$", "\\1", NAME)))

# ============================#


# 
# Somerville_data <- acs_data %>%
#   filter(city == "Somerville city, Massachusetts")
