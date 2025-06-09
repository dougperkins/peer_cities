# Set your Census API key
# census_api_key(API_KEY, install = TRUE, overwrite = TRUE)

# Load 1 5y DF ####
load_acs <- function(vars, year) {
  # Pull ACS data for cities
  
  acs_data <- get_acs(
    geography = "place",
    variables = vars,
    year = year,
    survey = "acs5",
    output = "wide",
    geometry = TRUE
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
  acs_counties <- get_acs(
    geography = "county",
    variables = vars,
    year = year,
    survey = "acs5",
    output = "wide",
    geometry = TRUE
  )
  
  acs_counties %<>%
    rowwise() %>%
    mutate(pcit = convert_city_acs_to_pcit(NAME, county = TRUE)) %>%
    ungroup() %>%
    mutate(geo_flag = "County")
}

load_acs_townships <- function(vars, year) {
  acs_townships <- get_acs(
    geography = "county subdivision",
    state = "NJ",
    variables = vars,
    year = year,
    survey = "acs5",
    output = "wide",
    geometry = TRUE
  )
  
  acs_townships %<>%
    rowwise() %>%
    mutate(pcit = convert_county_acs_to_pcit(NAME)) %>%
    ungroup() %>%
    mutate(geo_flag = "Township")
}

# This loads the ACS metro area geographies and any chosen variables
load_acs_metro_areas <- function(vars, year){
  acs_metros <- get_acs(
    geography = "metropolitan statistical area/micropolitan statistical area",
    variables = vars,  # total population
    survey = "acs5",           # or "acs5"
    year = year,               # most recent available
    output = "wide",
    geometry = TRUE
  ) %>%
    mutate(NAME = gsub(pattern = " Metro Area", "", NAME)) %>%
    mutate(NAME = gsub(pattern = " Micro Area", "", NAME))
}


# Load 3 5y DF for PCIT ####
# This works with getting multi-year data
# But does not itself get multi-year data
# TODO: Add in other variables for metro area, to engineer more city variables
# TODO: Fix error with 2009-2013, 2014-2018 geometry downloads
# Error : A state must be specified for this year/dataset combination.
load_acs_housing <- function(year) {
  # Pull ACS data for cities
  vars <- acs_housing_vars_by_year[[as.character(year)]]
  
  geom <- if (as.numeric(year) >= 2019) TRUE else FALSE
    
  acs_places <- get_acs(
    geography = "place",
    variables = vars,
    year = as.numeric(year),
    survey = "acs5",
    output = "wide",
    geometry = geom
  )
  
  acs_counties <- get_acs(
    geography = "county",
    variables = vars,
    year = as.numeric(year),
    survey = "acs5",
    output = "wide",
    geometry = geom
  )
  
  acs_townships <- get_acs(
    geography = "county subdivision",
    state = "NJ",
    variables = vars,
    year = as.numeric(year),
    survey = "acs5",
    output = "wide",
    geometry = geom
  )
  
  acs_metros <- get_acs(
    geography = "metropolitan statistical area/micropolitan statistical area",
    variables = "B01003_001",  # total population only, for now
    survey = "acs5",           # or "acs5"
    year = as.numeric(year),    # most recent available
    output = "wide",
    geometry = geom
  ) %>%
    mutate(NAME = gsub(pattern = " Metro Area", "", NAME)) %>%
    mutate(NAME = gsub(pattern = " Micro Area", "", NAME))
  
  list("places" = acs_places, 
       "counties" = acs_counties, 
       "townships" = acs_townships,
       "metros" = acs_metros)
}

# Get PCIT City Names ####
# They're formatted differently (ie boisecity_ID)
get_pcit_places <- function(acs_places){
  
  # This filtering is a janky way to not have to write 
  # convert_city_acs_to_pcit better
  acs_places <- acs_places %>%
    filter(!(NAME %in% c(
      "Islamorada, Village of Islands village, Florida",
      "Lynchburg, Moore County metropolitan government, Tennessee"
    ))) %>%
    rowwise() %>%
    mutate(pcit = convert_city_acs_to_pcit(NAME)) %>%
    ungroup() %>%
    mutate(geo_flag = "Place")
}

get_pcit_counties <- function(acs_counties){
  
  # This filtering is a janky way to not have to write 
  # convert_city_acs_to_pcit better
  acs_counties <- acs_counties %>%
    rowwise() %>%
    mutate(pcit = convert_city_acs_to_pcit(NAME, county = TRUE)) %>%
    ungroup() %>%
    mutate(geo_flag = "County")
}

get_pcit_townships <- function(acs_townships){
  # This filtering is a janky way to not have to write 
  # convert_city_acs_to_pcit better
  acs_townships <- acs_townships %>%
    rowwise() %>%
    mutate(pcit = convert_county_acs_to_pcit(NAME)) %>%
    ungroup() %>%
    mutate(geo_flag = "Township")
}


# Filter Non-PCIT Cities ####
# todo: Double check the population numbers from PCIT are the
# county ones, and not the weird city-county ones
load_acs_pcit_960 <- function(acs_places, acs_counties, acs_townships) {
  acs_in_pcit <- acs_places %>%
    filter(pcit %in% pcit_df$proj_uid) # 920/960

  missing_acs_data <- acs_places %>%
    filter(NAME %in% missing_places)

  missing_counties_data <- acs_counties %>%
    filter(NAME %in% missing_counties)

  missing_townships_data <- acs_townships %>%
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

# Drop Error Margin Cols ####
get_acs_estimates <- function(acs){
  acs %<>% select(NAME,
                  ends_with("E"))
}

# Check ACS Vars in Year ####
check_acs_vars <- function(vars, var_lookup_dfs){
  print(paste("Number of variables: ", length(vars)))
  lapply(var_lookup_dfs, function(var_lookup_df) {
      #print(sum(vars %in% var_lookup_df$name))
      paste0(sum(vars %in% var_lookup_df$name), ", Missing (if any): ", vars[!(vars %in% var_lookup_df$name)])
    })
}

# Derive Variables ####
# Derive static variables for a particular ACS-year data
derive_acs_static <- function(acs_year_ests){
  acs_year_ests %<>% mutate(
    
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
}

derive_acs_temporal_lagged <- function(){}

# Derive temporal variables for a set of ACS-year dataframes
derive_acs_temporal_all <- function(cities){
  # Step 1: Identify all columns and extract base names + years
  col_info <- names(cities) %>%
    str_match("^(.*)_(\\d{4})$") %>%
    as_tibble(.name_repair = "minimal") %>%
    setNames(c("full", "base", "year")) %>%
    filter(!is.na(base), !is.na(year))
  
  # Step 2: Nest by base variable
  grouped <- col_info %>%
    group_by(base) %>%
    filter(n() > 1) %>%     # Only keep variables that exist in more than one year
    arrange(year) %>%
    summarise(years = list(year), cols = list(full), .groups = "drop")
  
  # Step 3: Generate new columns with differences
  for (i in seq_len(nrow(grouped))) {
    base_var <- grouped$base[i]
    years <- grouped$years[[i]]
    cols <- grouped$cols[[i]]
    
    # Make all pairwise combinations of years (later minus earlier)
    if (length(years) >= 2) {
      for (j in 2:length(years)) {
        for (k in 1:(j - 1)) {
          y1 <- years[j]
          y2 <- years[k]
          col1 <- paste0(base_var, "_", y1)
          col2 <- paste0(base_var, "_", y2)
          new_col <- paste0(base_var, "_diff_", y1, "_", y2)
          
          if (is.numeric(cities[[col1]]) && is.numeric(cities[[col2]])) {
            cities[[new_col]] <- cities[[col1]] - cities[[col2]]
          }
        }
      }
    }
  }
  
  cities
}

# Get % Metro Area Pop ####
# Function for a single city
process_city <- function(city, metro_areas, metro_pop_var) {
  
  # Filter candidate metros using bounding box intersection
  candidates <- metro_areas[st_intersects(city, metro_areas, sparse = FALSE)[1, ], ]
  
  if (nrow(candidates) == 0) {
    return(NULL)  # No intersecting metro area
  }
  
  # Perform intersection to get actual overlaps
  intersections <- st_intersection(city, candidates)
  
  # Pick the metro with the largest population (metro_popE)
  intersections <- intersections %>%
    mutate(pop = as.numeric(.data[[metro_pop_var]])) %>%
    arrange(desc(pop)) %>%
    slice(1)  # Take the top metro area
  
  return(intersections)
}

get_metro_pops_parallel <- function(city_df, metro_areas, metro_pop_var = "metro_popE"){
  city_list <- split(city_df, seq(nrow(city_df)))
  
  handlers(global = TRUE)  # Enable handlers globally (or use in script only)
  plan(multisession, workers=10)
  
  results <- with_progress({
    p <- progressor(along = city_list)
    
    future_map(city_list, function(city) {
      p()  # update progress bar
      process_city(city, metro_areas = metro_areas, metro_pop_var = metro_pop_var)
    }) %>%
      compact() %>%
      bind_rows()
  })
  
  return(results)
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
# acs_data_ests <- acs_data_ests %>%
#   mutate(State = trimws(sub(".*,\\s*([^,]+)$", "\\1", NAME)))

# ============================#


# 
# Somerville_data <- acs_data %>%
#   filter(city == "Somerville city, Massachusetts")
