# Set your Census API key
# census_api_key(API_KEY, install = TRUE, overwrite = TRUE)



# Used to get particular years' variable sets
get_acs_vars <- function(years, variables_list) {
  years <- as.character(years)
  
  missing_years <- setdiff(years, names(variables_list$acs))
  if (length(missing_years) > 0) {
    warning("No variables defined for year(s): ", paste(missing_years, collapse = ", "))
  }
  
  purrr::compact(purrr::map(years, ~ variables_list$acs[[.x]])) %>%
    rlang::set_names(years[years %in% names(variables_list$acs)])
  
  
}

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
    mutate(NAME = gsub(pattern = " Micro Area", "", NAME)) %>%
    select(-metro_popM)
}

# Used to load each year of acs data
# TODO reorganize - there should definitely not be three layers of functions lol
# load_all_acs_5y_housing <- function(years, variables, vars_metro) {
#   purrr::map(as.character(years), function(y) {
#     get_acs_5y_housing(
#       vars = variables$acs[[y]],
#       vars_metro = vars_metro,
#       year = y
#     )
#   }) %>%
#     rlang::set_names(as.character(years))
# }


# Load 3 5y DF for PCIT ####
# This works with getting multi-year data
# But does not itself get multi-year data
# TODO: Add in other variables for metro area, to engineer more city variables
# TODO: Fix error with 2009-2013, 2014-2018 geometry downloads
# Error : A state must be specified for this year/dataset combination.


load_or_dl_one_acs_5y_housing <- function(vars, vars_metro, year) {
  message("Processing ACS data for ", year)
  dir.create("./data/acs", showWarnings = FALSE, recursive = TRUE)
  
  base_filename <- paste0("./data/acs/acs_5y_housing_", year)
  files_exist <- all(file.exists(paste0(base_filename, "_places.csv"),
                                 paste0(base_filename, "_counties.csv"),
                                 paste0(base_filename, "_townships.csv"),
                                 paste0(base_filename, "_metros.gpkg")))
  
  if (files_exist) {
    message("Loading cached ACS data for ", year)
    
    acs_places <- readr::read_csv(paste0(base_filename, "_places.csv"), show_col_types = FALSE)
    acs_counties <- readr::read_csv(paste0(base_filename, "_counties.csv"), show_col_types = FALSE)
    acs_townships <- readr::read_csv(paste0(base_filename, "_townships.csv"), show_col_types = FALSE)
    acs_metros <- st_read(paste0(base_filename, "_metros.gpkg"))
    
    return(list("places" = acs_places, 
                "counties" = acs_counties, 
                "townships" = acs_townships,
                "metros" = acs_metros))
  }
  
  message("Downloading ACS data for ", year)
  
  #geom <- if (as.numeric(year) >= 2019) TRUE else FALSE
  # We may not need geoms from here ever - can't get some for older years,
  # can always get them from tigris, although there's the weird error
  # with some city/metro geometries when trying to match metro areas to cities.
  geom <- FALSE  
  
  acs_places <- get_acs(
    geography = "place",
    variables = vars,
    year = as.numeric(year),
    survey = "acs5",
    output = "wide",
    geometry = geom
  )
  
  readr::write_csv(acs_places, paste0(base_filename, "_places.csv"))
  
  acs_counties <- get_acs(
    geography = "county",
    variables = vars,
    year = as.numeric(year),
    survey = "acs5",
    output = "wide",
    geometry = geom
  )
  
  readr::write_csv(acs_counties, paste0(base_filename, "_counties.csv"))
  
  acs_townships <- get_acs(
    geography = "county subdivision",
    state = "NJ",
    variables = vars,
    year = as.numeric(year),
    survey = "acs5",
    output = "wide",
    geometry = geom
  )
  
  acs_townships %<>%
    mutate(GEOID = as.character(GEOID))
  
  readr::write_csv(acs_townships, paste0(base_filename, "_townships.csv"))
  
  acs_metros <- get_acs(
    geography = "metropolitan statistical area/micropolitan statistical area",
    variables = vars_metro,  # total population only, for now
    survey = "acs5",           # or "acs5"
    year = as.numeric(year),    # most recent available
    output = "wide",
    geometry = TRUE
  ) %>%
    mutate(NAME = gsub(pattern = " Metro Area", "", NAME)) %>%
    mutate(NAME = gsub(pattern = " Micro Area", "", NAME)) %>%
    select(-metro_popM)
  
  st_write(acs_metros, paste0(base_filename, "_metros.gpkg"))
  
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

get_pcit_places_tigris <- function(tigris_places){
  # This filtering is a janky way to not have to write 
  # convert_city_acs_to_pcit better
  tigris_places <- tigris_places %>%
    filter(!(NAME %in% c(
      "Islamorada, Village of Islands village, Florida",
      "Lynchburg, Moore County metropolitan government, Tennessee"
    ))) %>%
    rowwise() %>%
    mutate(pcit = convert_city_acs_to_pcit(NAMELSAD)) %>%
    ungroup()
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

get_pcit_townships_tigris <- function(tigris_townships){
  # This filtering is a janky way to not have to write 
  # convert_city_acs_to_pcit better
  tigris_townships <- tigris_townships %>%
    rowwise() %>%
    mutate(pcit = convert_county_acs_to_pcit(NAMELSAD)) %>%
    ungroup()
}


# Filter Non-PCIT Cities ####
# todo: Double check the population numbers from PCIT are the
# county ones, and not the weird city-county ones
load_acs_pcit_960 <- function(acs_places, acs_counties, acs_townships, pcit_df) {
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
  acs %<>% select(GEOID, NAME,
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
    
    homeownership_rate = (occupied_ownerE/occupied_unitsE)*100,
    
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
  
  tryCatch({
  if (st_crs(city) != st_crs(metro_areas)) {
    city <- st_transform(city, st_crs(metro_areas))
  }  
    
  city <- st_make_valid(city)
  metro_areas <- st_make_valid(metro_areas)
  

  
  candidates <- metro_areas[st_intersects(city, metro_areas, sparse = FALSE)[1, ], ]
  
  if (nrow(candidates) == 0) {
    return(NULL)
  }
  
  intersections <- st_intersection(city, candidates)
  
  intersections <- intersections %>%
    mutate(pop = as.numeric(.data[[metro_pop_var]])) %>%
    arrange(desc(pop)) %>%
    slice(1)
  
  # Ensure geometry is a single, valid geometry
  intersections$geometry <- st_union(intersections$geometry)
  
  # Defensive check
  if (nrow(intersections) != 1) {
    intersections <- intersections[1, ]
  }
  
  return(intersections)
  }, error = function(e) {
    message("process_city() failed for a city: ", city$NAME, e$message)
    print(city$NAME)
    return(NULL)
  })
}

process_city_year <- function(city_df, metro_areas, s2use = TRUE, metro_pop_var = "metro_popE") {
  
  message("Starting process_city_year()")
  message("city_df class: ", paste(class(city_df), collapse = ", "))
  message("metro_areas class: ", paste(class(metro_areas), collapse = ", "))
  
  city_df <- st_make_valid(city_df) %>% subset(!st_is_empty(.))
  
  message("Done with city, starting metro")
  
  metro_areas <- st_make_valid(metro_areas)
  
  message("After st_make_valid() and subset:")
  message("city_df class: ", paste(class(city_df), collapse = ", "))
  message("Number of cities: ", nrow(city_df))
  
  #city_list <- split(city_df, seq(nrow(city_df)))
  city_list <- purrr::map(seq_len(nrow(city_df)), ~ city_df[., , drop = FALSE])
  
  message("Created city_list of length: ", length(city_list))
  
  
  #handlers(global = TRUE)
  plan(multisession, workers = 10)
  
  results <- with_progress({
    p <- progressor(along = city_list)
    future_map(city_list, function(city) {
      sf::sf_use_s2(s2use)
      p()
      result <- process_city(city, metro_areas, metro_pop_var)
      
      # If first attempt fails, retry with s2 disabled
      if (is.null(result) && s2use) {
        sf::sf_use_s2(FALSE)
        result <- process_city(city, metro_areas, metro_pop_var)
        
        # Track fallback attempt
        id <- if ("GEOID" %in% names(city)) city$GEOID else paste0("row_", i)
        message("Fallback to s2=FALSE for city: ", id)
      }
      
      return(result)
    }) %>%
      compact() %>%
      bind_rows()
  })
  
  results %<>% mutate(pct_metro_area_pop = (tot_popE / !!sym(metro_pop_var)) * 100)
  return(results)
}

# get_metro_pops_parallel <- function(city_list_by_year, metro_list_by_year,
#                                     s2use = TRUE, metro_pop_var = "metro_popE") {
#   
#   # Ensure both inputs are named lists with matching years
#   years <- intersect(names(city_list_by_year), names(metro_list_by_year))
#   
#   result <- map(years, function(yr) {
#     message("Processing year: ", yr)
#     process_city_year(city_list_by_year[[yr]], metro_list_by_year[[yr]],
#                         s2use = s2use, metro_pop_var = metro_pop_var)
#   })
#   
#   names(result) <- years
#   return(result)
# }

get_metro_pops_parallel <- function(city_list_by_year, metro_list_by_year,
                                    s2use = TRUE, metro_pop_var = "metro_popE",
                                    cache_dir = "./data/metro_areas") {
  
  if (!dir_exists(cache_dir)) dir_create(cache_dir)
  
  years <- intersect(names(city_list_by_year), names(metro_list_by_year))
  
  result <- map(years, function(yr) {
    message("Processing year: ", yr)
    
    cache_file <- file.path(cache_dir, paste0("map_lookup_", yr, ".gpkg"))
    
    if (file_exists(cache_file)) {
      message("Loading cached GeoPackage data for year ", yr)
      cached <- st_read(cache_file, quiet = TRUE)
      return(cached)
    } else {
      message("Cache not found. Computing data for year ", yr)
      computed <- process_city_year(city_list_by_year[[yr]], metro_list_by_year[[yr]],
                                    s2use = s2use, metro_pop_var = metro_pop_var)
      st_write(computed, cache_file, delete_dsn = TRUE, quiet = TRUE)
      return(computed)
    }
  })
  
  names(result) <- years
  return(result)
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
