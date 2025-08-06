get_acs_years <- function(years, vars){
  acs_data <- map_dfr(years, function(yr){
    acs_data <- get_acs(
      geography = "place",
      variables = vars,
      year = yr,
      survey = "acs5",
      output = "wide"
    ) %>%
    mutate(year = yr)
  })
}


# Function to convert e.g., "atlanta_GA" -> "Atlanta city, Georgia"
convert_city_pcit_to_acs <- function(city_state, geo_word) {
  parts <- strsplit(city_state, "_")[[1]]
  city <- tools::toTitleCase(gsub("([a-z])([A-Z])", "\\1 \\2", parts[1]))  # Title case
  state_abbr <- toupper(parts[2])
  state_full <- state_names[state_abbr]
  
  if (is.na(state_full)) stop(paste("Unknown state abbreviation:", state_abbr))
  
  paste0(city, " ", geo_word, ", ", state_full)
}

convert_city_acs_to_pcit <- function(city_acs, county=FALSE){
  #browser()
  # Make lowercase
  city_acs <- tolower(city_acs)
  #print("start")
  if (county == FALSE){
    # Remove suffixes
    city_acs <- gsub(" (city|town|township|municipality|village|cdp|borough|comunidad|urbana),", ",", city_acs)
  }
  else {
    city_acs <- gsub(" (city|town|township|municipality|village|cdp|borough|comunidad|urbana|municipio|county),", ",", city_acs)
  }
  
  # Split into city and state
  parts <- strsplit(city_acs, ",\\s*")[[1]]
  city <- gsub(" ", "", parts[1])  # Remove spaces
  state_full <- str_to_title(parts[2])
  cat(paste0(city, ", ", blue(state_full), " converted to PCIT name format"))
  #print(city)
  #print(state_full)
  if (state_full == "Moore County Metropolitan Government; Tennessee"){
    state_full <- "Tennessee"
  }
  if (state_full == "Village Of Islands Village; Florida"){
    state_full <- "Florida"
  }

  #print(state_full)
  
  # Lookup abbreviation
  state_names <- state_names[state_full]
  if (is.na(state_names)) stop(paste("Unknown state name:", state_full))
  
  # Return formatted result
  paste0(city, "_", state_names)
}

convert_county_acs_to_pcit <- function(county_acs, county=FALSE){
  # Make lowercase
  county_acs <- tolower(county_acs)
  
  county_acs <- gsub(",\\s*[^,]+ county,", ",", county_acs)
  
  
  # Split into city and state
  parts <- strsplit(county_acs, ",\\s*")[[1]]
  county <- gsub(" ", "", parts[1])  # Remove spaces
  state_full <- str_to_title(parts[2])
  #print(state_full)
  
  # Lookup abbreviation
  state_names <- state_names[state_full]
  if (is.na(state_names)) stop(paste("Unknown state name:", state_full))
  
  # Return formatted result
  paste0(county, "_", state_names)
}

capitalize_after_underscore <- function(x) {
  str_replace(x, "_(\\w+)$", function(m) {
    paste0("_", toupper(sub("_", "", m)))
  })
}

# TODO
# get_city_subset <- function(city_list){
#   
# }

# TODO
# get_city_subset_by_params <- function(p1, p2, p3){
#   
# }


get_vector_mode <- function(x) {
  tibble(value = x) %>%
    count(value, sort = TRUE) %>%
    slice(1) %>%
    pull(value)
}


add_col_year_suffixes <- function(df_list, years){
  col_suffixes <- paste0("_acs5_", years)
  
  data <- map2(df_list, col_suffixes, function(df, col_suffix) {
    colnames(df) <- paste0(colnames(df), col_suffix)
    df %<>% rename(NAME = paste0("NAME", col_suffix),
                   GEOID = paste0("GEOID", col_suffix))
    df
  })
  
  data
}

# Abstracting top_level Code ####

# TODO: Not ignore metros here? Better to do it all at once?
# ie to get metro area pops - can I for 2013, 2018?
get_pcit_cities_df <- function(places_df_list, counties_df_list, townships_df_list){
  data$pcit$all_geogs <- pmap(
    list(places_df_list, counties_df_list, townships_df_list),
    function(pl, co, tw) load_acs_pcit_960(pl, co, tw, data$raw$pcit)
  )
}

# get_tigris_data <- function(years){
#   # Load tigris data you created with tigris.R
#   data$tigris_in <- read_tigris_years(years)
#   
#   # Prep list of lists of dfs for each year & geog type, for 
#   tigris_in <- set_names(
#     map(years, ~ list(
#       places    = data$tigris_in$places[[.x]],
#       townships = data$tigris_in$townships[[.x]],
#       counties  = data$tigris_in$counties[[.x]],
#       acs       = data$pcit$all_geogs[[.x]]
#     )),
#     years
#   )
#   
#   data$pcit$tigris <- build_all_tigris_pcit(tigris_in)
# }

filter_all_geogs_by_geoids <- function(data, shared_geoids) {
  data$pcit$all_geogs <- purrr::map(data$pcit$all_geogs, ~ dplyr::filter(.x, GEOID %in% shared_geoids))
  data
}

join_areas_to_data <- function(data) {
  data$pcit$all_geogs <- purrr::map2(
    data$pcit$all_geogs,
    data$pcit$tigris,
    ~ dplyr::left_join(
      .x,
      .y %>%
        dplyr::rename(geometry = geom) %>%
        dplyr::select(GEOID, area_sqmi, geometry),
      by = "GEOID"
    )
  )
  return(data)
}

make_spatial <- function(data) {
  # Convert all_geogs to sf
  data$pcit$all_geogs <- purrr::map(data$pcit$all_geogs, sf::st_as_sf)
  
  # Fix and convert acs_metros to sf
  data$raw$acs_metros <- purrr::map(data$raw$acs_metros, function(df) {
    if (!inherits(df, "sf")) {
      if ("geometry" %in% names(df)) {
        # If geometry column is a character (e.g. WKT), convert to sfc
        if (is.character(df$geometry)) {
          df$geometry <- sf::st_as_sfc(df$geometry)
        }
        
        # Now convert to sf
        df <- sf::st_as_sf(df, sf_column_name = "geometry")
      } else {
        stop("No geometry column found in metro data.")
      }
    }
    df
  })
  
  return(data)
}

# Tiny Legibility Helpers ####
# Mostly serving to hide the use of map() and similar functions for readability

drop_geometry <- function(df_list){
  map(df_list, function(df){st_drop_geometry(df)})
}

use_features <- function(df_list, features){
  map(df_list, function(df){df %>% select(GEOID, NAME, all_of(features))})
}

combine_year_dfs <- function(df_list){
  df <- reduce(df_list, ~ left_join(.x, .y, by="GEOID"))
}

load_pcit_df <- function(){
  df <- read_csv("./data/pcit/2024-peer-city-data-full.csv") %>%
    mutate(proj_uid = capitalize_after_underscore(proj_uid))
}

derive_acs_static_all_years <- function(acs_list) {
  purrr::map(acs_list, derive_acs_static)
}

## Restructure Lists ####
# These are bad short-term fixes to structure the lists
# how I'd already written code to expect them. 
restructure_metros_list <- function(data) {
  # Extract metros from each year
  metros_list <- purrr::map(data$raw$acs, ~ .x$metros)
  
  # Remove metros from the original acs list
  data$raw$acs <- purrr::map(data$raw$acs, function(x) {
    x$metros <- NULL
    x
  })
  
  # Assign to data$raw$acs_metros
  data$raw$acs_metros <- metros_list
  
  return(data)
}

# restructure_metros_list <- function(df_acs_year_metros_list){
#   metros_list <- purrr::map(df_acs_year_metros_list, ~ .x$metros)
#   df_acs_year_metros_list <- purrr::map(df_acs_year_metros_list, function(yr) {
#     yr$metros <- NULL
#     yr
#   })
#   df_acs_year_metros_list$metros <- metros_list
#   df_acs_year_metros_list
# }

## Rewrite Eventually ####
# The internal functions being used here could also just be written to use map, 
# load_acs_data_by_year <- function(vars, vars_metro, years){
#   df_list <- map(set_names(years, years), function(year) {load_or_dl_one_acs_5y_housing(vars, vars_metro, year)})
# }

load_acs_data_by_year <- function(years, variable_list, vars_metro) {
  vars_by_year <- get_acs_vars(years, variable_list)
  
  purrr::map2(
    .x = vars_by_year,
    .y = names(vars_by_year),
    .f = ~ load_or_dl_one_acs_5y_housing(year = .y, vars = .x, vars_metro = vars_metro)
  )
}



get_pcit_places_list_in <- function(list_of_lists_years_geog_dfs){
  df_list <- map(list_of_lists_years_geog_dfs, function(year_list_of_geog_dfs){get_pcit_places(year_list_of_geog_dfs[[1]])})
}
get_pcit_counties_list_in <- function(list_of_lists_years_geog_dfs){
  df_list <- map(list_of_lists_years_geog_dfs, function(year_list_of_geog_dfs){get_pcit_counties(year_list_of_geog_dfs[[2]])})
}
get_pcit_townships_list_in <- function(list_of_lists_years_geog_dfs){
  df_list <- map(list_of_lists_years_geog_dfs, function(year_list_of_geog_dfs){get_pcit_townships(year_list_of_geog_dfs[[3]])})
}

get_pcit_names <- function(list_of_lists_years_geog_dfs){
  list(
    places    = get_pcit_places_list_in(list_of_lists_years_geog_dfs),
    counties  = get_pcit_counties_list_in(list_of_lists_years_geog_dfs),
    townships = get_pcit_townships_list_in(list_of_lists_years_geog_dfs)
  )
}

derive_static_acs <- function(df_list){
  map(df_list, function(df){derive_acs_static(df)})
}
