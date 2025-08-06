
# 
# places_2013 <- read_csv("./data/tigris/places_2013.csv")
# places_2018 <- read_csv("./data/tigris/places_2018.csv")
# places_2021 <- read_csv("./data/tigris/places_2021.csv")
# places_2023 <- read_csv("./data/tigris/places_2023.csv")
# 
# counties_2013 <- read_csv("./data/tigris/counties_2013.csv")
# counties_2018 <- read_csv("./data/tigris/counties_2018.csv")
# counties_2021 <- read_csv("./data/tigris/counties_2021.csv")
# counties_2023 <- read_csv("./data/tigris/counties_2023.csv")
# 
# townships_2013 <- read_csv("./data/tigris/townships_2013.csv")
# townships_2018 <- read_csv("./data/tigris/townships_2018.csv")
# townships_2021 <- read_csv("./data/tigris/townships_2021.csv")
# townships_2023 <- read_csv("./data/tigris/townships_2023.csv")

# For each combination of year and geog, reads cached tigris file or downloads
load_tigris_layers <- function(years, geogs, states, dir = "./data/tigris") {
  crossing(year = years, geog = geogs) %>%
    mutate(data = map2(geog, year, ~ load_or_dl_tigris_file(.x, .y, states, dir))) %>%
    group_by(geog) %>%
    summarise(layer_list = list(set_names(data, as.character(year))), .groups = "drop") %>%
    deframe()
}

load_or_dl_tigris_file <- function(geog, year, states, dir = "./data/tigris") {
  cached <- read_tigris_file(geog, year, dir)
  
  if (!is.null(cached)) {
    return(cached)
  }
  
  message("Downloading ", geog, " for ", year)
  data <- dl_tigris(year, geog, states)
  
  if (geog == "countysubs"){
    file_path <- file.path(dir, paste0("townships", "_", year, ".gpkg"))
  }
  else {file_path <- file.path(dir, paste0(geog, "_", year, ".gpkg"))}
  
  dir_create(dir)
  message("Writing ", file_path)
  st_write(data, file_path, delete_dsn = TRUE, quiet = TRUE)
  
  return(data)
}

read_tigris_file <- function(geog, year, dir = "./data/tigris") {
  
  file_path <- file.path(dir, paste0(geog, "_", year, ".gpkg"))
  if (geog == "countysubs"){
    file_path <- file.path(dir, paste0("townships", "_", year, ".gpkg"))
    
  }
  
  if (!file.exists(file_path)) {
    warning("File not found: ", file_path, ", trying download instead")
    return(NULL)
  }
  
  message("Reading: ", file_path)
  st_read(file_path, quiet = TRUE)
}

# TODO fairly sure this is unused now!
# read_tigris_years <- function(years, dir = "./data/tigris") {
#   geogs <- c("places", "townships", "counties")
#   tigris_in <- list()
#   
#   for (geog in geogs) {
#     for (year in years) {
#       layer_data <- read_tigris_file(geog, year, dir)
#       
#       if (!is.null(layer_data)) {
#         if (is.null(tigris_in[[geog]])) {
#           tigris_in[[geog]] <- list()
#         }
#         
#         tigris_in[[geog]][[as.character(year)]] <- layer_data
#       }
#     }
#   }
#   
#   return(tigris_in)
# }



dl_tigris <- function(year, type, states_to_get_area) {
  # Set up parallel processing
  plan(multisession, workers=10)
  
  fetch_state_data <- function(st) {
    message("Fetching: ", st, " for year ", year)
    
    if (type == "places"){
      df <- tryCatch(
        places(state = st, year = year) %>%
          mutate(type = "place", year = year, area_sqmi = ((ALAND / 1000000) / 2.58999)),
        error = function(e) NULL
      )}
    
    if (type == "counties"){
      df <- tryCatch(
        counties(state = st, year = year) %>%
          mutate(type = "county", year = year, area_sqmi = ((ALAND / 1000000) / 2.58999)),
        error = function(e) NULL
      )
    }
    
    if (type == "countysubs"){
      df <- tryCatch(
        county_subdivisions(state = st, year = year) %>%
          mutate(type = "county subdivision", year = year, area_sqmi = ((ALAND / 1000000) / 2.58999)),
        error = function(e) NULL
      )
    }
    
    df
  }
  
  # Parallel mapping: one task per state
  all_data <- future_map_dfr(states_to_get_area, fetch_state_data, .progress = TRUE)
  
  all_data %<>% distinct()
  
  if ("geom" %in% colnames(all_data) & !("geometry" %in% colnames(all_data))){
    all_data %<>% rename(geometry = geom)
  }
  
  return(all_data)
}

build_ready_input <- function(tigris_data_by_geog, acs_list, years) {
  map(as.character(years), function(y) {
    list(
      places    = tigris_data_by_geog$places[[y]],
      townships = tigris_data_by_geog$countysubs[[y]],
      counties  = tigris_data_by_geog$counties[[y]],
      acs       = acs_list[[y]]
    )
  }) %>% set_names(as.character(years))
}

build_all_tigris_pcit <- function(data_inputs) {
  # data_inputs: a named list of lists, each with places, townships, counties, acs
  
  lapply(names(data_inputs), function(year) {
    inputs <- data_inputs[[year]]
    
  inputs$acs %>% 
    mutate(GEOID = as.character(GEOID))
  
  inputs$places %>% 
    mutate(GEOID = as.character(GEOID))
  
  inputs$counties %>% 
    mutate(GEOID = as.character(GEOID))
  
  inputs$places %>%
    mutate(GEOID = as.character(GEOID))
    
    dplyr::bind_rows(
      inputs$places %>% dplyr::filter(GEOID %in% inputs$acs$GEOID),
      inputs$townships %>% dplyr::filter(GEOID %in% inputs$acs$GEOID),
      inputs$counties %>% dplyr::filter(GEOID %in% inputs$acs$GEOID) %>% dplyr::distinct()
    )
  }) %>% setNames(names(data_inputs))
}

fix_geometry <- function(g){
  tryCatch({
    #g <- lwgeom::st_make_valid(g)
    g <- ms_simplify(g, keep = 0.5, keep_shapes = TRUE)
    g$geometry <- st_as_sfc(st_as_text(g$geometry))  # round-trip fix
    if (!st_is_valid(g)) return(NULL)
    st_crs(g) <- 4269
    g
  }, error = function(e) NULL)
}


# 
# 
# plan(multisession, workers = 10)
# areas_2021 <- get_city_areas_sqmi_one_year(2021, states_to_get_area)
# 
# areas_2021_pcit <- get_pcit_places_tigris(areas_2021)
# 



# plcs <- places(state = states, year = 2023) # Takes 5+ minutes
# ctys <- counties(state = states, year) # 
# tshps <- county_subdivisions(states = states)
# 
# plan(multisession, workers = 10)

# mtrs <- core_based_statistical_areas(year = if_else(year == 2022, # 2022 files came down from Census site
#                                                     2021, 
#                                                     year))