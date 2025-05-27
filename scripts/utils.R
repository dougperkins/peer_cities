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
  # Make lowercase
  city_acs <- tolower(city_acs)
  
  if (county == FALSE){
    # Remove suffixes
    city_acs <- gsub(" (city|town|township|municipality|village|cdp|borough),", ",", city_acs)
  }
  else {
    city_acs <- gsub(" (city|town|township|municipality|village|cdp|borough|county),", ",", city_acs)
  }
  
  # Split into city and state
  parts <- strsplit(city_acs, ",\\s*")[[1]]
  city <- gsub(" ", "", parts[1])  # Remove spaces
  state_full <- str_to_title(parts[2])
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
