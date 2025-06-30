# FIPS codes
# Currently used for ALFIN data loading

# Append county fips codes that are 000, which seem to represent 
# entire aggregated states

# fips_state_county -> fips$state_county
# state_rows -> fips$state_rows
# fips_places -> fips$places
# states -> fips$states

fips <- list()

fips$state_county <- fips_codes # this is 2010-2025 fips codes, per tidycensus documentation. 000 fips codes appear to be for entire states, per fcc

fips$state_rows <- fips$state_county %>%
  distinct(state, state_code, state_name, state_name) %>%
  mutate(county_code = "000",
         county = paste(state_name, "Full"))

fips$state_county %<>%
  bind_rows(fips$state_rows) %>%
  arrange(state_code, county_code)

# Load in fips place codes
fips$places <- read_delim("./data/fips/national_place2020.txt", delim = "|")
