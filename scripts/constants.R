# vars_housing_acs <- c()
# vars_equity_acs <- c()
# vars_outlook_acs <- c()
# vars_resilience_acs <- c("B26114_002",
#                          "B26114_012",
#                          "B26114_001")

# BOISE 2020 VARIABLES
# median age of all residents,
# pop growth rate over last 5y (1y or 5y ACS?)
# pop density per sqmi
# % non-white residents
# % over 25 with a bachelor's
# % living below poverty line

load_scripts <- function(){
  
  # Scripts with helper functions
  source("./scripts/utils.R")
  
  # Scripts to load data
  source("./scripts/acs.R")
  # source("./scripts/alfin.R")
  source("./scripts/tigris.R")
  source("./scripts/fips.R")
  
  # Scripts to preprocess
  source("./scripts/preprocess.R")
  
  # Scripts with main logic
  source("./scripts/dim_red.R")
  source("./scripts/cluster.R")
  source("./scripts/eval.R")
  source("./scripts/plot.R")
  source("./scripts/peers.R")
  
}

vars_somerville_jan_2023 <- c()
vars_somerville_summer_2023 <- c()

feats <- list()
feats$housing <- list()

feats$housing$pcit <- c("pct_built_pre_1980",
                        "vacancy_rate",
                        "home_value_to_income",
                        "homeownership_rate",
                        "pct_rent_gt30",
                        "pct_metro_area_pop") 

feats$housing$somerstat_1 <- c(# FROM PCIT
                              "home_value_to_income", #
                              "homeownership_rate", #
                              "pct_rent_gt30", #
                              "vacancy_rate", ##
                              "pct_built_pre_1980", ##
                              
                              # New from SomerStat
                              "median_home_valueE") # 

feats$housing$somerstat_2 <- c(# FROM PCIT
                                "home_value_to_income",  
                                "homeownership_rate",
                                "pct_rent_gt30",
                                "pct_metro_area_pop",
                                
                                # New from SomerStat
                                "median_home_valueE",
                                "housing_units_per_sqmi") 

feats$housing$pcit_somerstat <- c(# FROM PCIT
                                "home_value_to_income",
                                "homeownership_rate",
                                "pct_rent_gt30",
                                "pct_metro_area_pop",
                                "vacancy_rate",
                                "pct_built_pre_1980",
                                
                                # New from SomerStat
                                "median_home_valueE",
                                "housing_units_per_sqmi")


# These are the names that we want the variables to end up having
# This is probably unnecessary - I think it's just names()

# features_pcit_housing <- c("pct_built_pre_1980",
#                            "vacancy_rate",
#                            "home_value_to_income",
#                            "homeownership_rate",
#                            "pct_rent_gt30",
#                            "pct_metro_area_pop") 

# features_somerstat_housing_1 <- c(# FROM PCIT
#                                     "home_value_to_income", #
#                                     "homeownership_rate", #
#                                     "pct_rent_gt30", #
#                                     "vacancy_rate", ##
#                                     "pct_built_pre_1980", ##
#                                     
#                                     # New from SomerStat
#                                     "median_home_valueE") # 

# features_somerstat_housing_2 <- c(# FROM PCIT
#                                 "home_value_to_income",  
#                                 "homeownership_rate",
#                                 "pct_rent_gt30",
#                                 "pct_metro_area_pop",
#                                 
#                                 # New from SomerStat
#                                 "median_home_valueE",
#                                 "housing_units_per_sqmi") 

# features_all_housing <- c(# FROM PCIT
#                           "home_value_to_income",
#                           "homeownership_rate",
#                           "pct_rent_gt30",
#                           "pct_metro_area_pop",
#                           "vacancy_rate",
#                           "pct_built_pre_1980",
#                           
#                           # New from SomerStat
#                           "median_home_valueE",
#                           "housing_units_per_sqmi")

#features_boise_general <- c()

# Named vector of state abbreviations to full names
state_names <- setNames(state.abb, state.name)
state_names["District Of Columbia"] <- "DC"
state_names["Puerto Rico"] <- "PR"
state_names["Atlantic County"] <- "NJ"

state_abbrs <- setNames(state.name, state.abb)
state_abbrs["DC"] <- "District Of Columbia"
state_abbrs["PR"] <- "Puerto Rico"

# ACS ####
metro_vars <- c(metro_pop = "B01003_001")

## Housing ####
acs_vars_housing <- c(
  
    ### Housing Costs ####
    median_home_value = "B25077_001", # (dollars)
    median_household_income = "B19013_001", # last 12mo (dollars)
    
    ### Housing Units & Occupancy ####
    occupied_units = "B25002_002", # occupancy status?
    vacant_units = "B25002_003", # occupancy status?
    housing_units = "B25001_001", # (housing units)
    
    ### Gross Rent as % Income ####
    rent_percent_tot = "B25070_001",
    rent_percent_0_9p9 = "B25070_002", # last 12mo (pct household income)
    rent_percent_10_14p9 = "B25070_003", # last 12mo (pct household income)
    rent_percent_15_19p9 = "B25070_004", # last 12mo (pct household income)
    rent_percent_20_24p9 = "B25070_005", # last 12mo (pct household income)
    rent_percent_25_29p9 = "B25070_006", # last 12mo (pct household income)
    rent_percent_30_34p9 = "B25070_007", # last 12mo (pct household income)
    rent_percent_35_39p9 = "B25070_008", # last 12mo (pct household income)
    rent_percent_40_49p9 = "B25070_009", # last 12mo (pct household income)
    rent_percent_50_100 = "B25070_010", # last 12mo (pct household income)
    rent_percent_nc = "B25070_011", # last 12mo (pct household income)
    
    ### Total Population ####
    # TODO: This should instead come from the decennial Census and
    # the Census Bureau's between-year population estimates, per ACS.
    tot_pop = "B01003_001", # (persons)
    
    ### Tenure ####
    occupied_owner = "B25003_002",#,
    
    ### Median Year Householder Moved Into Unit by Tenure
    median_year_moved_in_owner = "B25039_002",
    #median_year_moved_in_renter = "B25039_003", # removed for now due to 4 NAs
    
    ### Occupants per Room ####
    # TODO Percent
    owner_0p5_or_less_per_rm = "B25014_003",
    owner_0p51_1_per_rm = "B25014_004",
    owner_1p01_1p5_per_rm = "B25014_005",
    owner_1p51_2_per_rm = "B25014_006",
    owner_2p01_or_more_per_rm = "B25014_007",
    renter_0p5_or_less_per_rm = "B25014_009",
    renter_0p51_1_per_rm = "B25014_010",
    renter_1p01_1p5_per_rm = "B25014_011",
    renter_1p51_2_per_rm = "B25014_012",
    renter_2p01_or_more_per_rm = "B25014_013",

    ### Rooms ####
    # TODO Percent
    rm_1 = "B25017_002",
    rm_2 = "B25017_003",
    rm_3 = "B25017_004",
    rm_4 = "B25017_005",
    rm_5 = "B25017_006",
    rm_6 = "B25017_007",
    rm_7 = "B25017_008",
    rm_8 = "B25017_009",
    rm_9_or_more = "B25017_010",
    rm_median = "B25018_001",
    rm_total = "B25019_001",

    ### Units in Str ####
    # TODO Percent
    str_1_unit_detached = "B25024_002",
    str_1_unit_attached = "B25024_003",
    str_2_unit = "B25024_004",
    str_3_4_unit = "B25024_005",
    str_5_9_unit = "B25024_006",
    str_10_19_unit = "B25024_007",
    str_20_49_unit = "B25024_008",
    str_50_more_unit = "B25024_009",
    str_mobile_home = "B25024_010",
    str_boat_rv_van = "B25024_011",
    # 
    ### Year Moved In ####
    # TODO Percent
    # TODO Adjust for 2018, 2013
    # Maybe just use median instead
    # owner_moved_in_2021_later = "B25026_003",
    # owner_moved_in_2018_2020 = "B25026_004",
    # owner_moved_in_2010_2017 = "B25026_005",
    # owner_moved_in_2000_2009 = "B25026_006",
    # owner_moved_in_1990_1999 = "B25026_007",
    # owner_moved_in_1989_earlier = "B25026_008",
    # 
    # renter_moved_in_2021_later = "B25026_010",
    # renter_moved_in_2018_2020 = "B25026_011",
    # renter_moved_in_2010_2017 = "B25026_012",
    # renter_moved_in_2000_2009 = "B25026_013",
    # renter_moved_in_1990_1999 = "B25026_014",
    # renter_moved_in_1989_earlier = "B25026_015",
    # 
    # ### Turnover Rate(?) may need to be derived
    # TODO Derive
    # # B07401: Geo mobility in past yr by age for residence 1 year ago
    # 
    ### Utilities in Rent
    # TODO Percent
    utilities_pay_for_1_or_more = "B25069_002",
    utilities_pay_no_extra = "B25069_003",
    # 
    # ### Cost of Utilities
    # # May be coming from Monthly Electricity, Gas costs, Annual Water, Sewer costs, Annual Other Fuel costs
    # 
    # ### Plumbing facilities
    # TODO Percent
    plumbing_complete = "B25048_002",
    plumbing_incomplete = "B25048_003",
    
    # ### Kitchen facilities
    # TODO Percent
    kitchen_complete = "B25051_002",
    kitchen_incomplete = "B25051_003",
    # 
    # ### Median Monthly Owner Costs by Mortgage Status
    # owner_costs_mortgage = "B25088_002",
    # owner_costs_no_mortgage = "B25088_003",
    # 
    # ### Mortgage Status by Real Estate Taxes Paid
    # real_estate_tax_med_mortgage = "B25103_002",
    # real_estate_tax_med_no_mortgage = "B25103_003",
    # 
    ### House Heating Fuel
    # TODO: Percent
    fuel_utility_gas = "B25040_002",
    fuel_bottled_tank_lp_gas = "B25040_003",
    fuel_electricity = "B25040_004",
    fuel_fuel_oil_kerosene = "B25040_005",
    fuel_coal_coke = "B25040_006",
    fuel_wood = "B25040_007",
    fuel_solar = "B25040_008",
    fuel_other = "B25040_009",
    fuel_none = "B25040_010",
    # 
    # 
    # ### Number of Bedrooms
    # TODO: Percent
    bedrooms_0 = "B25041_002",
    bedrooms_1 = "B25041_003",
    bedrooms_2 = "B25041_004",
    bedrooms_3 = "B25041_005",
    bedrooms_4 = "B25041_006",
    bedrooms_5_more = "B25041_007",
    # 
    # ### Median Contract Rent
    # TODO Derive as percent income (additionally? or only?)
    rent_contract_med = "B25058_001"
    
    ### Rent Asked
    # B25061
    
    ### Price Asked
    # B25085
    
    ### Gross Rent
    # B25063
    
)

acs_housing_vars_diffs_2013 <- c(
  yr_str_built_tot = "B25034_001",
  #yr_str_built_2010_on = "B25034_002",
  yr_str_built_2000_2009 = "B25034_003",
  yr_str_built_1990_1999 = "B25034_004",
  yr_str_built_1980_1989 = "B25034_005",
  yr_str_built_1970_1979 = "B25034_006",
  yr_str_built_1960_1969 = "B25034_007",
  yr_str_built_1950_1959 = "B25034_008",
  yr_str_built_1940_1949 = "B25034_009",
  yr_str_built_1939_earlier = "B25034_010",
  yr_str_built_median = "B25035_001"
)

acs_housing_vars_diffs_2018 <- c(
  yr_str_built_tot = "B25034_001",
  #yr_str_built_2014_on = "B25034_002",
  #yr_str_built_2010_2013 = "B25034_003",
  yr_str_built_2000_2009 = "B25034_004",
  yr_str_built_1990_1999 = "B25034_005",
  yr_str_built_1980_1989 = "B25034_006",
  yr_str_built_1970_1979 = "B25034_007",
  yr_str_built_1960_1969 = "B25034_008",
  yr_str_built_1950_1959 = "B25034_009",
  yr_str_built_1940_1949 = "B25034_010",
  yr_str_built_1939_earlier = "B25034_011",
  yr_str_built_median = "B25035_001"
)

acs_housing_vars_diffs_2023 <- c(
  ### Year Structure Built ####
  yr_str_built_tot = "B25034_001",
  #yr_str_built_2020_on = "B25034_002",
  #yr_str_built_2010_2019 = "B25034_003",
  yr_str_built_2000_2009 = "B25034_004",
  yr_str_built_1990_1999 = "B25034_005",
  yr_str_built_1980_1989 = "B25034_006",
  yr_str_built_1970_1979 = "B25034_007",
  yr_str_built_1960_1969 = "B25034_008",
  yr_str_built_1950_1959 = "B25034_009",
  yr_str_built_1940_1949 = "B25034_010",
  yr_str_built_1939_earlier = "B25034_011",
  yr_str_built_median = "B25035_001"
)

all_variables <- list()

all_variables$acs <- list(
  "2013" = c(acs_vars_housing, acs_housing_vars_diffs_2013),
  "2018" = c(acs_vars_housing, acs_housing_vars_diffs_2018),
  "2021" = c(acs_vars_housing, acs_housing_vars_diffs_2023),
  "2023" = c(acs_vars_housing, acs_housing_vars_diffs_2023)
)

names(acs_vars_housing)[acs_vars_housing == "B25034_011"]
acs_vars_housing

# Create a data validation object to check for missing values
rules <- validator(
    median_home_valueE_complete = !is.na(median_home_valueE),
    median_household_incomeE_complete = !is.na(median_household_incomeE),
    occupancy_statusE_complete = !is.na(occupancy_statusE),
    housing_unitsE_complete = !is.na(housing_unitsE),
    gross_rent_as_pct_incomeE_complete = !is.na(gross_rent_as_pct_incomeE),
    tot_popE_complete = !is.na(tot_popE),
    name_complete = !is.na(NAME),
    state_complete = !is.na(State)
)

# Orange is missing
township_names_tigris_2021 <- c(
  "Belleville township",
  "Bloomfield township",
  "Irvington township",
  "Montclair township",
  "Nutley township",
  "West Orange township"
)

# county_names_tigris_2021 <- c(
#   
# )

missing_places <- c(
    "Bergenfield borough, New Jersey",
    "Boise City city, Idaho",
    "Carson City, Nevada",
    "East St. Louis city, Illinois",
    "Fair Lawn borough, New Jersey",
    "Indianapolis city (balance), Indiana",
    "Irvine city, California",
    "Lee's Summit city, Missouri",
    "Maple Grove city, Minnesota",
    "Margate city, Florida",
    "Milford city (balance), Connecticut",
    "Nashville-Davidson metropolitan government (balance), Tennessee",
    "Norristown borough, Pennsylvania",
    "O'Fallon city, Missouri",
    "Port St. Lucie city, Florida",
    "Pottstown borough, Pennsylvania",
    "Sandy city, Utah",
    "St. Charles city, Missouri",
    "St. Clair Shores city, Michigan",
    "St. Cloud city, Minnesota",
    "St. George city, Utah",
    "St. Joseph city, Missouri",
    "St. Louis Park city, Minnesota",
    "St. Louis city, Missouri",
    "St. Paul city, Minnesota",
    "St. Peters city, Missouri",
    "St. Petersburg city, Florida",
    "Valley Stream village, New York",
    "San Buenaventura (Ventura) city, California",
    "West Mifflin borough, Pennsylvania",
    "Weymouth Town city, Massachusetts",
    "Wilkes-Barre city, Pennsylvania",
    "Wilkinsburg borough, Pennsylvania",
    "Winston-Salem city, North Carolina"
)

missing_counties <- c(
    "Bibb County, Georgia",
    "Anchorage Municipality, Alaska",
    "Clarke County, Georgia",
    "Richmond County, Georgia",
    "Honolulu County, Hawaii",
    "Fayette County, Kentucky",
    "Jefferson County, Kentucky",
    "Silver Bow County, Montana",
    "Arlington County, Virginia",
    "Suffolk city, Virginia"
)

missing_townships <- c(
    "Belleville township, Essex County, New Jersey",
    "Bloomfield township, Essex County, New Jersey",
    "Irvington township, Essex County, New Jersey",
    "Montclair township, Essex County, New Jersey",
    "Nutley township, Essex County, New Jersey",
    "City of Orange township, Essex County, New Jersey",
    "West Orange township, Essex County, New Jersey"
)

# # Load ACS variable descriptions for 2022 ACS 5-year
acs_var_lookup_21 <- load_variables(2021, "acs5", cache = TRUE)
# acs_var_lookup_22 <- load_variables(2022, "acs5", cache = TRUE) # For comparison with existing tools
# acs_var_lookup_23 <- load_variables(2023, "acs5", cache = TRUE)
# acs_var_lookup_18 <- load_variables(2018, "acs5", cache = TRUE)
# acs_var_lookup_13 <- load_variables(2013, "acs5", cache = TRUE)

# # Check if the variable numbers are present in each year
# TODO Check that they also however need to have the same labels over the years
# acs_vars_housing
# check_acs_vars(acs_vars_housing, list(acs_var_lookup_23, acs_var_lookup_18, acs_var_lookup_13))
