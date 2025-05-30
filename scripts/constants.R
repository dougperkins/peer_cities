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

vars_somerville_jan_2023 <- c()
vars_somerville_summer_2023 <- c()

features_pcit_housing <- c("pct_built_pre_1980",
                           "vacancy_rate",
                           "home_value_to_income",
                           "homeownership_rate",
                           "pct_rent_gt30") # missing share of metro area pop

features_somerstat_housing <- c(# FROM PCIT
                                "home_value_to_income",
                                "homeownership_rate",
                                "pct_rent_gt30",
                                
                                # New from SomerStat
                                "median_home_valueE",
                                "housing_units_per_sqmi") # missing share of metro area pop

#features_boise_general <- c()

# Named vector of state abbreviations to full names
state_names <- setNames(state.abb, state.name)
state_names["District Of Columbia"] <- "DC"
state_names["Puerto Rico"] <- "PR"
state_names["Atlantic County"] <- "NJ"

state_abbrs <- setNames(state.name, state.abb)
state_abbrs["DC"] <- "District Of Columbia"
state_abbrs["PR"] <- "Puerto Rico"

acs_vars_housing <- c(
    median_home_value = "B25077_001", # (dollars)
    median_household_income = "B19013_001", # last 12mo (dollars)
    occupied_units = "B25002_002", # occupancy status?
    vacant_units = "B25002_003", # occupancy status?
    housing_units = "B25001_001", # (housing units)
    
    # Gross Rent as % Income
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
    
    tot_pop = "B01003_001", # (persons)
    
    # Year Structure Built
    yr_str_built_tot = "B25034_001",
    yr_str_built_2020_on = "B25034_002",
    yr_str_built_2010_2019 = "B25034_003",
    yr_str_built_2000_2009 = "B25034_004",
    yr_str_built_1990_1999 = "B25034_005",
    yr_str_built_1980_1989 = "B25034_006",
    yr_str_built_1970_1979 = "B25034_007",
    yr_str_built_1960_1969 = "B25034_008",
    yr_str_built_1950_1959 = "B25034_009",
    yr_str_built_1940_1949 = "B25034_010",
    yr_str_built_1939_earlier = "B25034_011",
    
    # Tenure
    occupied_owner = "B25003_002"
    
)

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
