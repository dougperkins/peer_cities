vars_housing_acs <- c()
vars_equity_acs <- c()
vars_outlook_acs <- c()
vars_resilience_acs <- c()

vars_boise <- c()
vars_somerville_jan_2023 <- c()
vars_somerville_summer_2023 <- c()

# Named vector of state abbreviations to full names
state_names <- setNames(state.abb, state.name)
state_names["District Of Columbia"] <- "DC"
state_names["Puerto Rico"] <- "PR"
state_names['Atlantic County'] <- "NJ"

state_abbrs <- setNames(state.name, state.abb)
state_abbrs["DC"] <- "District Of Columbia"
state_abbrs["PR"] <- "Puerto Rico"

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

missing_places <- c("Bergenfield borough, New Jersey",
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
                    "Winston-Salem city, North Carolina")

missing_counties <- c("Bibb County, Georgia",
                      "Anchorage Municipality, Alaska",
                      "Clarke County, Georgia",
                      "Richmond County, Georgia",
                      "Honolulu County, Hawaii",
                      "Fayette County, Kentucky",
                      "Jefferson County, Kentucky",
                      "Silver Bow County, Montana",
                      "Arlington County, Virginia",
                      "Suffolk city, Virginia")

missing_townships <- c("Belleville township, Essex County, New Jersey", 
                      "Bloomfield township, Essex County, New Jersey",
                      "Irvington township, Essex County, New Jersey",
                      "Montclair township, Essex County, New Jersey",
                      "Nutley township, Essex County, New Jersey",
                      "City of Orange township, Essex County, New Jersey",
                      "West Orange township, Essex County, New Jersey")
