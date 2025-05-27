vars_housing_acs <- c()
vars_equity_acs <- c()
vars_outlook_acs <- c()
vars_resilience_acs <- c()

vars_boise <- c()
vars_somerville_jan_2023 <- c()
vars_somerville_summer_2023 <- c()

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
