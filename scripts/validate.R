# Create a data validation object to check for missing values
rules <- validator(
    median_home_valueE_complete = !is.na(median_home_valueE),
    median_household_incomeE_complete = !is.na(median_household_incomeE),
    occupancy_statusE_complete = !is.na(occupancy_statusE),
    housing_unitsE_complete = !is.na(housing_unitsE),
    gross_rent_as_pct_incomeE_complete = !is.na(gross_rent_as_pct_incomeE),
    tot_popE_complete = !is.na(tot_popE),
    name_complete = !is.na(NAME),
    state_complete = !is.na(State),
    
    home_value_to_income_complete = !is.na(home_value_to_income),
    homeownership_rate_complete = !is.na(homeownership_rate),
    pct_rent_gt30_complete = !is.na(pct_rent_gt30),
    pct_metro_area_pop_complete = !is.na(pct_metro_area_pop),
    housing_units_per_sqmi_complete = !is.na(housing_units_per_sqmi)
    
)



# Perform the validation
v <- confront(acs_data_ests, rules)

# Plot the amount of missingness for each variable
png("./figures/missingness_per_variable.png", width = 800, height = 800)
plot(confront(acs_data_ests, rules), main = "Missingness per Variable")
dev.off()
