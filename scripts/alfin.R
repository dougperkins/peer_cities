
alfin_fn_codes <- tribble(
  ~code, ~description,
  "01", "Air transportation (airports)",
  "02", "Cemeteries",
  "03", "Miscellaneous commercial activities",
  "04", "Correctional institutions",
  "05", "Other corrections",
  "09", "Education (school building authorities)",
  "24", "Fire protection",
  "32", "Health",
  "40", "Hospitals",
  "41", "Industrial development",
  "42", "Mortgage credit",
  "44", "Regular highways",
  "45", "Toll highways",
  "50", "Housing and community development",
  "51", "Drainage",
  "52", "Libraries",
  "59", "Other natural resources",
  "60", "Parking facilities",
  "61", "Parks and recreation",
  "62", "Police protection",
  "63", "Flood control",
  "64", "Irrigation",
  "77", "Public welfare institutions",
  "79", "Other public welfare",
  "80", "Sewerage",
  "81", "Solid waste management",
  "86", "Reclamation",
  "87", "Sea and inland port facilities",
  "88", "Soil and water conservation",
  "89", "Other single-function districts",
  "91", "Water supply utility",
  "92", "Electric power utility",
  "93", "Gas supply utility",
  "94", "Mass transit system utility",
  "96", "Fire protection and water supply - combination of services",
  "97", "Natural resources and water supply - combination of services",
  "98", "Sewerage and water supply - combination of services",
  "99", "Other multifunction districts"

)

alfin_type_codes <- tribble(
  ~type, ~description,
  "0", "State",
  "1", "County",
  "2", "City",
  "3", "Township",
  "4", "Special District",
  "5", "Independent School District or Educational Service Agency"
)

# TODO: The 2009 ALFIN PDF does not list a "4" level code...
alfin_level_estimate_codes <- tribble(
  ~code, ~description,
  "1", "State and local government total",
  "2", "State government total",
  "3", "Local government total",
  "5", "County government total",
  "6", "City/municipality government total",
  "7", "Township government total",
  "8", "Special district government total",
  "9", "School district government total"
)

alfin_imp_type_data_flags <- tibble(flag = c("A", "I", "R", "S"),
                                    description = c("Analyst correction", "Imputed", "Reported", "Alternative source"))

# Read in ASFIN item code list
# (POTENTIALLY IMPORTANT: Annual Survey of State Government Finances)
# These are both more descriptive and more updated than the 2009 ones in the PDF
# that comes with the download of the public ALFIN files.
# I cross-checked some of them, but PDF tables are gross
# TODO: Cross-check PDF more thoroughly in some way / become sure that they're
# definitely also the correct item codes for the local level.
# TODO: Potentially manually write in changes for the discontinued ones?
# More likely, make a lookup table for them and remap everything to current codes
item_codes <- read_csv("./data/alfin/itemcodes.csv", col_names = c("item_code", "description"), skip = 1)

# load_alfin <- function(){
#
# }

# Read in ALFIN Individual Data Unit File
# Contains amount for each finance item code within each government unit
# for all respondents and non-respondents in the sample.
# TODO: lookup type, unit id, item code, item flag
load_alfin_idu <- function(){
  col_widths <- fwf_widths(c(c(2, 1, 3, 6), 3, 12, 4, 1),
                            col_names = c(c("fips_state", "type", "fips_county_ish", "unit_id"), "item_code", "amount (thousands)", "year", "impute_type_item_flag"))

  idu <- read_fwf(file = "./data/alfin/2021FinEstDAT_09202024modp_pu.txt", col_widths)

  idu <- idu %>%
    # Get state and county from FIPS codes
    left_join(fips$state_county %>%
                rename(fips_state = state_code,
                      fips_county_ish = county_code),

              by = c("fips_state", "fips_county_ish")) %>%

    # Get type from type codes
    mutate(type = as.character(type)) %>%
    left_join(alfin_type_codes, by="type") %>%

    rename(type_desc = description) %>%

    # Get unit name from PID
    left_join(alfin_pid %>%
                select(unit_id, id_name, PLACENAME, fips_place),

              by="unit_id") %>%

    mutate(fips_st_pl = paste0(fips_state, fips_place)) %>%

    # Get item description from item code
    left_join(item_codes, by="item_code") %>%
    rename(item_desc = description) %>%

    left_join(alfin_imp_type_data_flags, by=c("impute_type_item_flag" = "flag")) %>%
    rename(impute = description)

}

# Read in ALFIN Public-use Format File
# Contains aggregated estimates of item codes by
# level of estimate for each state
# TODO: How is it possible to tell what larger region the 00 FIPS states are?
# They seem to be larger regions, but to identify them you also need region or
# division FIPS code.
# 00 = United States per the 2009 ALFIN PDF
# TODO: Level of est code?
# TODO: Item codes?
load_alfin_state_agg <- function(codes = FALSE){
  col_widths <- fwf_widths(c(2, 1, 4, 13, 12, 3),
                              col_names = c("fips_state",
                                            "level_of_est_code",
                                            "item_code",
                                            "amount_thousands",
                                            "coeff_variation",
                                            "year_2_digits"))

  ff <- read_fwf(file = "./data/alfin/21statetypepu.txt", col_widths)

  ff <- ff %>% mutate(level_of_est_code = as.character(level_of_est_code))

  ff <- ff %>%

    # Get state from FIPS code
    left_join(fips$state_county %>%
             distinct(state, state_code, state_name) %>%
             rename(fips_state = state_code) %>%
             bind_rows((c(state = "US", fips_state = "00", state_name = "United States"))),

             by = c("fips_state")) %>%

    # Get estimation level from code
    left_join(alfin_level_estimate_codes,
              by=c("level_of_est_code" = "code")) %>%
    rename(est_level = description) %>%

    # Get item from code
    left_join(item_codes,
              by="item_code") %>%
    rename(item_desc = description)

  if (codes == FALSE){
    ff <- ff %>% select(state, state_name, est_level, item_desc, amount_thousands, coeff_variation, year_2_digits)
  }
  else {
    ff
  }
}

# Read in ALFIN PID Directory Information File
# Contains basic identifier information for each unit of government in the
# corresponding individual unit file in the annual finance survey
# TODO: lot of missingness - nonrespondents?
# TODO: potentially messed up full-state data?
load_alfin_pid <- function(){
  col_widths <- fwf_widths(c(c(2, 1, 3, 6), 64, 35, 5, 9, 2, 7, 2, 2, 2, 4, 2),
                               col_names = c("fips_state",
                                             "type",
                                             "fips_county_ish",
                                             "unit_id",
                                             "id_name",
                                             "county_name",
                                             "fips_place",
                                             "pop",
                                             "pop_year",
                                             "enrollment",
                                             "enrollment_year",
                                             "special_distr_fn_code",
                                             "school_lvl_code",
                                             "fy_ending",
                                             "survey_yr"))

  pid <- read_fwf(file = "./data/alfin/Fin_PID_2021.txt", col_widths)

  pid <- pid %>%
    left_join(fips$state_county %>% rename(fips_state = state_code,
                                          fips_county_ish = county_code),

             by = c("fips_state", "fips_county_ish")) %>%

    left_join(fips$places %>% rename(fips_place = PLACEFP,
                                     fips_state = STATEFP),

              by = c("fips_state", "fips_place"))
}

