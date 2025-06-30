# Feature Variables for Clustering

# 

# Housing

% rent-burdened households (B25070)

- B25070_001E – renter occupied

- B25070_007E – rent is 30 to 34.9 % of monthly income

- B25070_008E – 35 to 39.9 %

- B25070_009E – 40 to 49.9 %

- B25070_010E - > 50 %

home value to income ratio (B25077, B19013)

- B25077_001E – Median Value (owner-occupied housing units)

- B19013_001E – Median Household income in the past 12 months

median home value (B25077)

vacancy rate (dropped in 2023)

- B25002_001E – Total units

- B25002_003E – Vacant units

% housing built pre-1980 (dropped in 2023)

- B25034_001E – Total units

- B25034_007E – Built 1970 to1979

- B25034_008E – Built 1960 to 1969

- B25034_009E – Built 1950 to 1959

- B25034_010E – Built 1940 to 1949

- B25034_011E – Built 1939 or earlier

gross rent as % of income (B25070)

housing units per square mile (B25001)

share of metropolitan area population (B01003, assembled table of cities & metro areas)

#### Potential Additions

- Homeownership rate

- B25002_002E – Total occupied

- B25003_002E – Owner occupied

- B25014 - Occupants per Room 

Homelessness rates

Age of city

Proximity to major metro city

Own to Rent Ratio?

# Resilience

Variables listed on the PCIT site, ACS variables listed in Peer Cities Tool Documentation document.
**Unemployment rate**

Table B26114: Group Quarters Type By Employment Status (population 16 years and over) 

- B26114_002E – Total In Labor Force 

- B26114_012E – Total In Labor Force: Civilian Labor Force Unemployed 

Unemployment Rate = 100 * (Total In Labor Force: Civilian Labor Force Unemployed / Total In Labor Force) 

**Labor Force Participation Rate**

Table B26114: Group Quarters Type By Employment Status (population 16 years and over) 

- B26114_001E – Total 

- B26114_002E – Total In Labor Force 

Labor Force Participation Rate = 100 * (Total In Labor Force / Total) 

**Change in Labor Force Participation Rate**

2021 

Table B26114: Group Quarters Type By Employment Status (population 16 years and over) 

- B26114_001E – Total 

- B26114_002E – Total In Labor Force 

2000 

- P043002 – Total Male 

- P043003 – Total Male In Labor Force 

- P043009 – Total Female 

- P043010 – Total Female In Labor Force 

Change in Labor Force Participation Rate = 100 * (((Total In labor Force / Total)-((Total Male In Labor Force + Total Female In Labor Force) / (Total Male + Total Female)))/((Total Male In Labor Force + Total Female In Labor Force) / (Total Male + Total Female))) 

OR change = 100 * (new rate – old rate) / old rate 
**Labor Share of Manufacturing (double check)**

Table B26115: Group Quarters Type By Occupation (population 16 years and over) 

- B26115_001E – Total 

- B26115_006E – Production, transportation, and material moving occupations 

Labor Share of Manufacturing = 100 * (Production, transportation, and material moving occupations) / Total
**Change, Labor Share of Manufacturing, 1970-2021 (double check)**

2021 

Table B26115: Group Quarters Type By Occupation (population 16 years and over) 

- B26115_001E – Total 

- B26115_006E – Production, transportation, and material moving occupations 

1970 

- P006001 – Experienced civilian labor force, total 

- P006004 – Operatives, including transport 

Change = 100 * (((Production, transportation, and material moving occupations / Total) - (Operatives, including transport / Experienced civilian labor force, total)) / (Operatives, including transport / Experienced civilian labor force, total)) 

**Median Family Income** 

- B19013_001E – Median Household income in the past 12 months 

Median Family Income = Median Household income in the past 12 months 

**Change in Median Family Income, 2000-2021** 

2021 

- B19013_001E – Median Household income in the past 12 months 

2000 

- P053001 – Median household income in 1999 

Change in Median Family Income = 100 * (Median Household income in the past 12 months - Median household income in 1999) / Median household income in 1999

# Outlook / Demographics

Age

- B01001 - Sex by Age (Brackets)

- B01002 - Median

Population Size

- B01003 
- - Total Population
- - Population growth rate over last X years (derived from subtracting previous total population)
- - People per square mile (derived from dividing by area)

Race & Ethnicity

- B03002 - Hispanic or Latino Origin by Race (percent of non-white, non-Hispanic residents)

# Other

Poverty

- B06012 - Place of Birth by Poverty Status in the Past 12 Months

- B17010 - Poverty Status in the Past 12 Months of Families by Family Type by Presence of Related Children by Age of Related Children (percent children in poverty?)

- B17001 - Poverty Status in the Past 12 Months by Sex by Age

Education

- B15003 - Educational Attainment for the Population 25 Years and Over (percent over 25 with a bachelor's or higher)

2021 variable codes can be found [here](https://api.census.gov/data/2021/acs/acs1/variables.html). 

2000 variable codes can be found [here](https://api.census.gov/data/2000/dec/sf3/variables.html). 

# Other Important in Somerville

- Development pressure

- Physical accessibility

- Population Turnover

- Non-resident Owners

- Immigrant population

- The Hills / street geometry

- Stronger mayor setup (formal/informal), no strong counties

- Municipal budget structure - strong state control

- Specific business types - lab space - industry mix

- Arts uses - artists, art studios