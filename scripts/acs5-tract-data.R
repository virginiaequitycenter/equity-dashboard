# Get ACS5 5-year survey data - Tract Level
# Last updated: 4/9/2026

# This script gets and cleans ACS5 tract level data for a single survey year. 
# The resulting file is saved as `acs5_tract.RDS`. 

# ACS 5-yr variables included in this script:
##  - Total population -- B01003_001
##  - Poverty rate -- S1701_C03_001
##  - Child poverty rate -- S1701_C03_002
##  - Median HH Income -- S1901_C01_012	
##  - Gini Index of Income Inequality -- B19083_001
##  - Percent high school graduate or higher -- S1501_C02_014
##  - Percent bachelor's degree or higher -- S1501_C02_015
##  - Percent graduate degree or higher -- S1501_C02_013
##  - Percent Not Hispanic white alone -- DP05_0082P
##  - Percent Not Hispanic black or African American alone -- DP05_0083P
##  - Percent Not Hispanic American Indian and Alaska Native alone -- DP05_0084P
##  - Percent Not Hispanic Asian alone -- DP05_0085P
##  - Percent Not Hispanic Native Hawaiian and Other Pacific Islander alone -- DP05_0086P
##  - Percent Not Hispanic Some other race alone -- DP05_0087P
##  - Percent Not Hispanic Two or more races -- DP05_0088P
##  - Percent Hispanic or Latino -- DP05_0076P
##  - Percent unemployment (Population 16 and over) -- S2301_C04_001
##  - Percent with health insurance (Civilian noninstitutionalized population) -- S2701_C03_001	
##  - Percent with public health insurance (Civilian noninstitutionalized population) -- S2704_C03_001
##  - Age, population under 18 -- S0101_C02_022
##  - Age, population 18 to 24 -- S0101_C02_023	
##  - Age, 26 to 64 -- sum(S0101_C02_007, S0101_C02_008, S0101_C02_009, S0101_C02_010, S0101_C02_011, S0101_C02_012, S0101_C02_013, S0101_C02_014)
##  - Age, 65 and over --S0101_C02_030
##  - School enrollment for the population age 3 to 24 -- S1401_C02_014, S1401_C02_016, S1401_C02_018, S1401_C02_020, S1401_C02_022, S1401_C21_024
##  - Median personal earnings of all workers with earnings ages 16 and older -- S2001_C01_002
##  - Percent of cost-burdened renters -- B25070_007+B25070_008+B25070_009+B25070_010/B25070_001
##  - Home ownership rates -- B25003_002/B25003_001
##  - Housing vacant units -- B25002_003/B25002_001
##  - Number of households who receive cash public assistance/SNAP benefits -- B19058_002
##  - Number of foreign-born residents -- B05002_013
##  - Number of residents who have a disability -- S1810_C02_001 - (civilian noninstitutionalized population)

# Load packages ----
library(tidyverse)
library(tidycensus)
library(janitor)

# Create basic objects ----
# These variables are set in dashboard-update-workflow.Rmd
dyr <- dyr
acsyr <- acsyr
fips_codes <- fips_codes
filepath_data <- filepath_data

# ....................................................
# Get Data - Variables ----
# variables: define varlist
varlist_s = c("S1701_C03_001", # povrate
            "S1701_C03_002",   # cpovrate
            "S1901_C01_012",   # hhinc
            "S1501_C02_014",   # hsmore
            "S1501_C02_015",   # bamore
            "S1501_C02_013",   # gradmore
            "S2301_C04_001",   # unemp
            "S2701_C03_001",   # hlthins
            "S2704_C03_001",   # pubins
            "S2001_C01_002",   # earn
            "S1810_C02_001")   # disabled 
            

varlist_b = c("B01003_001", # totalpop
              "B19083_001",  # gini
              "B25070_007",  # 30-35 rent burdened
              "B25070_008",  # 35-40 rent burdened
              "B25070_009",  # 40-50 rent burdened
              "B25070_010",  # 50+ rent burdened
              "B25070_001",  # all renters
              "B25003_002",  # owner-occupied housing units
              "B25003_001",  # occupied housing units
              "B25002_003",  # vacant housing units
              "B25002_001",  # housing units
              "B19058_002",  # SNAP
              "B05002_013")  # Foreign-born

# pull variables
tract_data_s <- get_acs(geography = "tract",
                      variables = varlist_s,
                      state = "VA", 
                      county = fips_codes, 
                      survey = "acs5",
                      year = acsyr, 
                      output = "wide",
                      cache_table = TRUE)

tract_data_b <- get_acs(geography = "tract",
                       variables = varlist_b,
                       state = "VA", 
                       county = fips_codes, 
                       survey = "acs5",
                       year = acsyr, 
                       output = "wide",
                       cache_table = TRUE)

# rename variables
names(tract_data_s) = c("GEOID", "NAME",
                        "povrateE", "povrateM",
                        "cpovrateE", "cpovrateM",
                        "hhincE", "hhincM",
                        "hsmoreE", "hsmoreM",
                        "bamoreE", "bamoreM",
                        "gradmoreE", "gradmoreM",
                        "unempE", "unempM",
                        "hlthinsE", "hlthinsM",
                        "pubinsE", "pubinsM",
                        "earnE", "earnM",
                        "disabledE", "disabledM")

names(tract_data_b) = c("GEOID", "NAME",  
                         "totalpopE", "totalpopM",
                         "giniE", "giniM",
                         "rent30E", "rent30M",
                         "rent35E", "rent35M",
                         "rent40E", "rent40M",
                         "rent50E", "rent50M",
                         "rentallE", "rentallM",
                         "ownoccE", "ownoccM",
                         "occhseE", "occhseM",
                         "vachseE", "vachseM",
                         "allhseE", "allhseM",
                        "snapE", "snapM",
                        "foreignbE", "foreignbM")

# Derive some variables
tract_data_b <- tract_data_b %>% 
  mutate(rentersumE = (rent30E+rent35E+rent40E+rent50E),
         rentersumM = (rent30M/1.645)^2 + (rent35M/1.645)^2 + (rent40M/1.645)^2 + (rent50M/1.645)^2,
         rentersumM = sqrt(rentersumM)*1.645,
         renter30E = round((rentersumE/rentallE)*100,1),
         renter30M = moe_prop(rentersumE, rentallE, rentersumM, rentallM),
         renter30M = round(renter30M*100,1)) %>% 
  mutate(homeownE = round((ownoccE/occhseE)*100,1),
         homeownM = moe_prop(ownoccE, occhseE, ownoccM, occhseM),
         homeownM = round(homeownM*100, 1)) %>% 
  mutate(vacrateE = round((vachseE/allhseE)*100,1),
         vacrateM = moe_prop(vachseE, allhseE, vachseM, allhseM),
         vacrateM = round(vacrateM*100, 1)) %>% 
  select(-c(rentersumE, rentersumM,rent30E:occhseM))

# Derive snap variables
tract_data_b <- tract_data_b %>% 
  mutate(perc_snaphseE = round((snapE / allhseE)*100,1),
         perc_snaphseM = round(moe_prop(snapE, allhseE, snapM, allhseM), 2),
         .keep = "all")

# Derive foreign born variables
tract_data_b <- tract_data_b %>% 
  mutate(perc_forbE = round((foreignbE / totalpopE)*100,1),
         perc_forbM = round(moe_prop(foreignbE, totalpopE, foreignbM, totalpopM), 2),
         .keep = "all")


# Get Data
# pull tables (easier to just pull tables separately)
tract_race <- get_acs(geography = "tract", 
          table = "DP05", 
          state = "VA", 
          county = fips_codes, 
          survey = "acs5",
          year = acsyr,
          cache_table = TRUE)

tract_age <- get_acs(geography = "tract", 
          table = "S0101", 
          state = "VA", 
          county = fips_codes, 
          survey = "acs5", 
          year = acsyr,
          cache_table = TRUE)

tract_enroll <- get_acs(geography = "tract", 
          table = "S1401", 
          state = "VA", 
          county = fips_codes, 
          survey = "acs5", 
          year = acsyr,
          cache_table = TRUE)

# ....................................................
# Reduce and Combine data ----

# Reduce tables: tract_age, tract_race, tract_enroll
# tract_age: three age groups present as rows in the table,
#             one group must be summed
tract_age17 <- tract_age %>% 
  filter(variable == "S0101_C02_022") %>% 
  rename(age17E = estimate,
         age17M = moe) %>% 
  select(-variable)

tract_age24 <- tract_age %>% 
  filter(variable == "S0101_C02_023") %>% 
  rename(age24E = estimate,
         age24M = moe) %>% 
  select(-variable)

tract_age64 <- tract_age %>% 
  filter(variable %in% c("S0101_C02_007", "S0101_C02_008", "S0101_C02_009",
                         "S0101_C02_010", "S0101_C02_011", "S0101_C02_012",
                         "S0101_C02_013", "S0101_C02_014")) %>% 
  group_by(GEOID, NAME) %>% 
  summarize(age64E = sum(estimate),
            age64M = moe_sum(moe = moe, estimate = estimate))

tract_age65 <- tract_age %>% 
  filter(variable == "S0101_C02_030") %>% 
  rename(age65E = estimate,
         age65M = moe) %>% 
  select(-variable)

# tract_race: all groups present as rows in the table
#             but other race and native hawaiian/pacific islander combined
#             due to very small values
tract_white <- tract_race %>% 
  filter(variable == "DP05_0096P") %>% # white alone, not hispanic
  rename(whiteE = estimate,
         whiteM = moe) %>% 
  select(-variable)

tract_black <- tract_race %>% 
  filter(variable == "DP05_0097P") %>% # black alone not hispanic
  rename(blackE = estimate,
         blackM = moe) %>% 
  select(-variable)

tract_indig <- tract_race %>% 
  filter(variable == "DP05_0098P") %>% # American Indian and Alaska Native alone, not hispanic
  rename(indigE = estimate,
         indigM = moe) %>% 
  select(-variable)

tract_asian <- tract_race %>% 
  filter(variable == "DP05_0099P") %>% # asian alone not hispanic
  rename(asianE = estimate,
         asianM = moe) %>% 
  select(-variable)

tract_othrace <- tract_race %>% 
  filter(variable %in% c("DP05_0100P", "DP05_0101P")) %>% # Native Hawaiian and Other Pacific Islander alone & Some other Race, not hispanic
  group_by(GEOID, NAME) %>% 
  summarize(othraceE = sum(estimate),
            othraceM = moe_sum(moe = moe, estimate = estimate))

tract_multi <- tract_race %>% 
  filter(variable == "DP05_0102P") %>% 
  rename(multiE = estimate,
         multiM = moe) %>% 
  select(-variable)

tract_hisp <- tract_race %>% 
  filter(variable == "DP05_0090P") %>% 
  rename(hispE = estimate,
         hispM = moe) %>% 
  select(-variable)

# tract_schl: 6 groups (3-4, 5-9, 10-14, 15-17, 18-19, 20-24) must be summed
#             population and enrolled, and divided
tract_schl_num <- tract_enroll %>% 
  filter(variable %in% c("S1401_C01_014", "S1401_C01_016", "S1401_C01_018", "S1401_C01_020", "S1401_C01_022", "S1401_C01_024")) %>% 
  group_by(GEOID, NAME) %>% 
  summarize(schl_num = sum(estimate), 
            schl_numM = moe_sum(moe = moe, estimate = estimate))

tract_schl_den <- tract_enroll %>% 
  filter(variable %in% c("S1401_C01_013", "S1401_C01_015", "S1401_C01_017", "S1401_C01_019", "S1401_C01_021", "S1401_C01_023")) %>% 
  group_by(GEOID, NAME) %>% 
  summarize(schl_den = sum(estimate), 
            schl_denM = moe_sum(moe = moe, estimate = estimate))

tract_schl_ratio <- left_join(tract_schl_num, tract_schl_den)

tract_schl <- tract_schl_ratio %>% 
  summarize(schlE = round((schl_num/schl_den)*100,1),
            schlM = moe_prop(schl_num, schl_den, schl_numM, schl_denM),
            schlM = round(schlM*100,1))

# Combine indicators
# joining columns
tract_data <- tract_data_s %>% 
  left_join(tract_data_b) %>% 
  left_join(tract_schl) %>% 
  left_join(tract_white) %>% 
  left_join(tract_black) %>% 
  left_join(tract_indig) %>% 
  left_join(tract_asian) %>% 
  left_join(tract_othrace) %>% 
  left_join(tract_multi) %>% 
  left_join(tract_hisp) %>% 
  left_join(tract_age17) %>% 
  left_join(tract_age24) %>% 
  left_join(tract_age64) %>% 
  left_join(tract_age65)

tract_data <- tract_data %>% 
  mutate(year = as.character(acsyr)) %>% 
  select(GEOID, NAME, year, totalpopE, totalpopM, whiteE, whiteM, blackE, blackM, asianE, asianM, indigE, indigM, othraceE, othraceM, multiE, multiM, hispE, hispM, everything())

tract_data <- tract_data %>% 
  mutate(geoid = GEOID) %>% 
  separate(geoid, into = c("state", "locality", "tract"), 
           sep = c(2,5)) 

# ....................................................
# Summarize/Examine indicators ----
tract_data %>% select_at(vars(ends_with("E"))) %>% summary()

# ....................................................
# Save ----
saveRDS(tract_data, file = paste0(filepath_data, "acs5_tract.RDS")) 

# Remove vars from environment
rm(list = ls(pattern = "^varlist_"))
rm(list = setdiff(ls(pattern = "^tract_"), "tract_data"))
