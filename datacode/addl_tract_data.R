####################################################
# Greater Charlottesville Region Equity Profile
####################################################
# Acquire Additiona Tract-Level data
# Last updated: 01/03/2020
# Metrics from various sources: 
# * Small Area Life Expectancy Estimates: https://www.cdc.gov/nchs/nvss/usaleep/usaleep.html 
# * Segregation measures (from ACS data, but with more derivation)
#
# TO ADD
# * HMDA relevant metrics
#
# Geography: Tracts in Localities in Charlottesville region
#     Charlottesville, Albemarle, Greene, Louisa, 
#     Fluvanna, Nelson, Buckingham, Madison, Orange
#     (include Augusta, Waynesboro, Staunton?)
####################################################
# 1. Load libraries
# 2. Small-area life expectancy estimates
# 3. HMDA measures (not yet complete/incorporated)
####################################################

# ....................................................
# 1. Load libraries, provide api key (if needed), identify localities ----

# Load libraries
library(tidyverse)
library(tidycensus)

ccode <- read_csv("datacode/county_codes.csv")
region <- ccode$code # list of desired counties


# ....................................................
# 2. Small-area life expectancy estimates ----
# a. acquire ----

# url <- "https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NVSS/USALEEP/CSV/VA_A.CSV"
# download.file(url, destfile="tempdata/va_usasleep.csv", method="libcurl")

# read data and rename
lifeexp <- read_csv("tempdata/va_usasleep.csv")
names(lifeexp) <- c("geoid", "state", "county", "tract", "life_exp", "se", "flag")

# b. Limit to region and derive metrics ----
lifeexp <- lifeexp %>% 
  filter(county %in% region) %>% # 5 missing tracts (80 of 85)
  rename(lifeexpE = life_exp,
         locality = county) %>% 
  mutate(lifeexpM = 1.64*se,
         year = "2018") %>% 
  select(-se, -flag)

# check
summary(lifeexp)

# c. save ----
saveRDS(lifeexp, file = "data/tract_life_exp.RDS") 
# life_exp <- readRDS("data/tract_life_exp.RDS")


# ....................................................
# 3. HMDA measures ----
# url <- "https://s3.amazonaws.com/cfpb-hmda-public/prod/snapshot-data/2018/2018_public_lar_csv.zip"
# download.file(url, destfile="tempdata/hmda2018.zip", method="libcurl")
# unzip("tempdata/hmda2018.zip", exdir = "tempdata/hmda2018full")

# data dictionary: https://ffiec.cfpb.gov/documentation/2018/lar-data-fields/

hmda2018 <- read_csv("tempdata/hmda2018full/2018_public_lar_csv.csv")
hmda2018 <- hmda2018 %>% filter(state_code == "VA") # limit to VA

hmda2018 <- hmda2018 %>% 
  mutate(locality = substr(county_code, 3,5), 
         state_fip = substr(county_code, 1,2)) # create locality

hmda2018 <- hmda2018 %>% 
  filter(state_fip == "51") %>% 
  filter(locality %in% region) # limit to region

# hmda2018 <- hmda2018 %>% 
#   filter(action_taken == 1, loan_purpose == 1) 
# originated loans/completed purchases; for home purchase (not improvement, refinancing, etc.)

# number of purchases by white, black, asian, indig, hispanic

# maybe get past data
# https://www.consumerfinance.gov/data-research/hmda/historic-data/?geo=va&records=all-records&field_descriptions=codes
# http://cfpb.github.io/api/hmda/index.html




