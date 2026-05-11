# ALICE County-Level data
# Last updated: 4/30/2026

# This script gets and cleans additional county-level ALICE data for a single year. 
# The resulting file is saved as `alice.RDS`

# Source: 
# * Virginia ALICE data: https://www.unitedforalice.org/county-reports/virginia

# Load libraries ----
library(tidyverse)
library(readxl)
library(janitor)

# Create basic objects ----
# These variables are set in dashboard-update-workflow.Rmd
fips_codes <- fips_codes
filepath_data <- filepath_data

# ....................................................
# ALICE ----
# Acquire ----
if (!dir.exists("data/tempdata")){
  dir.create("data/tempdata")}

# https://www.unitedforalice.org/county-reports/virginia
# Copy url link for `Data Sheet`
url <- "https://www.unitedforalice.org/Attachments/StateDataSheet/2025%20ALICE%20-%20Virginia%20Data%20Sheet.xlsx"

download.file(url, destfile="data/tempdata/alice2025.xlsx", method="libcurl")

# Read data
alice_raw <- read_excel("data/tempdata/alice2025.xlsx", sheet = "County") %>% 
  clean_names()

# Limit to region 
alice <- alice_raw %>% 
  mutate(locality = str_remove(geo_id2, "51")) %>% 
  filter(locality %in% fips_codes) %>% 
  rename(GEOID = geo_id2)

# Derive percent for ALICE households
alice <- alice %>% 
  mutate(across(c(poverty_households:above_alice_households), ~ (.x/households)*100, .names = "per_{.col}"))

# Reduce to most recent year
alice <- alice %>% 
  filter(year == max(year)) %>% 
  mutate(coname = str_to_title(county))

# Select columns and rename
alice <- alice %>% 
  select(GEOID, locality, coname,
         poverty_households:alice_threshold_hh_65_years_and_over, 
         per_poverty_households:per_above_alice_households)
  

# d. save ----
saveRDS(alice, file = paste0(filepath_data, "alice.RDS")) 
