# Life Expectancy County-Level data
# Last updated: 4/9/2026

# This script gets and cleans additional county-level life expectancy data for a single year. 
# The resulting file is saved as `county_life_exp.RDS`

# Sources include: 
# * Life Expectancy Estimates: https://www.countyhealthrankings.org/health-data/virginia/data-and-resources
# * Small-area life expectancy estimates: https://www.cdc.gov/nchs/nvss/usaleep/usaleep.html
#    - New data has not been published since the 2010-2015 estimates (last checked 4/9/2029) 
#    - Removed from script in 2026 update

# Load libraries ----
library(tidyverse)
library(readxl)

# Create basic objects ----
# These variables are set in dashboard-update-workflow.Rmd
fips_codes <- fips_codes
filepath_data <- filepath_data

# ....................................................
# Small-area life expectancy estimates ----
# a. acquire ----
if (!dir.exists("data/tempdata")){
  dir.create("data/tempdata")}

# https://www.countyhealthrankings.org/health-data/virginia/data-and-resources
# https://www.countyhealthrankings.org/app/virginia/2022/downloads
# url <- "https://www.countyhealthrankings.org/sites/default/files/media/document/state/downloads/2019%20County%20Health%20Rankings%20Virginia%20Data%20-%20v1_0.xls" # 2019
# url <- "https://www.countyhealthrankings.org/sites/default/files/media/document/2020%20County%20Health%20Rankings%20Virginia%20Data%20-%20v1_0.xlsx" # 2020
# url <- "https://www.countyhealthrankings.org/sites/default/files/media/document/2021%20County%20Health%20Rankings%20Virginia%20Data%20-%20v1_0.xlsx" # 2021
# url <- "https://www.countyhealthrankings.org/sites/default/files/media/document/2022%20County%20Health%20Rankings%20Virginia%20Data%20-%20v1.xlsx" # 2022
# url <- "https://www.countyhealthrankings.org/sites/default/files/media/document/2023%20County%20Health%20Rankings%20Virginia%20Data%20-%20v3.xlsx" # 2023
# url <- "https://www.countyhealthrankings.org/sites/default/files/media/document/2024_county_health_release_virginia_data_-_v1.xlsx" # 2024
# url <- "https://www.countyhealthrankings.org/sites/default/files/media/document/2025_county_health_rankings_virginia_data_-_v1.xlsx" # 2025
url <- "https://www.countyhealthrankings.org/sites/default/files/media/document/2025%20County%20Health%20Rankings%20Virginia%20Data%20-%20v4.xlsx"

download.file(url, destfile="data/tempdata/countyhealthrankings2026.xlsx", method="libcurl")

# read data
life_exp <- read_excel("data/tempdata/countyhealthrankings2026.xlsx", sheet = "Additional Measure Data", skip = 1)

# b. reduce (consider using more from this source), rename, derive
life_exp <- life_exp %>% 
  select(FIPS, coname = County, 
         lifeexpE = `Life Expectancy`, lifeexp_lb = `95% CI - Low...5`, lifeexp_ub = `95% CI - High...6`,
         lifeexp_hispE = `Life Expectancy (Hispanic (all races))`, lifeexp_hisp_lb = `Life Expectancy (Hispanic (all races)) 95% CI - Low`, lifeexp_hisp_ub = `Life Expectancy (Hispanic (all races)) 95% CI - High`,
         lifeexp_asianE = `Life Expectancy (Non-Hispanic Asian)`, lifeexp_asian_lb = `Life Expectancy (Non-Hispanic Asian) 95% CI - Low`, lifeexp_asian_ub = `Life Expectancy (Non-Hispanic Asian) 95% CI - High`,
         lifeexp_blackE = `Life Expectancy (Non-Hispanic Black)`, lifeexp_black_lb = `Life Expectancy (Non-Hispanic Black) 95% CI - Low`, lifeexp_black_ub = `Life Expectancy (Non-Hispanic Black) 95% CI - High`,
         lifeexp_whiteE = `Life Expectancy (Non-Hispanic White)`, lifeexp_white_lb = `Life Expectancy (Non-Hispanic White) 95% CI - Low`, lifeexp_white_ub = `Life Expectancy (Non-Hispanic White) 95% CI - High`) %>% 
  mutate(lifeexpM = (lifeexp_ub-lifeexp_lb)/2,
         lifeexp_hispM = (lifeexp_hisp_ub-lifeexp_hisp_lb)/2,
         lifeexp_asianM = (lifeexp_asian_ub-lifeexp_asian_lb)/2,
         lifeexp_blackM = (lifeexp_black_ub-lifeexp_black_lb)/2,
         lifeexp_whiteM = (lifeexp_white_ub-lifeexp_white_lb)/2,
         fips = str_remove(FIPS, "51")) %>% 
  mutate_if(is.numeric, round, 1) %>% 
  select(FIPS, fips, coname, lifeexpE, lifeexpM, lifeexp_hispE, lifeexp_hispM, lifeexp_asianE, lifeexp_asianM, lifeexp_blackE, lifeexp_blackM, lifeexp_whiteE, lifeexp_whiteM)

# c. Limit to region 
life_exp <- life_exp %>% 
  filter(fips %in% fips_codes) %>% 
  rename(GEOID = FIPS,
         locality = fips)

# check
summary(life_exp)

# d. save ----
saveRDS(life_exp, file = paste0(filepath_data, "county_life_exp.RDS")) 
