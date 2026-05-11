# Segregation measures for county and tract-level
# Last updated: 4/9/2026

# This script gets and derives county-level and tract level segragation data for a single year. 
# The resulting files are saved as `seg_county.RDS` and `seg_tract.RDS`

# Sources include: 
# * Segregation measures (from ACS data, but with more derivation)

# Load libraries ----
library(tidyverse)
library(tidycensus)

# Create basic objects ----
# These variables are set in dashboard-update-workflow.Rmd
dyr <- dyr
acsyr <- acsyr
fips_codes <- fips_codes
filepath_data <- filepath_data

# ....................................................
# County-level segregation measures ----
# a. acquire tract data ----
acs_B03002_tract <- get_acs(geography = "tract", 
                      year= acsyr, 
                      state = "VA",
                      county = fips_codes,
                      table = "B03002", 
                      survey = "acs5",
                      geometry = FALSE, 
                      output = "wide", 
                      cache_table = TRUE)

# rename
race_tract_table <- acs_B03002_tract %>%
  mutate(white = B03002_003E,
         black = B03002_004E,
         asian = B03002_006E,
         indig = B03002_005E,
         other = B03002_007E + B03002_008E,
         multi = B03002_009E,
         hisp = B03002_012E, 
         total = B03002_001E,
         year = as.character(acsyr),
         state = substr(GEOID, 1,2),
         county = substr(GEOID, 3,5),
         tract = substr(GEOID, 6,9)) %>% 
  select(GEOID, white, black, indig, asian, other, multi, hisp, total, year, state, county, tract) 

# b. acquire county data ----
acs_B03002_county <- get_acs(geography = "county", 
                      year = acsyr, 
                      state = "VA",
                      county = fips_codes,
                      table = "B03002", 
                      survey = "acs5",
                      geometry = FALSE, 
                      output="wide", 
                      cache_table = TRUE)

# rename
race_county_table <- acs_B03002_county %>%
  mutate(cowhite = B03002_003E,
         coblack = B03002_004E,
         coasian = B03002_006E,
         coindig = B03002_005E,
         coother = B03002_007E + B03002_008E,
         comulti = B03002_009E,
         cohisp = B03002_012E, 
         cototal = B03002_001E,
         year = as.character(acsyr),
         state = substr(GEOID, 1,2),
         county = substr(GEOID, 3,5)) %>% 
  select(GEOID, cowhite, coblack, coindig, coasian, coother, comulti, cohisp, cototal, year, state, county) 

# c. Derive metrics ----
# nice explanations for segregation measures: 
# https://sejdemyr.github.io/r-tutorials/statistics/measuring-segregation.html
# https://rstudio-pubs-static.s3.amazonaws.com/473785_e782a2a8458d4263ba574c7073ca5057.html

# add county totals
race_tract_table <- left_join(race_tract_table, race_county_table, by=c("county", "year"))

# generate seg measures
dissim_wb <- race_tract_table %>%
  mutate(d.wb = abs(white/cowhite - black/coblack)) %>%
  group_by(county) %>%
  summarise(dissim_wb = .5*sum(d.wb, na.rm=T))

dissim_wh <- race_tract_table %>%
  mutate(d.wh = abs(white/cowhite - hisp/cohisp)) %>%
  group_by(county) %>%
  summarise(dissim_wh = .5*sum(d.wh, na.rm=T))

inter_bw <- race_tract_table %>%
  mutate(int.bw=(black/coblack * white/total))%>%
  group_by(county)%>%
  summarise(inter_bw= sum(int.bw, na.rm=T))

inter_hw <- race_tract_table %>%
  mutate(int.hw=(hisp/cohisp * white/total))%>%
  group_by(county)%>%
  summarise(inter_hw= sum(int.hw, na.rm=T))

isol_b <- race_tract_table %>%
  mutate(isob=(black/coblack * black/total) )%>%
  group_by(county) %>%
  summarise(iso_b = sum(isob, na.rm=T))

isol_h <- race_tract_table %>%
  mutate(isoh=(hisp/cohisp * hisp/total)) %>%
  group_by(county) %>%
  summarise(iso_h = sum(isoh, na.rm=T))

# join measures
seg_county <- dissim_wb %>% 
  left_join(dissim_wh) %>% 
  left_join(inter_bw) %>% 
  left_join(inter_hw) %>% 
  left_join(isol_b) %>% 
  left_join(isol_h)
# could estimate spatial segregation with seg package as well

# round
seg_county <- seg_county %>% 
  mutate_if(is.numeric, round, 3) %>% 
  mutate(year = as.character(acsyr))

# check
summary(seg_county)
pairs(seg_county[2:7])

# d. save ----
saveRDS(seg_county, file = paste0(filepath_data, "seg_county.RDS")) 

# Remove vars from environment
rm(list = ls(pattern = "^(race_|dissim_|inter_|isol_)"))

# ....................................................
# Tract-level segregation measures ----
# a. acquire block group data ----
# Retreived above
acs_B03002_blkgrp <- get_acs(geography = "block group", 
                       year = acsyr, 
                       state = "VA",
                       county = fips_codes, 
                       table = "B03002", 
                       survey = "acs5",
                       geometry = FALSE, 
                       output="wide", 
                       cache_table = TRUE)

# rename
race_blkgrp_table <- acs_B03002_blkgrp %>%
  mutate(white = B03002_003E,
         black = B03002_004E,
         asian = B03002_006E,
         indig = B03002_005E,
         other = B03002_007E + B03002_008E,
         multi = B03002_009E,
         hisp = B03002_012E, 
         total = B03002_001E,
         year = as.character(acsyr),
         state = substr(GEOID, 1,2),
         county = substr(GEOID, 3,5),
         tract = substr(GEOID, 6,11),
         blkgrp = substr(GEOID, 12, 12)) %>% 
  select(GEOID, white, black, indig, asian, other, multi, hisp, total, year, state, county, tract, blkgrp) 

# b. acquire tract data ----
# Retrieved above
# acs_B03002_tract <- get_acs(geography = "tract",
#                         year = acsyr,
#                         state = "VA",
#                         county = fips_codes,
#                         table = "B03002",
#                         survey = "acs5",
#                         geometry = FALSE,
#                         output="wide",
#                         cache_table = TRUE)

# rename
race_tract_table <- acs_B03002_tract %>%
  mutate(trwhite = B03002_003E,
         trblack = B03002_004E,
         trasian = B03002_006E,
         trindig = B03002_005E,
         trother = B03002_007E + B03002_008E,
         trmulti = B03002_009E,
         trhisp = B03002_012E, 
         trtotal = B03002_001E,
         year = as.character(acsyr),
         state = substr(GEOID, 1,2),
         county = substr(GEOID, 3,5),
         tract = substr(GEOID, 6,11)) %>% 
  select(GEOID, trwhite, trblack, trindig, trasian, trother, trmulti, trhisp, trtotal, year, state, county, tract) 

# c. Derive metrics ----
# nice explanations for segregation measures: 
# https://sejdemyr.github.io/r-tutorials/statistics/measuring-segregation.html
# https://rstudio-pubs-static.s3.amazonaws.com/473785_e782a2a8458d4263ba574c7073ca5057.html

# add tract totals
race_blkgrp_table <- left_join(race_blkgrp_table, race_tract_table, by=c("county", "tract"))

# generate seg measures
dissim_wb <- race_blkgrp_table %>%
  mutate(d.wb = abs(white/trwhite - black/trblack)) %>%
  group_by(county, tract) %>%
  summarise(dissim_wb = .5*sum(d.wb, na.rm=T))

dissim_wh <- race_blkgrp_table %>%
  mutate(d.wh = abs(white/trwhite - hisp/trhisp)) %>%
  group_by(county, tract) %>%
  summarise(dissim_wh = .5*sum(d.wh, na.rm=T))

inter_bw <- race_blkgrp_table %>%
  mutate(int.bw=(black/trblack * white/total))%>%
  group_by(county, tract) %>%
  summarise(inter_bw= sum(int.bw, na.rm=T))

inter_hw <- race_blkgrp_table %>%
  mutate(int.hw=(hisp/trhisp * white/total))%>%
  group_by(county, tract) %>%
  summarise(inter_hw= sum(int.hw, na.rm=T))

isol_b <- race_blkgrp_table %>%
  mutate(isob=(black/trblack * black/total) )%>%
  group_by(county, tract) %>%
  summarise(iso_b = sum(isob, na.rm=T))

isol_h <- race_blkgrp_table %>%
  mutate(isoh=(hisp/trhisp * hisp/total)) %>%
  group_by(county, tract) %>%
  summarise(iso_h = sum(isoh, na.rm=T))

# join segregation measures
seg_tract <- dissim_wb %>% 
  left_join(dissim_wh) %>% 
  left_join(inter_bw) %>% 
  left_join(inter_hw) %>% 
  left_join(isol_b) %>% 
  left_join(isol_h)
# could estimate spatial segregation with seg package as well

# round
seg_tract <- seg_tract %>% 
  mutate_if(is.numeric, round, 3) 

# check
summary(seg_tract)
pairs(seg_tract[3:8])

# d. save ----
saveRDS(seg_tract, file = paste0(filepath_data, "seg_tract.RDS")) 

# Remove vars from environment
rm(list = ls(pattern = "^(acs_|race_|dissim_|inter_|isol_)"))