####################################################
# Greater Charlottesville Region Equity Profile
####################################################
# Acquire Additiona County-Level data
# Last updated: 03/12/2021
# Metrics from various sources: 
# * Life Expectancy Estimates: https://www.countyhealthrankings.org/app/virginia/2020/downloads 
# * Segregation measures (from ACS data, but with more derivation)
#
# TO ADD
# * Historical data on enslaved populations
#
# Geography: Localities in Charlottesville region
#     Charlottesville, Albemarle, Greene, Louisa, 
#     Fluvanna, Nelson, Buckingham, Madison, Orange
#     (include Augusta, Waynesboro, Staunton?)
####################################################
# 1. Load libraries
# 2. Life expectancy estimates
# 3. Segregation measures
# 4. Enslaved population (not yet complete/incorporated)
####################################################

# ....................................................
# 1. Load libraries, provide api key (if needed), identify localities ----

# Load libraries
library(tidyverse)
library(tidycensus)
library(sf)
library(readxl)

ccode <- read_csv("datacode/county_codes.csv")
region <- ccode$code # list of desired counties


# ....................................................
# 2. Small-area life expectancy estimates ----
# a. acquire ----

# https://www.countyhealthrankings.org/app/virginia/2019/measure/outcomes/147/data
# url <- "https://www.countyhealthrankings.org/sites/default/files/media/document/state/downloads/2019%20County%20Health%20Rankings%20Virginia%20Data%20-%20v1_0.xls" # 2019
# url <- "https://www.countyhealthrankings.org/sites/default/files/media/document/2020%20County%20Health%20Rankings%20Virginia%20Data%20-%20v1_0.xlsx" # 2020
# download.file(url, destfile="tempdata/countyhealthrankings2020.xlsx", method="libcurl")

# read data
life_exp <- read_excel("tempdata/countyhealthrankings2020.xlsx", sheet = "Additional Measure Data", skip = 1)

# b. reduce (consider using more from this source), rename, derive
life_exp <- life_exp %>% 
  select(FIPS, locality = County, 
         lifeexpE = `Life Expectancy`, lifeexp_lb = `95% CI - Low...5`, lifeexp_ub = `95% CI - High...6`,
         lifeexp_whiteE = `Life Expectancy (White)`, lifeexp_white_lb = `Life Expectancy (White) 95% CI - Low`, lifeexp_white_ub = `Life Expectancy (White) 95% CI - High`,
         lifeexp_blackE = `Life Expectancy (Black)`, lifeexp_black_lb = `Life Expectancy (Black) 95% CI - Low`, lifeexp_black_ub = `Life Expectancy (Black) 95% CI - High`,
         lifeexp_ltnxE = `Life Expectancy (Hispanic)`, lifeexp_ltnx_lb = `Life Expectancy (Hispanic) 95% CI - Low`, lifeexp_ltnx_ub = `Life Expectancy (Hispanic) 95% CI - High`,
         lifeexp_asianE = `Life Expectancy (Asian)`, lifeexp_asian_lb = `Life Expectancy (Asian) 95% CI - Low`, lifeexp_asian_ub = `Life Expectancy (Asian) 95% CI - High`) %>% 
  mutate(lifeexpM = (lifeexp_ub-lifeexp_lb)/2,
         lifeexp_whiteM = (lifeexp_white_ub-lifeexp_white_lb)/2,
         lifeexp_blackM = (lifeexp_black_ub-lifeexp_black_lb)/2,
         lifeexp_ltnxM = (lifeexp_ltnx_ub-lifeexp_ltnx_lb)/2,
         lifeexp_asianM = (lifeexp_asian_ub-lifeexp_asian_lb)/2,
         fips = str_remove(FIPS, "51"),
         year = "2019") %>% 
  mutate_if(is.numeric, round, 1) %>% 
  select(FIPS, fips, locality, year, lifeexpE, lifeexpM, lifeexp_blackE, lifeexp_blackM, lifeexp_ltnxE, lifeexp_ltnxM, lifeexp_whiteE, lifeexp_whiteM, lifeexp_asianE, lifeexp_asianM)

# c. Limit to region 
life_exp <- life_exp %>% 
  filter(fips %in% region) 

# check
summary(life_exp)

# d. save ----
saveRDS(life_exp, file = "data/county_life_exp.RDS") 
# life_exp <- readRDS("data/county_life_exp.RDS")


# ....................................................
# 3. Segregation measures ----
# a. acquire tract data ----
race_table19 <-get_acs(geography = "tract", 
                        year=2019, 
                        state = "VA",
                        table = "B03002", 
                        survey = "acs5",
                        geometry = F, 
                        output="wide", 
                        cache_table = T)

# rename
seg_tract <- race_table19 %>%
  mutate(white = B03002_003E,
         black = B03002_004E,
         asian = B03002_006E,
         indig = B03002_005E,
         other = B03002_007E + B03002_008E,
         multi = B03002_009E,
         hisp = B03002_012E, 
         total = B03002_001E,
         year = 2019,
         state = substr(GEOID, 1,2),
         county = substr(GEOID, 3,5),
         tract = substr(GEOID, 6,9)) %>% 
  select(GEOID, white, black, indig, asian, other, multi, hisp, total, year, state, county, tract) 

# b. acquire county data ----
race_table19 <- get_acs(geography = "county", 
                        year=2019, 
                        state = "VA",
                        table = "B03002", 
                        survey = "acs5",
                        geometry = F, 
                        output="wide", 
                        cache_table = T)

# rename
seg_county <- race_table19 %>%
  mutate(cowhite = B03002_003E,
         coblack = B03002_004E,
         coasian = B03002_006E,
         coindig = B03002_005E,
         coother = B03002_007E + B03002_008E,
         comulti = B03002_009E,
         cohisp = B03002_012E, 
         cototal = B03002_001E,
         year = "2019",
         state = substr(GEOID, 1,2),
         county = substr(GEOID, 3,5)) %>% 
  select(GEOID, cowhite, coblack, coindig, coasian, coother, comulti, cohisp, cototal, year, state, county) 


# c. Limit to region and derive metrics ----
# nice explanations for segregation measures: 
# https://sejdemyr.github.io/r-tutorials/statistics/measuring-segregation.html
# https://rstudio-pubs-static.s3.amazonaws.com/473785_e782a2a8458d4263ba574c7073ca5057.html

# limit to region
seg_tract <- seg_tract %>% 
  filter(county %in% region) %>% 
  arrange(GEOID)

# add county totals
seg_tract <- left_join(seg_tract, seg_county, by=c("county"))

# generate seg measures
dissim_wb <- seg_tract %>%
  mutate(d.wb = abs(white/cowhite - black/coblack)) %>%
  group_by(county) %>%
  summarise(dissim_wb = .5*sum(d.wb, na.rm=T))

dissim_wh <- seg_tract %>%
  mutate(d.wh = abs(white/cowhite - hisp/cohisp)) %>%
  group_by(county) %>%
  summarise(dissim_wh = .5*sum(d.wh, na.rm=T))

inter_bw <- seg_tract %>%
  mutate(int.bw=(black/coblack * white/total))%>%
  group_by(county)%>%
  summarise(inter_bw= sum(int.bw, na.rm=T))

inter_hw <- seg_tract %>%
  mutate(int.hw=(hisp/cohisp * white/total))%>%
  group_by(county)%>%
  summarise(inter_hw= sum(int.hw, na.rm=T))

isol_b <- seg_tract %>%
  mutate(isob=(black/coblack * black/total) )%>%
  group_by(county) %>%
  summarise(iso_b = sum(isob, na.rm=T))

isol_h <- seg_tract %>%
  mutate(isoh=(hisp/cohisp * hisp/total)) %>%
  group_by(county) %>%
  summarise(iso_h = sum(isoh, na.rm=T))

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
  mutate(year = "2019")

# check
summary(seg_county)
pairs(seg_county[2:7])

# d. save ----
saveRDS(seg_county, file = "data/seg_county.RDS")
# seg_county <- readRDS("data/seg_county.RDS")


# ....................................................
# 4. History of enslaved population ----

# 1790 data from NHGIS
# requires an account to query and download desired data; can't call programmatically

# read data, rename, and filter for VA/Cville region
ens1790 <- read_csv("tempdata/nhgis0001_csv/nhgis0001_ds1_1790_county.csv")
names(ens1790) <- c("gisjoin", "year", "state", "state_cd", "county", "county_cd", "areaname",
                "totalpop", "nonwhite_free", "nonwhite_slave", "white", "totalfam", 
                "sh_family", "nsh_family", "totalslave", "avg_slave_fam", "white2", "nonwhite2")
ens1790 <- ens1790 %>% 
  filter(state == "Virginia") %>% 
  filter(county_cd %in% c("0030", "1090", "0650", "0150", "1370", "0290", "0090", "0470")) %>% 
  select(gisjoin, year, county, county_cd, totalpop:white)

# Derive metric: percent slaves, nonwhite
ens1790 <- ens1790 %>% 
  mutate(per_enslaved = round((nonwhite_slave/totalpop)*100,1),
         per_nonwhite = round(((nonwhite_slave+nonwhite_free)/totalpop)*100,1)
         )

# read shapefiles, filter for VA, check localities
geo1790_00 = st_read(dsn = "tempdata/nhgis0001_shape/nhgis0001_shapefile_tl2000_us_county_1790/US_county_1790.shp")
# geometry type:  MULTIPOLYGON
# dimension:      XY
# epsg (SRID):    102003
# proj4string:    +proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs

geo1790_00 <- geo1790_00 %>% 
  filter(STATE == "510") %>% 
  filter(COUNTY %in% ens1790$county_cd) %>% 
  select(NHGISNAM, COUNTY:SHAPE_LEN)

plot(geo1790_00["COUNTY"])


# 1860 data from NHGIS

ens1860 <- read_csv("tempdata/nhgis0002_csv/nhgis0002_ds14_1860_county.csv")
names(ens1860) <- c("gisjoin", "year", "state", "state_cd", "county", "county_cd", "areaname",
                 "totalpop", "white", "nonwhite_free", "nonwhite_slave", "indian", "multi",
                 "asian", 
                 "sl1", "sl2", "sl3", "sl4", "sl5", "sl6", "sl7", "sl8", "sl9", "sl10" ,
                 "sl15", "sl20", "sl30", "sl40", "sl50", "sl70", "sl100", "sl200", "sl300",
                 "sl500", "sl1000", "tot_sh", "totalslave", "totalfree")

ens1860 <- ens1860 %>% 
  filter(state == "Virginia") %>% 
  filter(county_cd %in% c("0030", "1090", "0650", "0150", "1370", "0290", "0790", "1250", "1130")) %>% 
  select(gisjoin, year, county, county_cd, totalpop:nonwhite_slave)

# Derive metric: percent slaves, nonwhite
ens1860 <- ens1860 %>% 
  mutate(per_enslaved = round((nonwhite_slave/totalpop)*100,1),
         per_nonwhite = round(((nonwhite_slave+nonwhite_free)/totalpop)*100,1)
  )


# read shapefiles, filter for VA, check localities
geo1860_00 = st_read(dsn = "tempdata/nhgis0003_shape/nhgis0003_shapefile_tl2000_us_county_1860/US_county_1860.shp")
# geometry type:  MULTIPOLYGON
# dimension:      XY
# epsg (SRID):    102003
# proj4string:    +proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs

geo1860_00 <- geo1860_00 %>% 
  filter(STATE == "510") %>% 
  filter(COUNTY %in% ens1860$county_cd) %>% 
  select(NHGISNAM, COUNTY:SHAPE_LEN)

plot(geo1860_00["COUNTY"])

