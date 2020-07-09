####################################################
# Greater Charlottesville Region Equity Profile
####################################################
# Combine data for shiny app
# Last updated: 07/08/2020
####################################################
# 1. Load libraries 
# 2. Load data
# 3. Merge tract attributes, county attributes
# 4. Add geography (parks, schools/attendance zones, mag dist)
# 5. Read in crosswalk and join
# 6. Define color palettes
# 7. Save for app
####################################################


# ....................................................
# 1. Load libraries and data ----
# Libraries
library(tidyverse)
library(RColorBrewer)
library(googlesheets4)
library(sf)
library(tools)
library(tigris)
library(sp)
library(geosphere)


# function to move variables to end
move_last <- function(DF, last_col) {
  match(c(setdiff(names(DF), last_col), last_col), names(DF))
}


# ....................................................
# 2. Load data ----
# block group data
blkgrp_data <- readRDS("data/blkgrp_data.RDS")

# tract level ACS
tract_data <- readRDS("data/tract_data.RDS")
tract_data_time <- readRDS("data/tract_data_time.RDS")
life_exp_tract <- readRDS("data/tract_life_exp.RDS")

# county level ACS
county_data <- readRDS("data/county_data.RDS")
county_data_time <- readRDS("data/county_data_time.RDS")
life_exp_county <- readRDS("data/county_life_exp.RDS")
seg_county <- readRDS("data/seg_county.RDS")

# points and polygons
parks_sf <- st_read("data/parks_sf.geojson") 
schools_sf <- st_read("data/schools_sf.geojson") # may want to segment by type (public, private)
sabselem_sf <- st_read("data/sabselem_sf.geojson")
sabshigh_sf <- st_read("data/sabshigh_sf.geojson")
mcd_sf <- st_read("data/mcd_sf.geojson")
# other files as needed: polygons and points

ccode <- read_csv("datacode/county_codes.csv")
region <- ccode$code # list of desired counties


# ....................................................
# 2. Merge tract, county attributes, derive HDI ----
# join tract/tract_time, county/county_time
tract_data <- bind_rows(tract_data, tract_data_time)
county_data <- bind_rows(county_data, county_data_time)

# add life expectancy by tract
life_exp_tract <- life_exp_tract %>% select(geoid, year, life_expE, life_expM) %>% 
  mutate(geoid = as.character(geoid))

tract_data <- tract_data %>% 
  left_join(life_exp_tract, by = c("GEOID" = "geoid", "year" = "year")) %>% 
  select(move_last(., c("state", "locality", "tract"))) 

# add life expectancy by county
county_data <- county_data %>% 
  left_join(life_exp_county, by = c("GEOID" = "FIPS", "year" = "year")) %>% 
  rename(locality = "locality.x") %>% select(-locality.y) %>% 
  select(move_last(., c("state", "locality"))) 

# add segregation measures by county
county_data <- county_data %>% 
  left_join(seg_county, by = c("locality" = "county", "year" = "year")) %>% 
  select(move_last(., c("state", "locality")))

# generate HDI measure: function of school enrollment, educ attainment; life expectancy; median personal earnings
# "goalposts" defined in methodology: http://measureofamerica.org/Measure_of_America2013-2014MethodNote.pdf
# earnings goalposts are adjusted for inflation -- set to 2015 values
tract_data <- tract_data %>% 
  mutate(hlth_index = ( (life_expE-66) / (90-66) * 10),
         inc_index = ( (log(earnE)-log(15776.86)) / (log(66748.26)-log(15776.86)) * 10),
         attain_index = ( (((hsmoreE/100 + bamoreE/100 + gradmoreE/100)-0.5)/ (2-0.5)) *10),
         enroll_index = (schlE-60)/(95-60)*10,
         educ_index = attain_index*(2/3) + enroll_index*(1/3),
         hd_index = round((hlth_index + educ_index + inc_index)/3,1))

tract_data <- tract_data %>% 
  select(-c("hlth_index", "inc_index", "attain_index", "enroll_index", "educ_index")) %>% 
  select(move_last(., c("state", "locality", "tract")))

# add hd_index to county
county_data <- county_data %>% 
  mutate(hlth_index = ( (life_expE-66) / (90-66) * 10),
         inc_index = ( (log(earnE)-log(15776.86)) / (log(66748.26)-log(15776.86)) * 10),
         attain_index = ( (((hsmoreE/100 + bamoreE/100 + gradmoreE/100)-0.5)/ (2-0.5)) *10),
         enroll_index = (schlE-60)/(95-60)*10,
         educ_index = attain_index*(2/3) + enroll_index*(1/3),
         hd_index = round((hlth_index + educ_index + inc_index)/3,1))

county_data <- county_data %>% 
  select(-c("hlth_index", "inc_index", "attain_index", "enroll_index", "educ_index")) %>% 
  select(move_last(., c("state", "locality")))


# ....................................................
# 3. Read in crosswalk and join ----
# read pretty table: contains better variable lables, sources, and descriptions
# gs_auth(new_user = TRUE)

googlesheets4::gs4_deauth()
url_sheet <- "https://docs.google.com/spreadsheets/d/1hwR-U4ykkT4s-ZGaBXhBOaCt78ull4DT2kH51d-7Phg/edit?usp=sharing"
# prettytab <- gs_title("prettytable")
pretty <- googlesheets4::read_sheet(url_sheet, sheet = "acs_tract")
pretty$goodname <- toTitleCase(pretty$description)

pretty2 <- googlesheets4::read_sheet(url_sheet, sheet = "acs_county")
pretty2$goodname <- toTitleCase(pretty2$description)

pretty3 <- googlesheets4::read_sheet(url_sheet, sheet = "acs_blockgroup")
pretty3$goodname <- toTitleCase(pretty3$description)

# join pretty names to existing tract data
tab <- select(tract_data, locality, NAME)
tab <- separate(tab, NAME,
                into=c("tract","county.nice", "state"), sep=", ", remove=F)

tab <- unique(select(tab, locality, county.nice))
tract_data <- left_join(tract_data, tab, by="locality")

# join pretty names to existing county data
tab2 <- select(county_data, locality, NAME)
tab2 <- separate(tab2, NAME,
                into=c("county.nice", "state"), sep=", ", remove=F)

tab2 <- unique(select(tab2, locality, county.nice))
county_data <- left_join(county_data, tab2, by=c("locality"))

# join pretty names to existing blockgroup data
tab3 <- select(blkgrp_data, locality, NAME)
tab3 <- separate(tab3, NAME,
                 into=c("block.group", "tract", "county.nice", "state"), sep=", ", remove=F)

tab3 <- unique(select(tab2, locality, county.nice))
blkgrp_data <- left_join(blkgrp_data, tab3, by=c("locality"))


# ....................................................
# 4. Add geography  ----
# get tract polygons
geo <- tracts(state = 'VA', county = region) # from tigris

# join coordinates to data
tract_data_geo <- merge(geo, tract_data, by = "GEOID", duplicateGeoms = TRUE) # from sp -- keep all obs (full_join)
# tract_data_geo2 <- geo_join(geo, tract_data, by = "GEOID") # from sf -- keep only 2017 obs (left_join)
names(tract_data_geo@data)[names(tract_data_geo@data)=="NAME.y"] <- "NAME"

# add centroid coordinates for tract polygons: from geosphere
# as possible way of visualizing/layering a second attribute
tract_data_geo$ctr <- centroid(tract_data_geo)
tract_data_geo$lng <- tract_data_geo$ctr[,1]
tract_data_geo$lat <- tract_data_geo$ctr[,2]


# get locality polygons
counties_geo <- counties(state = 'VA') # from tigris
counties_geo <- counties_geo %>% subset(COUNTYFP %in% region)

# join coordinates to data
county_data_geo <- merge(counties_geo, county_data, by = "GEOID", duplicateGeoms = TRUE) # from sp -- keep all obs (full_join)
# county_data_geo2 <- geo_join(counties_geo, county_data, by = "GEOID") # from sf -- keep only 2017 obs (left_join)
# rename for consistency (NAME references geo label in for each geography level)
names(county_data_geo@data)[names(county_data_geo@data)=="NAME.y"] <- "NAME"

# add centroid coordinates for tract polygons
# as possible way of visualizing/layering a second attribute
county_data_geo$ctr <- centroid(county_data_geo)
county_data_geo$lng <- county_data_geo$ctr[,1]
county_data_geo$lat <- county_data_geo$ctr[,2]


# get block group polygons
blkgrp_geo <- block_groups(state = 'VA', county = region) # from tigris

# join coordinates to data
blkgrp_data_geo <- merge(blkgrp_geo, blkgrp_data, by = "GEOID", duplicateGeoms = TRUE) # from sp -- keep all obs (full_join)

# add centroid coordinates for tract polygons
# as possible way of visualizing/layering a second attribute
blkgrp_data_geo$ctr <- centroid(blkgrp_data_geo)
blkgrp_data_geo$lng <- blkgrp_data_geo$ctr[,1]
blkgrp_data_geo$lat <- blkgrp_data_geo$ctr[,2]


# ....................................................
# 5. Metrics for snapshop valuebox ----
county_dash <- county_data %>% filter(year == "2018") %>% 
  select(GEOID, county.nice, totalpopE, hd_index, hhincE, cpovrateE, life_expE, renter30E)


# ....................................................
# 6. Define color palettes ----
nb.cols <- 10
mycolors <- colorRampPalette(brewer.pal(8, "YlGnBu"))(nb.cols)


# ....................................................
# 7. Save for app ----
save.image(file = "data/combine_data.Rdata") # for updates
# load("data/combine_data.Rdata")

rm(ccode, geo, blkgrp_geo, life_exp_tract, life_exp_county, seg_county, 
   county_data_time, tract_data_time, tab, tab2, tab3, region, move_last)

save.image(file = "data/app_data.Rdata") 
save.image(file = "cville-region/www/app_data.Rdata")
# load("data/app_data.Rdata")


