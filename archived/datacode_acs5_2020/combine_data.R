####################################################
# Greater Charlottesville Regional Equity Atlas (2020)
####################################################
# Combine data for shiny app
# Last updated: 01/11/2023
####################################################
# 1. Load libraries 
# 2. Load data
# 3. Merge tract, county data
# 4. Join variable attributes
# 5. Add geography 
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
blkgrp_data <- readRDS("data20/blkgrp_data.RDS")

# tract level ACS
tract_data <- readRDS("data20/tract_data.RDS")

lifeexp_tract <- readRDS("data20/tract_life_exp.RDS") 
lifeexp_tract <- lifeexp_tract %>% select(geoid, lifeexpE, lifeexpM) %>% 
  mutate(geoid = as.character(geoid))
# not merging by year any longer; should probably remove from addl_tract_data.R

seg_tract <- readRDS("data20/seg_tract.RDS")

hmda_tract <- read.csv("data20/hmda_cville_tract_avg2016-2021.csv")
hmda_tract$census_tract <- as.character(hmda_tract$census_tract)
# these also appear to contain only the six planning district localities

# county level ACS
county_data <- readRDS("data20/county_data.RDS")
lifeexp_county <- readRDS("data20/county_life_exp.RDS") 
lifeexp_county <- lifeexp_county %>% select(-year)
# not merging by year any longer; should probably remove from addl_county_data.R
seg_county <- readRDS("data20/seg_county.RDS")

# points and polygons
parks_sf <- st_read("data20/parks_OSM_sf.geojson") 
schools_sf <- st_read("data20/schools_sf.geojson") # may want to segment by type (public, private)
sabselem_sf <- st_read("data20/sabselem_sf.geojson")
sabshigh_sf <- st_read("data20/sabshigh_sf.geojson")
mcd_sf <- st_read("data20/mcd_sf.geojson") # as of 1/2022 (Alb not updated yet; others?)
# other files as needed: polygons and points

ccode <- read_csv("data20/county_codes.csv")
ccode <- ccode %>% mutate(
  code = as.character(code),
  code = str_pad(code, width = 3, side = "left", pad = "0")
  )
region <- ccode$code # list of desired counties

# variable metadata/attributes
# pretty table contains better variable lables, sources, and descriptions
url_sheet <- "https://docs.google.com/spreadsheets/d/1hwR-U4ykkT4s-ZGaBXhBOaCt78ull4DT2kH51d-7Phg/edit?usp=sharing"
pretty <- googlesheets4::read_sheet(url_sheet, sheet = "acs_tract")
pretty$goodname <- toTitleCase(pretty$description)

pretty2 <- googlesheets4::read_sheet(url_sheet, sheet = "acs_county")
pretty2$goodname <- toTitleCase(pretty2$description)

pretty3 <- googlesheets4::read_sheet(url_sheet, sheet = "acs_blockgroup")
pretty3$goodname <- toTitleCase(pretty3$description)


# ....................................................
# 3. Merge tract, county data, derive HDI ----
# a. Merge tract data ----

# add life expectancy by tract
tract_data <- tract_data %>% 
  left_join(lifeexp_tract, by = c("GEOID" = "geoid")) %>% 
  select(move_last(., c("state", "locality", "tract"))) 

# add segregation measures by county
tract_data <- tract_data %>% 
  left_join(seg_tract, by = c("locality" = "county", "tract" = "tract", "year" = "year")) %>% 
  select(move_last(., c("state", "locality", "tract")))

# add hmda data 
tract_data <- tract_data %>% 
  left_join(hmda_tract, by = c("GEOID" = "census_tract")) %>% 
  select(move_last(., c("state", "locality", "tract"))) 


# b. Merge county data ----

# add life expectancy by county
county_data <- county_data %>% 
  left_join(lifeexp_county, by = c("GEOID" = "FIPS")) %>% 
  rename(locality = "locality.x") %>% select(-locality.y) %>% 
  select(move_last(., c("state", "locality"))) 

# add segregation measures by county
county_data <- county_data %>% 
  left_join(seg_county, by = c("locality" = "county")) %>% 
  select(move_last(., c("state", "locality")))

# generate HDI measure: function of school enrollment, educ attainment; life expectancy; median personal earnings
# "goalposts" defined in methodology: http://measureofamerica.org/Measure_of_America2013-2014MethodNote.pdf
# earnings goalposts are adjusted for inflation -- set to 2015 values

# without tract-level life expectancy, we can't estimate tract-level AHDI
# may need to omit
tract_data <- tract_data%>% 
  mutate(hlth_index = ( (lifeexpE-66) / (90-66) * 10),
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
  mutate(hlth_index = ( (lifeexpE-66) / (90-66) * 10),
         inc_index = ( (log(earnE)-log(15776.86)) / (log(66748.26)-log(15776.86)) * 10),
         attain_index = ( (((hsmoreE/100 + bamoreE/100 + gradmoreE/100)-0.5)/ (2-0.5)) *10),
         enroll_index = (schlE-60)/(95-60)*10,
         educ_index = attain_index*(2/3) + enroll_index*(1/3),
         hd_index = round((hlth_index + educ_index + inc_index)/3,1))

county_data <- county_data %>% 
  select(-c("hlth_index", "inc_index", "attain_index", "enroll_index", "educ_index")) %>% 
  select(move_last(., c("state", "locality")))


# ....................................................
# 4. Join attributes ----

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
# 5. Add geography  ----
# get tract polygons
geo <- tracts(state = 'VA', county = region, year = 2020) %>% # from tigris
  rename(tr = NAME)

# join coordinates to data
tract_data_geo <- merge(geo, tract_data, by = c("GEOID"), duplicateGeoms = TRUE) # from sp -- keep all obs (full_join)

# get locality polygons
counties_geo <- counties(state = 'VA', year = 2020) # from tigris
counties_geo <- counties_geo %>% subset(COUNTYFP %in% region)

# join coordinates to data
county_data_geo <- merge(counties_geo, county_data, by = "GEOID", duplicateGeoms = TRUE) # from sp -- keep all obs (full_join)
names(county_data_geo)[names(county_data_geo)=="NAME.y"] <- "NAME"

# get block group polygons
blkgrp_geo <- block_groups(state = 'VA', county = region, year = 2020) # from tigris

# join coordinates to data
blkgrp_data_geo <- merge(blkgrp_geo, blkgrp_data, by = "GEOID", duplicateGeoms = TRUE) # from sp -- keep all obs (full_join)


# ....................................................
# 6. Define color palettes ----
nb.cols <- 10
mycolors <- colorRampPalette(brewer.pal(8, "YlGnBu"))(nb.cols)


# ....................................................
# 7. Create attributes (CF Edits) ----

# combine into one data frame
all_data <- bind_rows("County" = county_data_geo, 
                      "Block Group" = blkgrp_data_geo, 
                      "Census Tract" = tract_data_geo, 
                      .id = "GEO_LEVEL")

# fix 3 var names (fixed in googlesheet)
j <- match(pretty2$varname, names(all_data))
j <- j[1:99]

# add pretty labels, sources and about to all_data
for(i in seq_along(j)){
  attr(all_data[[j[i]]], which = "goodname") <- pretty2$goodname[i]
  attr(all_data[[j[i]]], which = "source") <- pretty2$source[i]
  attr(all_data[[j[i]]], which = "about") <- pretty2$about[i]
}

# create data frame of group and varnames
group_df <- pretty2 %>% 
  select(varname, group, goodname) %>% 
  filter(!is.na(group)) 
# categories <- unique(group_df$group)

unique(all_data$GEO_LEVEL)
# [1] "County"       "Block Group"  "Census Tract"

# create indicator lists based on geography
# Block group
ind_bg <- all_data %>% 
  filter(GEO_LEVEL == "Block Group") %>% 
  select(group_df$varname) %>% 
  map_lgl(~ !all(is.na(.x))) 

# census tract
ind_ct <- all_data %>% 
  filter(GEO_LEVEL == "Census Tract") %>% 
  select(group_df$varname) %>% 
  map_lgl(~ !all(is.na(.x))) 

# add indicator logicals to group_df and sort
# column bg - TRUE if variable available for Block Group
# column ct - TRUE if variable available for Census Tract
# all vars available for County
group_df <- cbind(group_df, bg = ind_bg[-length(ind_bg)], 
                  ct = ind_ct[-length(ind_ct)]) %>% 
  arrange(group, goodname)

# different lists of available indicators by geo level
ind_choices_county <- split(group_df, group_df$group) %>% 
  map(function(x)pull(x, varname, name = goodname))

ind_choices_bg <- split(group_df, group_df$group) %>% 
  map(function(x)filter(x, bg)) %>% 
  map(function(x)pull(x, varname, name = goodname))

ind_choices_ct <- split(group_df, group_df$group) %>% 
  map(function(x)filter(x, ct)) %>% 
  map(function(x)pull(x, varname, name = goodname))

counties <- levels(factor(tract_data_geo$county.nice))

# get helpers
source('datacode/helpers.R')

# NOT USED
# # conditional time (year) selections
# # get vars that have all years:
# year_ind <- all_data %>% 
#   select(group_df$varname, year) %>% 
#   split(all_data$year) %>% 
#   map(.f = function(x)map(x, function(y)!all(is.na(y)))) %>% 
#   bind_rows() %>% 
#   select(-year, -geometry) %>% 
#   map_lgl(.f = function(x)all(x)) %>% 
#   `[`(group_df$varname, .)
# 
# # get years
# years <- sort(unique(all_data$year))

# # arrange for plotting by race: 
# race_comp <- all_data %>% 
#   st_drop_geometry() %>% 
#   filter(GEO_LEVEL == "County") %>% 
#   select(GEOID, NAME, year,
#          hhinc_blackE:hhinc_ltnxM, 
#          lifeexp_blackE:lifeexp_asianM) %>% 
#   pivot_longer(hhinc_blackE:lifeexp_asianM, names_to = "var", values_to = "value") %>% 
#   separate(var, into=c("var", "type"), sep=-1) %>% 
#   separate(var, into=c("var", "race"), sep="_") %>% 
#   separate(NAME, into=c("NAME", "state"), sep=", ") %>% 
#   mutate(race = fct_recode(race,
#                            "Black"="black", 
#                            "Asian"="asian", 
#                            "Hispanic"="ltnx", 
#                            "Multi-racial"="multi",
#                            "White"="white")) %>% 
#   filter(type == "E") %>% 
#   unite("var", var, type, sep = "") 
# 
# race_vars <- unique(race_comp$var)


# ....................................................
# 7. Save for app ----
# create new app_data.Rdata file
save(counties_geo, counties, all_data, mycolors, 
     parks_sf, schools_sf, sabselem_sf, mcd_sf, group_df,
     ind_choices_county, ind_choices_bg, ind_choices_ct,
     helpers,
     file = "data20/app_data_2020.Rdata")

save(counties_geo, counties, all_data, mycolors, 
     parks_sf, schools_sf, sabselem_sf, mcd_sf, group_df,
     ind_choices_county, ind_choices_bg, ind_choices_ct,
     helpers, 
     file = "cville-region/www/app_data_2020.Rdata")

