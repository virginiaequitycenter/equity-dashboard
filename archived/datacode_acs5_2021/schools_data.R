####################################################
# Greater Charlottesville Regional Equity Atlas
####################################################
# Acquire School geometry data
# Last updated: 01/10/23 
# From NCES 
# * https://nces.ed.gov/programs/edge/Geographic/SchoolLocations
#
# Geography: Schools in Charlottesville region
#     Charlottesville, Albemarle, Greene 
#     Louisa, Fluvanna, Nelson,
####################################################
# 1. Load libraries
# 2. Download data
# 3. Reduce data, add county FIPS
# 4. Combine data file and save as geojson

# 4. Add geography
# 5. Summarize/Examine
# 6. Save
####################################################

# ....................................................
# 1. Load libraries ----
library(tidyverse)
library(sf)
library(tigris)

ccode <- read_csv("data/county_codes.csv")
ccode <- ccode[1:6,]
region <- ccode$code # list of desired counties

options(timeout = max(1080, getOption("timeout")))

# ....................................................
# 2. Download data ----
if (!dir.exists("data/tempdata")){
  dir.create("data/tempdata")}

# public schools -- updated public schools as of 01/23 are from 2021-2022 school year
download.file(url = "https://nces.ed.gov/programs/edge/data/EDGE_GEOCODE_PUBLICSCH_2122.zip", 
              destfile = "data/tempdata/public_schools.zip") # public school data file 
unzip(zipfile = "data/tempdata/public_schools.zip", exdir = "data/tempdata/public_schools")

pubschools_sf = st_read(dsn = "data/tempdata/public_schools/EDGE_GEOCODE_PUBLICSCH_2122/Shapefiles_SCH/EDGE_GEOCODE_PUBLICSCH_2122.shp")
# Simple feature collection with 102130 features and 25 fields
# Geometry type: POINT
# Dimension:     XY
# Bounding box:  xmin: -176.6403 ymin: -14.34892 xmax: 145.7844 ymax: 71.30034
# Geodetic CRS:  NAD83

# private schools -- updated private schools as of 07/20 are from 2019-2020 school year (no 2021/22 data available)
download.file(url = "https://nces.ed.gov/programs/edge/data/EDGE_GEOCODE_PRIVATESCH_1920.zip",
              destfile = "data/tempdata/private_schools.zip")
unzip(zipfile = "data/tempdata/private_schools.zip", exdir = "data/tempdata/private_schools")

privschools_sf = st_read(dsn = "data/tempdata/private_schools/EDGE_GEOCODE_PRIVATESCH_1920.shp")
# Simple feature collection with 21572 features and 23 fields
# Geometry type: POINT
# Dimension:     XY
# Bounding box:  xmin: -159.713 ymin: 19.49081 xmax: -67.83857 ymax: 64.87414
# Geodetic CRS:  WGS 84


# As of 07/20/22, the school boundaries are still from 2015-2016
# get school attendance boundaries: https://nces.ed.gov/programs/edge/SABS
# Details: https://nces.ed.gov/pubs2015/2015118.pdf
url <- "https://nces.ed.gov/programs/edge/data/SABS_1516.zip"
download.file(url, destfile="data/tempdata/SABS_1516.zip", method="libcurl")
unzip("data/tempdata/SABS_1516.zip", exdir = "data/tempdata/sabs_1516")

sabs_sf <- st_read("data/tempdata/sabs_1516/SABS_1516/SABS_1516.shp")
# Simple feature collection with 75128 features and 13 fields
# Geometry type: MULTIPOLYGON
# Dimension:     XY
# Bounding box:  xmin: -19951910 ymin: 2139103 xmax: 20021890 ymax: 11554790
# Projected CRS: WGS 84 / Pseudo-Mercator

# ....................................................
# 3. Reduce data, add county FIPS ----
# public schools
pubschools_sf <- pubschools_sf %>% 
  filter(STATE == "VA") 

# pubschools_sf$STATE <- droplevels(pubschools_sf$STATE)
# pubschools_sf$CNTY <- droplevels(pubschools_sf$CNTY)

pubschools_sf <- pubschools_sf %>% 
  mutate(county = substr(CNTY, 3,5)) %>% 
  filter(county %in% region) %>% 
  mutate(type = "public") %>% 
  rename(id = NCESSCH) %>% 
  select(id:LON, type, county, geometry, -OPSTFIPS, -LEAID)

# private schools
privschools_sf <- privschools_sf %>% 
  filter(STATE == "VA")

# privschools_sf$STATE <- droplevels(privschools_sf$STATE)
# privschools_sf$CNTY <- droplevels(privschools_sf$CNTY)

privschools_sf <- privschools_sf %>% 
  mutate(county = substr(CNTY, 3,5)) %>% 
  filter(county %in% region) %>% 
  mutate(type = "private") %>% 
  rename(id = PPIN) %>% 
  select(id:LON, type, county, geometry)


plot(pubschools_sf[,1])
plot(privschools_sf[,1])

# public school attendance boundaries
sabs_sf <- sabs_sf %>% 
  filter(stAbbrev == "VA") %>% # just VA
  filter(ncessch %in% pubschools_sf$id) %>%  # just region
  mutate(id = ncessch)

# sabs_sf <- droplevels(sabs_sf) # doesn't work on multipolygon

# add fips code to sabs
school_fips <- pubschools_sf %>% select(id, CNTY, county) %>% st_drop_geometry()
sabs_sf <- geo_join(sabs_sf, school_fips,  by = "id")


# ....................................................
# 4. Combine data files and transform ----
# combine public and private schools
# Need to get them on the same coordinate system 
pubschools_sf <- st_transform(pubschools_sf, 4326)
privschools_sf <- st_transform(privschools_sf, 4326)
schools_sf <- rbind(pubschools_sf, privschools_sf)

# For parks I had coordinate system 4326; these seem to have 4269; do I need to change them?
schools_sf <- st_transform(schools_sf, 4326)

# Transform attendance boundaries to same epsg as school locations
sabs_sf <- st_transform(sabs_sf, 4326)
sabselem_sf <- sabs_sf %>% 
  filter(level == 1)
sabshigh_sf <- sabs_sf %>% 
  filter(level == 3)


# ....................................................
# 4. Save as geojson ----
st_write(schools_sf, "data/schools_sf.geojson", driver = "GeoJSON", delete_dsn = TRUE) 
# st_crs(schools_sf)
# schools_sf <- st_read("data/schools_sf.geojson") 
st_write(sabselem_sf, "data/sabselem_sf.geojson", driver = "GeoJSON", delete_dsn = TRUE)
st_write(sabshigh_sf, "data/sabshigh_sf.geojson", driver = "GeoJSON", delete_dsn = TRUE)


