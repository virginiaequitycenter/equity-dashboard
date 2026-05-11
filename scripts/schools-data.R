# Acquire School geometry data
# Last updated: 4/9/26 

# From NCES 
# * https://nces.ed.gov/programs/edge/Geographic/SchoolLocations

# Load libraries ----
library(tidyverse)
library(sf)
library(tigris)

options(timeout = max(1080, getOption("timeout")))

# Create basic objects ----
# These variables are set in dashboard-update-workflow.Rmd
dyr <- dyr
tigeryr <- tigeryr
fips_codes <- fips_codes
filepath_data <- filepath_data

urlpublic <- urlpublic
urlprivate <- urlprivate
urlboundaries <- urlboundaries

# ....................................................
# Download data ----
if (!dir.exists("data/tempdata")){
  dir.create("data/tempdata")}

# Public schools -- 
# download.file(url = urlpublic,
#               destfile = "data/tempdata/public_schools.zip") # public school data file
unzip(zipfile = "data/tempdata/public_schools.zip", exdir = "data/tempdata/public_schools")
unzip(zipfile = "data/tempdata/public_schools/Shapefile_SCH.zip", exdir = "data/tempdata/public_schools/Shapefile_SCH")

pubschlyr <- str_sub(urlpublic, -8,-5)

pubschools_sf = st_read(dsn = paste0("data/tempdata/public_schools/Shapefile_SCH/EDGE_GEOCODE_PUBLICSCH_", pubschlyr, ".shp"))

# Private schools -- 
# download.file(url = urlprivate,
#               destfile = "data/tempdata/private_schools.zip")
unzip(zipfile = "data/tempdata/private_schools.zip", exdir = "data/tempdata/private_schools")

privschlyr <- str_sub(urlprivate, -8,-5)
privschools_sf = st_read(dsn = paste0("data/tempdata/private_schools/EDGE_GEOCODE_PRIVATESCH_", privschlyr, ".shp"))


# School Boundaries ----
# get school attendance boundaries: https://nces.ed.gov/programs/edge/SABS
# Details: https://nces.ed.gov/pubs2015/2015118.pdf
# download.file(url = urlboundaries, 
#               destfile="data/tempdata/school_boundaries.zip", method="libcurl")
# unzip("data/tempdata/school_boundaries.zip", exdir = "data/tempdata/school_boundaries")

sabsyr <- str_sub(urlboundaries, -8,-5)

sabs_sf <- st_read(paste0("data/tempdata/school_boundaries/SABS_", sabsyr, "/SABS_", sabsyr, ".shp"))

# ....................................................
# Reduce data, add county FIPS ----
# public schools
pubschools_sf <- pubschools_sf %>% 
  filter(STATE == "VA") 

# pubschools_sf$STATE <- droplevels(pubschools_sf$STATE)
# pubschools_sf$CNTY <- droplevels(pubschools_sf$CNTY)

pubschools_sf <- pubschools_sf %>% 
  mutate(county = substr(CNTY, 3,5)) %>% 
  filter(county %in% fips_codes) %>% 
  mutate(type = "public") %>% 
  rename(id = NCESSCH,
         coname = NMCNTY) %>% 
  select(id:LON, type, county, geometry, -OPSTFIPS, -LEAID)

# private schools
privschools_sf <- privschools_sf %>% 
  filter(STATE == "VA")

# privschools_sf$STATE <- droplevels(privschools_sf$STATE)
# privschools_sf$CNTY <- droplevels(privschools_sf$CNTY)

privschools_sf <- privschools_sf %>% 
  mutate(county = CNTY) %>% 
  filter(county %in% fips_codes) %>% 
  mutate(type = "private") %>% 
  rename(id = PPIN,
         coname = NAMELSAD) %>% 
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
# Combine data files and transform ----
# combine public and private schools
# Need to get them on the same coordinate system 
pubschools_sf <- st_transform(pubschools_sf, 4326)
privschools_sf <- st_transform(privschools_sf, 4326)
schools_sf <- rbind(pubschools_sf, privschools_sf)

# Transform attendance boundaries to same epsg as school locations
sabs_sf <- st_transform(sabs_sf, 4326)
sabselem_sf <- sabs_sf %>% 
  filter(level == 1)
sabshigh_sf <- sabs_sf %>% 
  filter(level == 3)

# ....................................................
# Save as geojson ----
st_write(schools_sf, dsn = paste0(filepath_data, "schools_sf.geojson"), driver = "GeoJSON", delete_dsn = TRUE) 
st_write(sabselem_sf, dsn = paste0(filepath_data, "sabselem_sf.geojson"), driver = "GeoJSON", delete_dsn = TRUE)
st_write(sabshigh_sf, dsn = paste0(filepath_data, "sabshigh_sf.geojson"), driver = "GeoJSON", delete_dsn = TRUE)

# Remove vars from environment
rm(list = ls(pattern = "^(url|pub|priv|school_|sabsyr)"))

