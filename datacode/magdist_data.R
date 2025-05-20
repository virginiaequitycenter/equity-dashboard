####################################################
# Greater Charlottesville Regional Equity Atlas
####################################################
# Acquire Magisterial District geometry data
# Last updated: 5/20/2025
#  - updated to 2024 TIGER/Line shapefiles
#  - https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.2024.html#list-tab-790442341
# From Tigris
#
# Geography: Magesterial Districts in Charlottesville region
#     Charlottesville, Albemarle, Greene 
#     Louisa, Fluvanna, Nelson,
####################################################
# 1. Load libraries
# 2. Download data
# 3. Check and transform
# 4. Save as geojson
####################################################

# ....................................................
# 1. Load libraries ----
library(tidyverse)
library(sf)
library(tigris)


ccode <- read_csv("datacode/county_codes.csv")
ccode <- ccode[1:6,]
region <- ccode$code # list of desired counties


# ....................................................
# 2. Download data ----

# pull county subdivisions and reduce to region
mcd <- county_subdivisions(state = "VA", 
                           county = region, 
                           year = "2024")

# ....................................................
# 3. Check and transform ----

# check
mcd_df <- fortify(mcd)

p <- ggplot() + 
  geom_sf(data=mcd_df,
          color="black", fill="white", size=0.25)
p

# change to sf format
mcd_sf <- st_as_sf(mcd)
st_crs(mcd_sf)

# transform crs
mcd_sf <- st_transform(mcd_sf, 4326)

p <- ggplot() + 
  geom_sf(data=mcd_df,
          color="black", fill="white", size=0.25)
p 

# ....................................................
# 4. Save as geojson ----

st_write(mcd_sf, "data/mcd_sf.geojson", driver = "GeoJSON", delete_dsn = TRUE) 
# mcd_sf <- st_read("data/mcd_sf.geojson") 

