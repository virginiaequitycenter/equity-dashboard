####################################################
# Greater Charlottesville Region Equity Profile
####################################################
# Acquire Magisterial District geometry data
# Last updated: 07/08/2020
# From Tigris
#
# Geography: Magesterial Districts in Charlottesville region
#     Charlottesville, Albemarle, Greene, Louisa, 
#     Fluvanna, Nelson, Buckingham, Madison, Orange
#     (include Augusta, Waynesboro, Staunton?)
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
library(ggthemes)


ccode <- read_csv("county_codes.csv")
region <- ccode$code # list of desired counties


# ....................................................
# 2. Download data ----

# pull county subdivisions and reduce to region
mcd <- county_subdivisions(state = "VA", county = "003", year = "2020")
# will need to change back to county = region

# test <- voting_districts(state = "VA", county = "003", year = "2020")

mcd_df <- fortify(mcd)

p <- ggplot() + 
  geom_sf(data=mcd_df,
          color="black", fill="white", size=0.25)
p

# LL 11/3 note: As of today, it doesn't appear that the magisterial districts 
# for Albemarle county have changes to redistricting option 2 that was selected
# by the county. However, the documentation from tigris seems to suggest that it 
# should be for the year 2020.... I tried downloading voting districts as well, 
# and those also claim to be 2020. 

# ....................................................
# 3. Check and transform ----

# check
mcd_df <- fortify(mcd)

p <- ggplot() + 
  geom_sf(data=mcd_df, map=mcd_df,
           aes(x=long, y=lat, map_id=id),
           color="black", fill="white", size=0.25)
p + coord_map() + theme_map() # with correct mercator projection

# change to sf format
mcd_sf <- st_as_sf(mcd)
st_crs(mcd_sf)

# transform crs
mcd_sf <- st_transform(mcd_sf, 4326)
plot(mcd_sf[6])


# ....................................................
# 4. Save as geojson ----

st_write(mcd_sf, "data/mcd_sf.geojson", driver = "GeoJSON", delete_dsn = TRUE) 
# mcd_sf <- st_read("data/mcd_sf.geojson") 

