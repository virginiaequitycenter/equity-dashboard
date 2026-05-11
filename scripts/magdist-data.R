# Acquire Magisterial District geometry data
# Last updated: 4/9/2026

# 1. Load libraries ----
library(tidyverse)
library(sf)
library(tigris)
options(tigris_use_cache = TRUE)

# Create basic objects ----
# These variables are set in dashboard-update-workflow.Rmd
dyr <- dyr
tigeryr <- tigeryr
fips_codes <- fips_codes
filepath_data <- filepath_data

# Download data ----
# pull county subdivisions and reduce to region
mcd <- county_subdivisions(state = "VA", 
                           county = fips_codes, 
                           year = tigeryr)

# ....................................................
# Check and transform ----

# check
mcd_df <- fortify(mcd)

# p <- ggplot() + 
#   geom_sf(data=mcd_df,
#           color="black", fill="white", size=0.25)
# p


# change to sf format
mcd_sf <- st_as_sf(mcd)
st_crs(mcd_sf)

# transform crs
mcd_sf <- st_transform(mcd_sf, 4326)

# Check
# p <- ggplot() + 
#   geom_sf(data=mcd_sf, color="black", fill="white", size=0.25)
# 
# p

# ....................................................
# Save as geojson ----
st_write(mcd_sf, dsn = paste0(filepath_data, "mcd_sf.geojson"), driver = "GeoJSON", delete_dsn = TRUE) 

# Remove vars from environment
rm(list = setdiff(ls(pattern = "^mcd"), "mcd_sf"))

