####################################################
# Greater Charlottesville Region Equity Profile
####################################################
# Acquire Park geometry data
# Last updated: 09/29/2022
# Data from Open Street Map using the osm package
## Open street map amenity key: https://wiki.openstreetmap.org/wiki/Key:amenity
####################################################
# 1. Load libraries
# 2. Download data
# 3. Reduce data, add county FIPS
# 4. Combine data file and save as geojson
####################################################

# ....................................................
# 1. Load libraries ----
library(tidyverse)
library(osmdata)
library(sf)
library(leaflet)

# ....................................................
# 2. Download data ----
# define bounding box via county shape files
cville_bounds <- readRDS("../data/cville_tracts.RDS")
cville_bbox <- st_bbox(cville_bounds)

# align crs
cville_bounds <- sf::st_transform(cville_bounds, 4326)

### 2. Parks ----------------------------------------------------------------------------------------
# Cville parks map
region_parks <- opq(cville_bbox) %>% 
  add_osm_feature(key = "leisure", value = "park") %>% 
  osmdata_sf()

parks1 <- region_parks$osm_points[!is.na(region_parks$osm_points$name),] 
parks2 <- region_parks$osm_polygons[!is.na(region_parks$osm_polygons$name),] 
parks3 <- region_parks$osm_multipolygons[!is.na(region_parks$osm_multipolygons$name),] 

# remove entities outside of county bounds
parks1_bounds <- st_intersection(parks1, cville_bounds)
parks2_bounds <- st_intersection(parks2, cville_bounds)
parks3_bounds <- st_intersection(st_make_valid(parks3), cville_bounds)

# Need to combine the three data frames 
## WARNING: The exact column names and numbers of columns appear to change over time for each
# of the different data frames created above with st_intersection. This means that the next section that
# selects different columns so that the three data frames can be bound together with rbind will likely
# need to change when this chunk of code is run in the future. 
# for now, I filter based on the column names of the data frame with the 
# fewest columns 
names <- colnames(parks3_bounds)

parks1_bounds <- parks1_bounds[,colnames(parks1_bounds) %in% names == T]

parks2_bounds <- parks2_bounds[,colnames(parks2_bounds) %in% names == T]

parks <- rbind(parks1_bounds, parks2_bounds, parks3_bounds)

# view
leaflet(cville_bounds) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(weight = 1, fill = 0, color = "black") %>% 
  addCircles(data = st_collection_extract(parks, "POINT"), color = "blue",
             popup = st_collection_extract(parks, "POINT")$name) %>% 
  addPolygons(data = st_collection_extract(parks, "POLYGON"), color = "orange",
              popup = st_collection_extract(parks, "POLYGON")$name)


# ....................................................
# 3. Reduce data, add county FIPS ----

# In spot checking the data frame, there are many that have historic district in the name, and 
# might not be worth including
# there are also several points within larger polygons that appear to be redundant. 

# View(parks[str_detect(parks$name, pattern = "Historic"),])
# As of 09/29/2022, these "parks" include
# Albemarle Historic District
# Charlottesville and Albemarle County Courthouse Historic District
# Rugby Road-University Corner Historic District
# University of Virginia Historic District
# Scottsville Historic District
# Wertland Street Historic District
# Ridge Street Historic District
# Fluvanna County Courthouse Historic District

## Forming a subset of just the historic districts to map them 
historic <- parks[str_detect(parks$name, pattern = "Historic"),]

leaflet(cville_bounds) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(weight = 1, fill = 0, color = "black") %>% 
  addCircles(data = st_collection_extract(historic, "POINT"), color = "blue",
             popup = st_collection_extract(historic, "POINT")$name) %>% 
  addPolygons(data = st_collection_extract(historic, "POLYGON"), color = "orange",
              popup = st_collection_extract(historic, "POLYGON")$name)

## None of these appear to capture any green space, except for "University of Virginia Historic District"
# which really just includes the lawn. Other data sources we considered for the parks data 
# also included the lawn; It could potentially be worth including in the future because certainly other people 
# outside the UVA community access the lawn, but those often seem to be tourists. 
# but for now, I'm excluding it along with the other historic districts. 

parks <- parks %>%
  filter(parks$name %in% historic$name == F)

## Spot checking for redundancies and point/polygon inconsistencies:
# Polygon for Fry Spring's Park and point for Fry's Spring Beach Club (only keeping polygon)
# Polygon for Northeast Park and a point for "Northeast Park Southbound" (only keeping polygon)
# 
# Point for Pen Park, but no polygon (Cville/Albemarle)
# Point for Brook Hill River Park, but no polygon (Cville/Albemarle)
# Point for Carysbrook Sports Complex, but no polygon (Buckingham)
# Point for Carys Creek Wayside, but no polygon (Buckingham)

# Filtering out the redundant points mentioned above and 
# getting rid of the osm id column because it's unnecessary
parks <- parks %>%
  filter(parks$name != "Fry's Spring Beach Club" & parks$name != "Northeast Park Southbound") %>%
  select(-osm_id)

# ....................................................
# 4. Save date as geojson  
# Save as geojson
st_write(parks, "../data/parks_OSM_sf.geojson", driver = "GeoJSON", delete_dsn = TRUE) 

st_crs(parks)




