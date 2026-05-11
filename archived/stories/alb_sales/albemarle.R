# Albemarle Data: Transfer history (sales) and other parcel information (tract/block group)
# http://www.albemarle.org/department.asp?department=gds&relpage=3914
# For equity dashboard prototype
# Number of properties sold by tract/block group/ES district each year -- 2011-2018
# Average sales price by tract/block group/ES district -- 2011-2018


# 1. Load libraries
library(tidyverse)
library(lubridate)
library(tigris)
library(sf)


# 2. Read transfer and other parcel data downloaded with get_alb_data.R
transfer <- read_tsv("../../cvl_alb_property/albco_data/albco_realestate_website/VISION_sales.txt")
other <- read_tsv("../../cvl_alb_property/albco_data/albco_realestate_website/CityView_View_OtherParcelCharacteristics.txt")
card <- read_tsv("../../cvl_alb_property/albco_data/albco_realestate_website/GIS_CardLevelData_new.txt")


# 3. Reduce and format data
transfer <- transfer %>% 
  mutate(date = mdy(saledate1),
         year = year(date))
table(transfer$year)
transfer <- transfer %>% 
  filter(year > 2010 & year < 2019 & saleprice > 0 & !is.na(saleprice))
# removed if saleprice is missing or zero; may need to limit it to validity codes with "valid"

# Generate census block group id (in format to match spatial data)
other <- other %>% 
  select(ParcelID:ESDistrict, CensusBlockGroup:CensusTract) %>% 
  mutate(blockgroupname = as.character(CensusBlockGroup),
         censustractname = as.character(CensusTract),
         censustractname = if_else(str_detect(censustractname, "\\."), censustractname,
                                   paste0(censustractname, ".")),
         censustractname = str_pad(censustractname, 6, side = "right", pad = "0"),
         censustractname = str_replace(censustractname, "\\.", ""),
         censustractname = str_pad(censustractname, 6, "left", "0"),
         censusblockgroupname = paste0(censustractname, blockgroupname))


# 4. Join by Parcel Number 
sum(transfer$mapblolot %in% other$ParcelID) # 21890/21891 (99%)

sales <- transfer %>% 
  left_join(other, by = c("mapblolot" = "ParcelID"))

sales2 <- sales %>% 
  left_join(card, by = c("mapblolot" = "TMP"))

# Keep only residential property sales
reslist = c("Condo-Res-TH", "Doublewide", "Duplex", "Mobile Home", "Single Family", "Single Family-Rental")
sales2 <- sales2 %>% filter(UseCode %in% reslist & CardNum == 1)


# 5. Generate number of sales, average sales by ES district, tract, block group
alb_sales_school <- sales2 %>% filter(ESDistrict != "Unassigned") %>% 
  group_by(year, ESDistrict) %>% 
  summarize(numsales = n(), mediansales = median(saleprice))

alb_sales_tract <- sales %>% filter(!is.na(CensusTract)) %>% 
  group_by(year, censustractname, CensusTract) %>% 
  summarize(numsales = n(), mediansales = median(saleprice))

alb_sales_blockgroup <- sales %>% filter(blockgroupname != "NANA") %>% 
  group_by(year, censusblockgroupname) %>% 
  summarize(numsales = n(), mediansales = median(saleprice))


# 6. Read in relevant geography - ES district, tract, block group - and join
# census tracts
tracts <- tracts(state = "VA", county = "003")
tracts_sf <- st_as_sf(tracts) # convert to sf object
st_crs(tracts_sf) # check

alb_sales_tract <- alb_sales_tract %>%  # create character tract
  mutate(tractname = as.character(CensusTract))

alb_sales_tract <- tracts_sf %>%  # join tract sf to tract sales
  inner_join(alb_sales_tract, by = c("NAME" = "tractname"))

alb_sales_tract <- alb_sales_tract %>%  # reduce to key columns
  select(TRACTCE:NAMELSAD, year:geometry)


# block groups
blockgroups <- block_groups(state = "VA", county = "003")
blockgroups_sf <- st_as_sf(blockgroups) # convert to sf object
st_crs(blockgroups_sf) # check

blockgroups_sf <- blockgroups_sf %>% 
  mutate(censusblockgroupname = paste0(TRACTCE, BLKGRPCE))

alb_sales_blockgroup <- blockgroups_sf %>%  # join blockgroup sf to blockgroup sales
  inner_join(alb_sales_blockgroup, by = "censusblockgroupname")

alb_sales_blockgroup <- alb_sales_blockgroup %>%  # reduce to key columns
  select(TRACTCE:NAMELSAD, censusblockgroupname:geometry)


# school districts
es_district <- st_read("data/sabselem_sf.geojson") 
es_district <- es_district %>% filter(county == "003")

plot(es_district[1])

# match school names
albschools <- unique(alb_sales_school$ESDistrict)

es_district <- es_district %>% 
  mutate(ESDistrict = SrcName,
         ESDistrict = str_to_title(ESDistrict))

escshools <- unique(es_district$ESDistrict)

# join districts to district sales
alb_sales_school <- es_district %>% # join blockgroup sf to blockgroup sales
  left_join(alb_sales_school, by = "ESDistrict")

alb_sales_school <- alb_sales_school %>%  # reduce to key columns
  select(SrcName:ncessch, ESDistrict:geometry)
plot(alb_sales_school[1])

# 7. Change everything to CRS 4326 and save
alb_sales_school <- st_transform(alb_sales_school, 4326)
alb_sales_tract <- st_transform(alb_sales_tract, 4326)
alb_sales_blockgroup <- st_transform(alb_sales_blockgroup, 4326)

# save as geojson
st_write(alb_sales_school, "data/alb_sales_school.geojson", driver = "GeoJSON", delete_dsn = TRUE) 
st_write(alb_sales_tract, "data/alb_sales_tract.geojson", driver = "GeoJSON", delete_dsn = TRUE) 
st_write(alb_sales_blockgroup, "data/alb_sales_blockgroup.geojson", driver = "GeoJSON", delete_dsn = TRUE) 

