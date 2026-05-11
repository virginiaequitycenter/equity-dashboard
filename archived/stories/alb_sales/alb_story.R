# Albemarle Story Example
# Home sales?


library(tidyverse)
library(RColorBrewer)
library(sf)
library(plotly)
library(leaflet)
# library(sp)
# library(geosphere)


# Albemarle story data
alb_sales_school <- st_read("data/alb_sales_school.geojson")
alb_sales_tract <- st_read("data/alb_sales_tract.geojson")
alb_sales_blockgroup <- st_read("data/alb_sales_blockgroup.geojson")

# School attendance zone
alb_sales_school <- alb_sales_school %>% filter(ESDistrict != "Yancey")
# Summaries
# Number by year
numplot <- ggplot(alb_sales_school, aes(x = year, y = numsales, color = ESDistrict)) + 
  geom_line() + 
  #scale_color_brewer(palette = "Set3") +
  labs(title = "Number of Residential Properties Transferred",
       subtitle = "By School Attendance Zones and Year",
       x = "Year", y = "Number of Properties", color = "School Zone")
ggplotly(numplot)

# Median by year
medplot <- ggplot(filter(alb_sales_school, year > 2011), aes(x = year, y = mediansales, color = ESDistrict)) + 
  geom_line() + 
#  scale_color_brewer(palette = "Set3") +
  labs(title = "Median Price of ResidentialProperties Transferred", 
       subtitle = "By School Attendance Zones and Year",
       x = "Year", y = "Median Price of Properties", color = "School Zone")
ggplotly(medplot)

# Maps
plot(alb_sales_school[1])

alb_sales_school_sum <- alb_sales_school %>% 
  group_by(ESDistrict) %>% 
  summarize(numsales = sum(numsales), mediansales = round(mean(mediansales), 0))

plot(alb_sales_school_sum[1])

nb.cols <- 10
mycolors <- colorRampPalette(brewer.pal(8, "PRGn"))(nb.cols)
pal <- colorNumeric(palette = mycolors,
                    domain = alb_sales_school_sum$numsales)
# Number by zone
leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>% 
  # addTiles() %>% # to show streets more prominently
  addPolygons(data = alb_sales_school_sum,
              fillColor = ~pal(numsales),
              fillOpacity = 0.5,
              color = "white",
              weight = 2,
              smoothFactor = 0.2,
              popup = paste("School:", alb_sales_school_sum$ESDistrict,  "<br>",
                            "Sales:", alb_sales_school_sum$numsales, "<br>",
                            "Price:", alb_sales_school_sum$mediansales),
              highlight = highlightOptions(
                weight = 5,
                fillOpacity = 0.7,
                bringToFront = TRUE)) %>%
  addLegend(pal = pal, 
            values = alb_sales_school_sum$numsales,
            position = "topright", 
            opacity = 0.25,
            title = "Number of Sales") 

# Median by zone
nb.cols <- 10
mycolors <- colorRampPalette(brewer.pal(10, "PRGn"))(nb.cols)
pal <- colorNumeric(palette = mycolors,
                    domain = alb_sales_school_sum$mediansales)
# Number by zone
leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>% 
  # addTiles() %>% # to show streets more prominently
  addPolygons(data = alb_sales_school_sum,
              fillColor = ~pal(mediansales),
              fillOpacity = 0.5,
              color = "white",
              weight = 2,
              smoothFactor = 0.2,
              popup = paste("School:", alb_sales_school_sum$ESDistrict,  "<br>",
                            "Sales:", alb_sales_school_sum$numsales, "<br>",
                            "Price:", alb_sales_school_sum$mediansales),
              highlight = highlightOptions(
                weight = 5,
                fillOpacity = 0.7,
                bringToFront = TRUE)) %>%
  addLegend(pal = pal, 
            values = alb_sales_school_sum$mediansales,
            position = "topright", 
            opacity = 0.25,
            title = "Median Sale Price") 
