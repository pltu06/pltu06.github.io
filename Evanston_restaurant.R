library(ggmap)
library(tidyverse)
library(osmdata)
library(sf)
library(plotly)


# Data sets for project
restaurants <- 
  read_csv(file = "Data/Evanston_restaurants.csv") %>%
  select(
    business_license = `Business License`, 
    business_name = `Business Name`,
    address = Address,
    city = City,
    state = State,
    zip = `Zip Code`,
    location = Location,
    last_inspection = `Last Inspection Date`
    ) %>%
  mutate(last_inspection = as.Date(last_inspection, format = "%m/%d/%Y")) %>%
  separate(location, into = c("location", "lat"), sep = "\\(") %>%
  separate(lat, into = c("lat", "long"), sep = ",") %>%
  mutate(long = gsub("\\)", "", long), 
         long = as.numeric(long), 
         lat = as.numeric(lat))

violations <- 
  read_csv(file = "Data/Evanston_restaurant_violations.csv") %>%
  select(
    business_license = `Business LIcense`,
    violation_date = `Violation Date`,
    violation = Violation,
    comments = `Inspector Comments`
    ) %>%
  mutate(violation_date = as.Date(violation_date, format = "%m/%d/%Y"))

violation_data <- left_join(violations, restaurants, by = "business_license")

#second option (recommended)
coords <- matrix(c(-0.1,-0.07,51.5,51.52), byrow = TRUE, nrow = 2, ncol = 2, dimnames = list(c('x','y'),c('min','max'))) 
location <- coords %>% opq()

water <- location %>%
  add_osm_feature(key = "natural", value = c("water")) %>%
  osmdata_sf()

ggplot() + 
  geom_sf(data = water$osm_multipolygons, fill = 'light blue') + 
  theme_minimal()

#build different types of streets
main_st <- 
  data.frame(
    type = c(
      "motorway",
      "trunk",
      "primary",
      "motorway_junction",
      "trunk_link",
      "primary_link",
      "motorway_link"
      )
    )
st <- data.frame(type = available_tags('highway'))
st <- subset(st, !type %in% main_st$type)
path <- data.frame(type = c("footway","path","steps","cycleway"))
st <- subset(st, !type %in% path$type)
st <- as.character(st$type)
main_st <- as.character(main_st$type)
path <- as.character(path$type)

#query OSM
main_streets <- 
  location %>%
  add_osm_feature(key = "highway", value = main_st) %>%
  osmdata_sf()

streets <- 
  location %>%
  add_osm_feature(key = "highway", value = st) %>%
  osmdata_sf()

water <- 
  location %>%
  add_osm_feature(key = "natural", value = c("water")) %>%
  osmdata_sf()

rail <- 
  location %>%
  add_osm_feature(key = "railway", value = c("rail")) %>%
  osmdata_sf()

parks <- 
  location %>%
  add_osm_feature(
    key = "leisure", 
    value = c(
      "park",
      "nature_reserve",
      "recreation_ground",
      "golf_course",
      "pitch",
      "garden"
      )
    ) %>%
  osmdata_sf()

buildings <- 
  location %>%
  add_osm_feature(key = "amenity", value = "pub") %>%
  osmdata_sf()

#plot map
ggplot() + 
  geom_sf(data = water$osm_multipolygons, fill = 'light blue') + 
  theme_minimal()

ggplot() + 
  geom_sf(data = main_streets$osm_lines, color = '#ff9999', size = 2) + 
  geom_sf(data = streets$osm_lines, size = 1.5, color = '#eedede') +
  geom_sf(data = water$osm_polygons, fill = '#c6e1e3') +
  geom_sf(data = water$osm_multipolygons, fill = '#c6e1e3') +
  geom_sf(data = rail$osm_lines, color = '#596060', size = 1) +
  geom_sf(data = parks$osm_polygons, fill = '#94ba8e') +
  geom_sf(data = buildings$osm_points, color = '#40493f', fill = '#40493f', size = 2) +
  coord_sf(
    xlim = c(coords[1], coords[1,2]), 
    ylim = c(coords[2], coords[2,2]), 
    expand = TRUE
    ) + 
  theme_minimal()

# Evanston
coords <- matrix(c(-87.7324, -87.6623, 42.0189, 42.0714), 
                 byrow = TRUE, nrow = 2, 
                 ncol = 2, 
                 dimnames = list(c('x','y'),
                                 c('min','max'))) 
location <- coords %>% opq()

#ggplot() + 
 # geom_sf(data = water$osm_multipolygons, fill = 'light blue') + 
  #theme_minimal()

#build different types of streets
main_st <- 
  data.frame(
    type = c(
      "motorway",
      "trunk",
      "primary",
      "motorway_junction",
      "trunk_link",
      "primary_link",
      "motorway_link"
    )
  )
st <- data.frame(type = available_tags('highway'))
st <- subset(st, !type %in% main_st$type)
path <- data.frame(type = c("footway","path","steps","cycleway"))
st <- subset(st, !type %in% path$type)
st <- as.character(st$type)
main_st <- as.character(main_st$type)
path <- as.character(path$type)

#query OSM
main_streets <- 
  location %>%
  add_osm_feature(key = "highway", value = main_st) %>%
  osmdata_sf()

streets <- 
  location %>%
  add_osm_feature(key = "highway", value = st) %>%
  osmdata_sf()

water <- 
  location %>%
  add_osm_feature(key = "natural", value = c("water")) %>%
  osmdata_sf()

rail <- 
  location %>%
  add_osm_feature(key = "railway", value = c("rail")) %>%
  osmdata_sf()

parks <- 
  location %>%
  add_osm_feature(
    key = "leisure", 
    value = c(
      "park",
      "nature_reserve",
      "recreation_ground",
      "golf_course",
      "pitch",
      "garden"
    )
  ) %>%
  osmdata_sf()

buildings <- 
  location %>%
  add_osm_feature(key = "amenity", value = "pub") %>%
  osmdata_sf()

#plot map
ggplot() + 
  geom_sf(data = water$osm_multipolygons, fill = 'light blue') + 
  theme_minimal()

evanston_map <- ggplot() + 
  geom_sf(data = parks$osm_polygons, fill = '#94ba8e') +
  geom_sf(data = water$osm_polygons, fill = '#c6e1e3') +
  geom_sf(data = water$osm_multipolygons, fill = '#c6e1e3') +
  geom_sf(data = streets$osm_lines, size = 0.75, color = '#eedede') +
  geom_sf(data = main_streets$osm_lines, color = '#ff9999', size = 0.5) + 
  geom_sf(data = rail$osm_lines, color = '#596060', size = 1) +
  #geom_sf(data = buildings$osm_points, color = '#40493f', fill = '#40493f', size = 2) +
  geom_point(data = restaurants, size = 0.5, aes(x = long, y = lat))+
  coord_sf(
    xlim = c(coords[1], coords[1,2]), 
    ylim = c(coords[2], coords[2,2]), 
    expand = TRUE
  ) + 
  theme_minimal()
evanston_map

freq_vio <- violation_data %>%
  select(violation, comments, business_name, lat, long) %>%
  mutate(
    violation_num = regmatches(
      violation, 
      gregexpr("[[:digit:]]{1,2}", violation)
    ),
    violation_num = as.numeric(violation_num),
    crit_vio = grepl("CRITICAL VIOLATION", comments)
  ) 

freq_vio_sum <- freq_vio %>%
  count(violation_num)
  
ggplot(data = freq_vio_sum, 
         aes(x = reorder(violation_num,n),  y = n))+
  geom_bar(stat = "identity")+
  #scale_x_continuous(breaks = seq(1,45,1), minor_breaks = NULL)+
  labs(
    title = "Bar Graph of City of Evanston Food Violations", 
    x = "Violation Code", 
    y = "Violations"
  )+
  coord_flip()

freq_res_vio <- freq_vio %>%
  count(business_name, crit_vio)

# Assignment
# 1. Map all restaurants with more than two critical violations
# 2. Try to get plotly to work https://plotly.com/ggplot2/ 
# 3. Look at a package that make tables in html 
#https://rfortherestofus.com/2019/11/how-to-make-beautiful-tables-in-r/
#4. Print off regular expressions cheatsheet

crit_res_vio <- left_join(freq_res_vio, freq_vio, by = "business_name")

three_vio_res <- crit_res_vio %>%
  select(crit_vio.x, lat, long, business_name, n) %>%
  filter(crit_vio.x == TRUE, n > 2)

ggplot() + 
  geom_sf(data = water$osm_multipolygons, fill = 'light blue') + 
  theme_minimal()

evanston_vio_map <- ggplot() + 
  geom_sf(data = parks$osm_polygons, fill = '#94ba8e') +
  geom_sf(data = water$osm_polygons, fill = '#c6e1e3') +
  geom_sf(data = water$osm_multipolygons, fill = '#c6e1e3') +
  geom_sf(data = streets$osm_lines, size = 0.75, color = '#eedede') +
  geom_sf(data = main_streets$osm_lines, color = '#ff9999', size = 0.5) + 
  geom_sf(data = rail$osm_lines, color = '#596060', size = 1) +
  #geom_sf(data = buildings$osm_points, color = '#40493f', fill = '#40493f', size = 2) +
  geom_point(data = three_vio_res, size = 0.5, aes(x = long, y = lat))+
  coord_sf(
    xlim = c(coords[1], coords[1,2]), 
    ylim = c(coords[2], coords[2,2]), 
    expand = TRUE
  ) + 
  theme_minimal()
evanston_vio_map

