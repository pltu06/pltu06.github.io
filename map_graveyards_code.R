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