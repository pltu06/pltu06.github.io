library(tidyverse)
library(tidygeocoder)
library(censusxy)

svi_il <- read_csv("./Data/Illinois.csv")

svi_geo <- 
  svi_il %>%
  select(LOCATION, svi = RPL_THEMES) %>%
  separate(
    LOCATION, 
    into = c("census_tract", "county", "state"), 
    sep = ",", 
    remove = FALSE
    ) %>%
  mutate(tract_num = as.factor(gsub("[^0-9.]", "", LOCATION)), 
         county_name = sub("\\s+", "", county))

x <- tibble(address = c("2531 Lawndale Ave", "3717 Columbus Rd", "103 N Stott St"), 
            city = c("Evanston", "Quincy", "Genoa"),
            state = c("IL", "IL", "IL"),
            zip = c("60201", "62305", "60135"))

svi_function <- function(data = x, svi_data = svi_geo){
  
  coords <- data %>%
    geocode(street = address, 
            city = city, 
            state = state, 
            postalcode = zip, 
            method = "osm")%>%
    mutate(index = 1:nrow(.))
  
  census_tracts <- coords %>%
    split(.$index) %>%
    map_dfr(~cxy_geography(lon = .x$long, lat = .x$lat))%>%
    mutate(tract_nums = as.factor(Census.Tracts.BASENAME))
  
  svi_tract <- left_join(census_tracts, svi_data, 
                         by = c("Counties.NAME" = "county_name", 
                                "tract_nums" = "tract_num"))
  
  data$svi <- svi_tract$svi
  data$lon <- coords$long
  data$lat <- coords$lat
  
  return(data)
  
}

svi_function()
