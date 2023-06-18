library(tidyverse)

svi_il <- read_csv("./Data/Illinois.csv")

# Assignment 3/19/23
# 1. Calculate percentile rank for each variable
# 2. Try out on one variable first in wide format
# 3. Find out if variables are controlled for population of tract
# 4. Read over the methods section
# 5. Figure out how the statistics were determined

test <- svi_il%>%
  summarise(LOCATION, 
            pop_pr = percent_rank(E_TOTPOP), 
            hu_pr = percent_rank(E_HU), 
            hh_pr = percent_rank(E_HH),
            hburd_pr = percent_rank(E_HBURD),
            nohsdp_pr = percent_rank(E_NOHSDP),
            uninsur_pr = percent_rank(E_UNINSUR),
            age65_pr = percent_rank(E_AGE65),
            age17_pr = percent_rank(E_AGE17),
            disabl_pr = percent_rank(E_DISABL),
            sngpnt_pr = percent_rank(E_SNGPNT),
            limeng_pr = percent_rank(E_LIMENG),
            minrty_pr = percent_rank(E_MINRTY),
            munit_pr = percent_rank(E_MUNIT),
            mobile_pr = percent_rank(E_MOBILE),
            crowd_pr = percent_rank(E_CROWD),
            noveh_pr = percent_rank(E_NOVEH),
            groupq_pr = percent_rank(E_GROUPQ),
            pov150_pr = percent_rank(E_POV150),
            unemp_pr = percent_rank(E_UNEMP),
            )

test2 <- svi_il%>%
  select(LOCATION, starts_with("E_"))%>%
  pivot_longer(cols = -LOCATION)%>%
  group_by(name, LOCATION)%>%
  summarise(pr = percent_rank(value))

test3 <- svi_il%>%
  mutate(
    across(
      .cols = starts_with("E_"), 
      .fns = percent_rank, 
      .names = "pr_{.col}"))

library(tidycensus)
library(sf)
options(tigris_use_cache = TRUE)

get_acs(geography = "tract", state = "IL", year = 2020, geometry = TRUE)

plot(dc_income["estimate"])

il_income <- get_acs(
  geography = "tract", 
  variables = "B19013_001",
  state = "IL", 
  year = 2020,
  geometry = TRUE)

plot(il_income["estimate"])

ggplot(data = il_income, aes(fill = estimate))+
  geom_sf(colour = "red")

il_census <- left_join(il_income, svi_il, by = c("NAME" = "LOCATION"))

ggplot(il_census%>%
         filter(RPL_THEMES>=0), 
       aes(fill = RPL_THEMES))+
  geom_sf()

#ASSIGNMENT 3.30.23
#1. Find out how to find what census tract addresses are in R
#2. Look at ArcGIS

library(censusxy)

a <- cxy_benchmarks()

cxy_vintages(benchmark = a$benchmarkName[])

df <- cxy_single("3700 Lindell Blvd", 
                     "St. Louis", 
                     "MO", 
                     63139, 
                     return = "geographies",
                     benchmark = "Public_AR_Current",
                     vintage = "Census2020_Current")

vintages <- paste0(c("Current", 
              "Census2010", 
              "ACS2017", 
              "ACS2018", 
              "ACS_2019", 
              "Census_2020", 
              "ACS2022", 
              "ACS2021"), "_Current")

yelp_api_key <- "sUj3A_0f3IEQ-70mRhElc61mVJ9UkRl7jft6_8qRKtpne5DHOqwA3caJz2aJGmboLxfZXmetRQucG4FJp10pco96F0g-lF7o6OB4m8rXDhGVtP4Nwh0TiJPTS308ZHYx"

library(httr)
library(jsonlite)

# set API endpoint
url <- "https://api.yelp.com/v3/businesses/search"

# set API parameters
params <- list(
  location = "Evanston",
  term = "restaurant",
  category = "italian"
)

# reads in API key
api_key <- read.delim(file = , stringsAsFactors = FALSE, header = FALSE)$V1

# set API headers
headers <- c(
  Authorization = paste("Bearer", yelp_api_key)
)

# make API request
response <- GET(url, query = params, add_headers(headers))# parse JSON response
content <- fromJSON(content(response, "text"), flatten = TRUE)

# ASSIGNMENT 4.16.23
#1. USE THE ADDRESSES FROM THE YELP API CALL ABOVE TO FIND THEIR CENSUS TRACT FOR EACH ADDRESS

census_tracts <- cxy_geocode(content[["businesses"]], 
                             street = "location.address1", 
                             city = "location.city", 
                             state = "location.state", 
                             zip = "location.zip_code", 
                             return = "geographies", 
                             benchmark = "Public_AR_Current", 
                             vintage = "Census2020_Current",
                             output = "full")

census_one$cxy_tract_id <- str_replace(census_one$cxy_tract_id, "(\\d{4})", "\\1.")

# pipe the above df into a mutate and turn any string above 4 digits to a 
# number with a period after the 4th digit (i.e., 1234.56)
# anything less than 5 digits leave alone (ASSIGNMENT #1 23 APRIL 2023)

svi_il <- read_csv("./Data/Illinois.csv")

tracts <- svi_il%>%
  mutate(census_tract = gsub("[^0-9.]", "", LOCATION))%>%
  mutate(census_tract_num = as.numeric(gsub("\\.", "", census_tract)))

test3 <- svi_il%>%
  mutate(
    across(
      .cols = starts_with("E_"), 
      .fns = percent_rank, 
      .names = "pr_{.col}"))

df2 <- left_join(census_tracts, tracts, by = c("cxy_tract_id" = "census_tract_num"))

df2%>%
  select(location.address1, cxy_tract_id, RPL_THEMES)

# ASSIGNMENT #2:
#* create a function that takes addresses as an input and outputs
#* census tract information and SVI
#* function_name <- function(x){magic here}

address_geo <- "2531 Lawndale Ave"
city_geo <- "Evanston"
zip_geo <- "60201"
state_geo <- "IL"
country_geo <- "USA"

svi_finder <- function(
    address = address_geo, 
    city = city_geo, 
    zip = zip_geo, 
    state = state_geo, 
    country = country_geo){}

library(tidycensus)
options(tigris_use_cache = TRUE)
library(tigris)
library(tidygeocoder)

# Enter your Census API key here
census_api_key("YOUR_API_KEY")

yelp_ad <- as_tibble(content$businesses)%>%
  select(name, 
         addr = location.address1, 
         city = location.city,
         state = location.state, 
         zip = location.zip_code,
         )%>%
  mutate(geo = paste(addr, city, state, zip, sep = ", "))%>%
  select(name, addr = geo)
  

location <- yelp_ad%>%
  geocode(addr, method = "census")

sample_addresses%>%
  slice(1:2)

# Use the Census Bureau API to get the census tract that contains the location
tract <- get_acs(geography = "tract", 
                 variables = "NAME",
                 lat = location$lat, 
                 lon = location$lon,
                 year = 2019)

# Print the name and census tract code of the tract that contains the location
cat("The address", address, "is located in", tract$NAME[1], "in census tract", tract$GEOID[1], "\n")
