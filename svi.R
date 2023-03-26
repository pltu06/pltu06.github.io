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

get_acs(geography = "tract", state = "IL", year = 2020,geometry = TRUE)

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
#1. Find out how to find what census tract adresses are in R
#2. Look at ArcGIS