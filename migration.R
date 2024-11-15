
library(tidycensus)
library(dplyr)
library(ggplot2)
library(sf)
library(tigris)

################  MIGRATION   ################ 

### get top growth in cities
### measure by net migration
cities_gpt <- get_acs(geography = "place", 
                      variables = "B01001_001",  # Total population, for example
                      state = "TX", 
                      year = 2020)

### texas county migration
cities <- get_flows(
  geography = "county",
  state = "TX")

### TX = Harris County = Houston 235,809 move in
### Dallas County == 150,536
cities %>% 
  tidyr::separate(FULL1_NAME, sep=",", into=c("county", "state")) %>% 
  group_by(county) %>% 
  filter(variable == "MOVEDIN") %>% 
  reframe(sum = sum(estimate)) %>% 
  arrange(desc(sum))
reframe()

### most growing cities
msa_flow <- get_flows(
  geography = "metropolitan statistical area",
  geometry = T)

### top growing cities
top_cities <- msa_flow %>% 
  mutate(FULL1_NAME = str_remove(FULL1_NAME, " Metro Area")) %>%  
  tidyr::separate(FULL1_NAME, sep = ", ", into = c("city", "state")) %>%  
  group_by(city) %>% 
  filter(variable == "MOVEDNET") %>% 
  reframe(migration = sum(estimate, na.rm=T)) %>% 
  arrange(desc(migration))

### most leaving cities
least_cities <- msa_flow %>% 
  tidyr::separate(FULL1_NAME, sep=",", into=c("city", "county")) %>% 
  group_by(city) %>% 
  filter(variable == "MOVEDNET") %>% 
  reframe(migration = sum(estimate, na.rm=T)) %>% 
  arrange(migration)

### migration flow
tx <- get_flows(
  geography="county",
  state="TX",
  geometry = T) %>% 
  filter(variable == "MOVEDIN") %>%
  na.omit() %>%
  arrange(desc(estimate))

#### travis county TX
travis_inflow <- get_flows(
  geography = "county",
  state = "TX",
  county = "Travis",
  geometry = TRUE
) %>%
  filter(variable == "MOVEDIN") %>%
  na.omit() %>%
  arrange(desc(estimate))

### travis county migration
travis_inflow %>%
  slice_max(estimate, n = 30) %>%
  mutate(weight = estimate / 500) %>%
  mapdeck(token = token) %>%
  add_arc(origin = "centroid2",
          destination = "centroid1",
          stroke_width = "weight",
          update_view = FALSE) 


library(mapdeck)
token="pk.eyJ1IjoiamNvbm5zIiwiYSI6ImNtMjVmbnBqZTBxeDQybG9nc3B2MHltdjAifQ.CAWSeJi0_rG9GJb9yjUIGA"

### top MSA region PHZ, Riverside, Orlando
msa_flow %>% 
  tidyr::separate(FULL1_NAME, sep=",", into=c("county", "state")) %>% 
  group_by(county) %>% 
  filter(variable == "MOVEDNET") %>% 
  reframe(sum = sum(estimate, na.rm=T)) %>% 
  arrange(desc(sum))