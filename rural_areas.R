
library(tidycensus)
library(dplyr)
library(ggplot2)
library(sf)
library(tigris)

########### section two: rural areas ############

### join housing units into counties master
counties <- rural_housing %>% 
  select(NAME, units) %>% 
  left_join(counties, by="NAME")

### racial pop 2020
race <- get_decennial(
  geography="county",
  variables = c(total = "P1_001N",
                white = "P1_003N",
                black = "P1_004N"),
  year = 2020
)

### pivot wider race df
race <- race %>% 
  select(-GEOID) %>% 
  tidyr::pivot_wider(names_from = variable, values_from = value) %>% 
  rename(total_pop = total)

### join race to counties
counties <- counties %>% 
  select(-pop) %>% 
  left_join(race, by="NAME")

### housing units
units_dec <- get_decennial(
  geography="county",
  variables = c(total_units = "H1_001N",
                occupied = "H1_002N",
                vacant = "H1_003N"),  
  year = 2020
)


units_dec <- units_dec %>% 
  rename(units = value)

### rural by headcount
ppl_dec <- get_decennial(
  geography="county",
  variable = "P1_001N",  
  year = 2020
)

ppl_dec <- ppl_dec %>% 
  rename(pop = value)

### join pop and units rural status
rural_total <- ppl_dec %>% 
  left_join(units_dec, by="NAME")

### create rural counties
rural_counties <- rural_total %>%
  filter(pop <= 5000 | units <= 2000) 

### total rural count 34M
total_rural_pull <- rural_counties %>%
  reframe(total_rural_population = sum(pop, na.rm = TRUE)) %>% 
  pull(total_rural_population)

### 10% of Americans live in rural areas defined as under 5k ppl or 2k units
### these areas are county level
(total_rural_pull / total_pop_2020) *100

### states with the most rural counties
rural_states <- rural_housing %>% 
  tidyr::separate(NAME, sep=",", into=c("county", "state")) %>% 
  group_by(state) %>% 
  reframe(counties = n()) %>% 
  arrange(desc(counties))

### states with the most rural counties
rural_states <- rural_housing %>% 
  tidyr::separate(NAME, sep=",", into=c("county", "state")) %>% 
  group_by(state) %>% 
  reframe(counties = n()) %>% 
  arrange(desc(counties))

### group by state
rural_states <- rural_counties %>% 
  mutate(STATE = str_extract(NAME, "\\b[A-Za-z]+$")) %>% 
  group_by(STATE) %>% 
  reframe(count = n()) %>% 
  
  ### tx dakota kansas nebraska VA GA have the most rural counties
  rural_counties %>% 
  mutate(STATE = str_extract(NAME, "\\b[A-Za-z]+$")) %>% 
  group_by(STATE) %>% 
  reframe(count = n()) %>% 
  arrange(desc(count)) %>% 
  head(n=15) %>% 
  ggplot() +
  geom_col(aes(reorder(as.factor(STATE), count), count, fill=as.factor(STATE))) +
  coord_flip() +
  theme_minimal()


states_map <- map_data("state")
rural_states_geo <- rural_states %>% 
  mutate(region = tolower(STATE)) %>% 
  left_join(states_map, by="region")

### plot rural states/why not counties ??
ggplot(rural_states_geo, aes(x = long, y = lat, group = group, fill = count)) +
  geom_polygon(color = "white") +
  ggthemes::theme_map()

### how many counties
rural_total %>% 
  n_distinct(NAME)

