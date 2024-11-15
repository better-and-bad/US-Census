
library(tidycensus)
library(dplyr)
library(ggplot2)
library(sf)
library(tigris)

### load variables from 2020 Census
vars_2020 <- load_variables(2020, "pl", cache = TRUE)
vars_2010 <- load_variables(2010, "pl", cache = TRUE)

### 2020 total age by race
pop_2020 <- get_decennial(geography = "county", 
                          variables = "P1_001N", 
                          year = 2020)

### total 2020 pop
total_pop_2020 <- pop_2020 %>% 
  reframe(total_pop = sum(value)) %>% 
  pull(total_pop)

### racial demographics
### racial pop 2020
race_2020 <- get_decennial(
  geography="county",
  variables = c(total = "P1_001N",
                white = "P1_003N",
                black = "P1_004N",
                native= "P1_005N",
                asian= "P1_006N",
                two_plus= "P1_009N"),
  year = 2020
)

### 2020 total pop by race
totals_2020 <- race_2020 %>% 
  group_by(variable) %>% 
  reframe(now = sum(value, na.rm = T)) %>% 
  arrange(desc(now))

### 2010 population by race
race_2010 <- get_decennial(
  geography="county",
  variables = c(total = "P003001",
                white = "P003002",
                black = "P003003",
                native = "P003004",
                asian = "P003005",
                two_plus = "P003008"),
  year = 2010,
  sumfile = "sf1"
)

### 2010 total pop by race
totals_2010 <- race_2010 %>% 
  group_by(variable) %>% 
  reframe(before = sum(value, na.rm = T)) %>% 
  arrange(desc(before))

### total 2010 pop
total_pop_2010 <- race_2010 %>% 
  filter(variable == "total") %>% 
  reframe(total_pop = sum(value)) %>% 
  pull(total_pop)

### 2010 racial composition of the US
totals_2010 %>% 
  mutate(pop = total_pop) %>% 
  group_by(variable) %>% 
  reframe(percent = before / pop) %>% 
  arrange(desc(percent))

### 2020 racial composition of the US
totals_2020 %>% 
  mutate(pop = total_pop) %>% 
  group_by(variable) %>% 
  reframe(percent = before / total_pop_2020) %>% 
  arrange(desc(percent))

### Change in racial makeup
totals_2010 %>% 
  left_join(totals_2020, by="variable") %>% 
  group_by(variable) %>% 
  mutate(change = ((now - before)/before)*100) %>% 
  arrange(desc(change))
