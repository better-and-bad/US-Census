

#### census
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


### incarcerated americans
jail <- get_decennial(geography = "county",
                      variables=c(correctional = "P5_003N",
                                  juvenile = "P5_004N"),
                      year=2020)
### rename to create master county df
jail <- jail %>% 
  rename(jail = value)

### jail long for county master data frame ###
counties <- jail %>% 
  select(NAME, variable, jail) %>% 
  tidyr::pivot_wider(names_from = variable, values_from = jail)

### states with the most rural counties
jail_states <- jail %>% 
  tidyr::separate(NAME, sep=",", into=c("county", "state")) %>% 
  group_by(state, variable) %>% 
  reframe(people = sum(jail)) %>% 
  arrange(desc(people))

### total us
jailed_pop <- jail_states %>% 
  reframe(jailed_pop = sum(people)) %>%
  pull(jailed_pop)

### percent of pop incarcerated
(jailed_pop / pop_2020) * 100

### percent living in rural areas
### jailed state total
total_jailed <- jail_states %>% 
  group_by(state) %>% 
  reframe(jail = sum(people))

### percent jailed per state
jailed_per_state <- age20 %>% 
  tidyr::separate(NAME, sep=",", into=c("county", "state")) %>% 
  group_by(state) %>% 
  reframe(people = sum(value)) %>% 
  arrange(desc(people)) %>% 
  left_join(total_jailed, by = "state") %>% 
  mutate(
    percent_jailed = (jail/people) *100) %>% 
  arrange(desc(percent_jailed))


### pivot wider race df
units_wide <- units_dec %>% 
  select(-GEOID) %>% 
  tidyr::pivot_wider(names_from = variable, values_from = value) 
dim(counties_dec)

### join units in counties df
### drop off the only 243 counties
counties_dec <- counties_dec %>% 
  select(-total_units, -vacant, -occupied) %>% 
  left_join(units_wide, by="NAME")

### exlore counties data frame
counties_dec %>% 
  ggplot() +
  geom_point(aes(total_units, total_pop, color=white), size=3, alpha=0.6) +
  theme_minimal()


      ######## income and federal aid ###########
acs_2023 <- load_variables(2023, "acs1")

# Get median household income by race
income_data <- get_acs(
  geography = "puma",
  variables = c(
    White = "B19013A_001",
    Black = "B19013B_001",
    Asian = "B19013D_001",
    Hispanic = "B19013I_001"),
  year = 2023,
  survey = "acs1",
  geometry = FALSE)

# View the average median household income per race
income_data %>%
  group_by(variable) %>%
  reframe(median_income = mean(estimate, na.rm = TRUE))


income_data %>% 
  ggplot(aes(forcats::fct_reorder(variable, estimate), estimate, fill=variable)) +
  ggdist::stat_halfeye(adjust=0.5,
                       justification= -0.2,
                       .width=0,
                       point_colour=NA) +
  geom_boxplot(width=0.15,
               alpha=0.5) +
  coord_flip() +
  guides(fill="none") +
  tidyquant::scale_fill_tq() +
  tidyquant::theme_tq() + 
  theme(
    axis.title.y= element_blank(),
    axis.text.y = element_text(vjust=2)
  ) +
  labs(title="Distribution of Household Income", subtitle = "Inflation Adj.-2017",
       caption="Source: Census, tidycensus",
       y="") +
  scale_y_continuous(labels = scales::dollar_format()) +
  theme(
    plot.title = element_text(size=18, face="bold", hjust=0.5),
    plot.subtitle = element_text(size=14, hjust=0.5),
    axis.text.y = element_text(size=16, face="bold", vjust=-2),
    axis.text.x = element_text(size=14, face="bold")    
  ) 

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

### save data for shiny app
us_income <- tidycensus::get_acs(geography = "puma",
                                 variable="B19013_001",
                                 year="2023",
                                 survey="acs1",
                                 geometry = TRUE)

st_write(us_income, "us_income.geojson", driver = "GeoJSON", append=FALSE)

us_income_clean <- st_collection_extract(us_income, type = "POLYGON")

### write data out
st_write(us_income_clean, "us_income_clean.geojson")

### ID lowest income area
hh_income <- get_acs(geography = "puma",
                     variable="B19013_001",
                     year="2023",
                     survey="acs1")

library(stringr)

low_income <- hh_income %>% 
  filter(!str_detect(NAME, "Puerto Rico")) %>% 
  group_by(NAME) %>% 
  arrange(estimate) %>% 
  head(15)

### poorest income counties in the US
# Define the custom labels as a named vector, mapping original names to shortened ones
labels_short <- c(
  "Bronx CD 3 & 6, NY",
  "Detroit NW, MI",
  "Cleveland East, OH",
  "Louisville NW, KY",
  "Baton Rouge North, LA",
  "Philadelphia North, PA",
  "Detroit NE, MI",
  "Bronx CD 1 & 2, NY",
  "Flint City, MI",
  "Bronx CD 5, NY",
  "South Delta, MS",
  "Detroit SW, MI",
  "Cumberland Valley South, KY",
  "Toledo Central, OH",
  "Bronx CD 4, NY"
)

# Top 15 low-income PUMA regions in the US
low_income %>%
  ggplot() +
  geom_errorbar(aes(x= NAME, ymin = estimate - moe, ymax = estimate + moe),
                size=1) +
  geom_point(aes(reorder(NAME, -estimate), estimate), size=6, alpha=0.7, color="darkgreen") +
  scale_x_discrete(labels = labels_short) +  # Apply the custom labels
  coord_flip() +
  theme_minimal() +
  theme(axis.text.y = element_text(size=12),
        axis.text = element_text(size=14, face="bold"),
        axis.title = element_text(size=14),
        plot.title = element_text(size=18, hjust=0.5, face="bold"),
        plot.subtitle = element_text(size=16, hjust=0.5),
        plot.caption = element_text(size=12)) +
  labs(y="Median Household Income", x="",
       caption  = "Source: US Census, tidycensus") +
  labs(title="Poorest Regions in the US", subtitle = "PUMA Regions") +
  scale_y_continuous(labels = scales::label_dollar())


# interactive median income map
library(glue)
popup_content <- glue::glue(
  "<strong>{us_income$NAME}</strong><br>",
  "Median Income: {scales::dollar_format() (us_income$estimate)}"
)

us_income$popup <- popup_content

### build the interactive map
Sys.setenv(MAPBOX_PUBLIC_TOKEN = "pk.eyJ1IjoiamNvbm5zIiwiYSI6ImNtMjVmc2ppZzBzM3EybHB1bGJsNDk3MHgifQ.tFzW7DeAnFbDPgXac_LC5A")

library(mapgl)

geo_median_income <- mapboxgl(style=mapbox_style("light"),
                              center = c(-98.57, 39.82),
                              zoom=3) %>% 
  add_fill_layer(id="puma_income",
                 source=us_income,
                 fill_color=interpolate(
                   column="estimate",
                   values=c(25000, 50000, 100000, 150000, 200000),
                   stops = viridisLite::plasma(5),
                   na_color="lightgrey"),
                 fill_opacity = 0.7,
                 popup="popup",
                 hover_options = list(
                   fill_color="cyan",
                   fill_opacity = 1
                 )) %>% 
  add_legend("Median HH Income by Puma",
             values=c("$25k", "50k", "100k", "$200k"),
             colors= viridisLite::magma(5))

sf::st_write(us_income, "us_incom.geojson")


                    ### honorable mention ###

### age pyramids per state
### clean variable names
age <- get_estimates(
  geography = "state",
  product = "characteristics",
  breakdown = c("SEX", "AGEGROUP"),
  breakdown_labels = TRUE,
  vintage  = 2023
)

us_pyramid_data <- age %>% 
  filter(str_detect(AGEGROUP, "^Age"),
         SEX != "Both sexes") %>%
  group_by(NAME) %>%
  mutate(prop = value / sum(value, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(prop = ifelse(SEX == "Male", -prop, prop))

### plot age pyramid per state
ggplot(us_pyramid_data, aes(x = prop, y = AGEGROUP, fill = SEX)) + 
  geom_col(width = 1) + 
  scale_fill_manual(values = c("darkred", "darkblue")) + 
  facet_wrap(~NAME, scales="free") + 
  theme_classic() +
  theme(axis.text = element_blank(),
        strip.text.x = element_text(size = 10),
        plot.title = element_text(size=18, hjust=0.5, face="bold"),
        plot.caption = element_text(size=10),
        legend.text = element_text(size=12)) + 
  labs(x = "", 
       y = "", 
       title = "Age Pyramids - 2023", 
       fill = "", 
       caption = "Source: Census Bureau, tidycensus") 
#### change in life expectancy and income ?
library(tidycensus)
estimates_list <- list_estimates(product = "characteristics", geography = "state", vintage = 2023)

### we can call multiple years with time_series=T
age_time <- get_estimates(
  geography="state",
  product="characteristics",
  breakdown = "AGEGROUP",
  breakdown_labels = T,
  time_series = T,
  vintage = 2023
)

flow <- get_flows(
  geography = "metropolitan statistical area",
  state="WA",
  year=2023
)

all <- get_estimates(
  geography = "state",
  variables = "all",
  time_series = T)


### net migration per county
migration <- get_flows(geography = "county",
                       geometry = T
)

### aggregate to the state level
library(tidyr)
net_ex_state <- migration %>% 
  separate(FULL1_NAME, sep=",", into=c("country", "state")) %>% 
  group_by(state) %>% 
  filter(variable == "MOVEDNET") %>% 
  reframe(netex = sum(estimate, na.rm=T))

### geo-plot migration by state
us <- map_data("state")

### clean data to match us state
net_ex_state <- net_ex_state %>%
  mutate(state = trimws(state)) %>% 
  mutate(state = tolower(state))

### join geo-labels and data
map_data <- us %>%
  left_join(net_ex_state, by = c("region" = "state"))

### Net Migration plot
ggplot(map_data, aes(x = long, y = lat, group = group, fill = netex)) +
  geom_polygon(color = "white") +
  ggthemes::theme_map() +
  labs(title = "Net Migration by State",
       caption="Source: Census, tidycensus") +
  coord_fixed(1.3)   +
  theme(
    plot.title = element_text(size=18, hjust=0.5, face="bold"),
    legend.title = element_text(size=12),
    legend.text = element_text(size=12),
    plot.caption = element_text(size=12)
  ) +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0, 
                       name = "Net Migration",
                       labels = scales::comma) 

### shape file
usa <- st_as_sf(maps::map("state", fill=TRUE, plot =FALSE))

### join migration data to shape file
sf_migration <- usa %>%
  left_join(net_ex_state, by = c("ID" = "state"))

### plot migration by state
ggplot(sf_migration) +
  geom_sf(aes(fill=netex)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0, name = "Net Migration") +
  ggthemes::theme_map() +
  scale_fill_gradient2(
    low = "red", mid = "white", high = "blue", midpoint = 0, 
    name = "Net Migration", 
    labels = scales::label_number(scale = 1e-3, suffix = "K")  # Converts numbers to thousands
  ) +
  theme(
    legend.text = element_text(size=12),
    legend.title = element_text(size=12),
    plot.title = element_text(size=16, hjust=0.5),
    plot.caption = element_text(size=10)) +
  labs(title = "Migration by State",
       caption="Source: Census, tidycensus")

### morning commute
trans <- get_acs(
  geography="puma",
  variables=c(walk = "B08101_033", 
              home = "B08101_049",
              taxi = "B08101_041",
              car = "B08101_009",
              public = "B08101_025"),
  survey="acs1",
  year=2023)

time_to_work <- get_acs(geography="county",
                        variable="C08534_001",
                        survey="acs1",
                        year=2023)

trans %>% 
  tidyr::drop_na() %>% 
  group_by(variable) %>% 
  reframe(
    total = sum(estimate)) %>% 
  arrange(desc(total))

### home affordability
### mortgage as ratio of value to hh income
mortgage <- get_acs(geography="puma",
                    table="B25100",
                    survey="acs1",
                    year=2023)

time_to_work <- get_acs(geography="county",
                        variable="C08534_001",
                        survey="acs1",
                        year=2023)

median_income <- get_acs(
  geography="county",
  variable="B19013_001",
  survey="acs1",
  year=2023)


### per region
### color by 4+ mortgage
county_df <- median_income %>% 
  rename(income = estimate) %>% 
  left_join(time_to_work %>% rename(time = estimate), by="NAME") %>% 
  mutate(state = str_extract(NAME, "\\b\\w+$")) %>% 
  na.omit()

### create column for regions
county_df <- county_df %>% 
  mutate(Region = case_when(
    # Northeast
    state %in% c("Maine", "New Hampshire", "Vermont", "Massachusetts", "Rhode Island", 
                 "Connecticut", "New York", "New Jersey", "Pennsylvania") ~ "Northeast",
    # South
    state %in% c("Delaware", "Maryland", "District of Columbia", "Virginia", "West Virginia", 
                 "North Carolina", "South Carolina", "Georgia", "Florida", "Kentucky", 
                 "Tennessee", "Alabama", "Mississippi", "Arkansas", "Louisiana", "Oklahoma", 
                 "Texas") ~ "South",
    # Midwest
    state %in% c("Ohio", "Michigan", "Indiana", "Illinois", "Wisconsin", "Minnesota", 
                 "Iowa", "Missouri", "North Dakota", "South Dakota", "Nebraska", "Kansas") ~ "Midwest",
    # West
    state %in% c("Montana", "Idaho", "Wyoming", "Colorado", "New Mexico", "Arizona", 
                 "Utah", "Nevada", "Washington", "Oregon", "California", "Alaska", "Hawaii") ~ "West",
    # Default case if state not matched (optional)
    TRUE ~ "Other"
  ))

### 445 counties
county_df %>% 
  filter(Region != "Other") %>% 
  ggplot() +
  geom_jitter(aes(log(time), log(income), color=Region), size=4, alpha=0.7) +
  geom_smooth(aes(log(time), log(income)), method = "lm", se=F) +
  labs(y="Median Income", x= "Time to Work (min)", title="Your Morning Commute May Pay Off", 
       subtitle="Logged Values", caption="Source: Census, tidycensus") +
  theme_minimal() +
  theme(
    legend.text = element_text(size=14),
    legend.title = element_text(size=14),
    plot.title = element_text(size=18, hjust=0.5, face="bold"),
    plot.subtitle = element_text(size=14, hjust=0.5),
    plot.caption = element_text(size=10),
    axis.title = element_text(size=14, face="bold"),
    axis.text = element_text(size=12))

### mortgage ratio of value to income by county
mortgage_geo <- get_acs(geography="puma",
                        table="B25100",
                        survey="acs1",
                        year=2023,
                        geometry=T)

### clean variable name
mortgage_geo_clean <- mortgage_geo %>% 
  left_join(acs, by=c("variable" = "name")) %>% 
  mutate(label_clean = str_remove(label, "^Estimate!!Total:!!|!!"),  # Remove "Estimate!!Total:" and the first "!!" symbols
         label_clean = str_replace_all(label_clean, "!!", " "),    # Replace remaining "!!" symbols with hyphens
         label_clean = str_squish(label_clean))

### plot region by it's highest mortgage ratio type
top_mortgage_type <- mortgage_geo_clean %>% 
  filter(!variable %in% c("B25100_001", "B25100_002", "B25100_007",
                          "B25100_008")) %>% 
  filter(!str_detect(label_clean, "Not")) %>% 
  group_by(NAME) %>% 
  slice_max(estimate, n=1) %>% 
  ungroup()

### highest mortgage ratio per county
ggplot(top_mortgage_type) +
  geom_sf(aes(fill = as.factor(label_clean)), color = "white", size = 0.1) +  # Fill by 'estimate'
  scale_fill_viridis_d(
    option = "plasma", 
    name = "Value to Income",
    labels = function(x) gsub(".*: ", "", x)  # Remove text up to and including the colon
  ) +  labs(
    title = "Mortgages: Ratio of Value to Income", caption="Source: Census, tidycensus",
    fill = "Mortgage Estimate") +
  coord_sf(
    xlim = c(-130, -60), ylim = c(23, 50), # Limits for the contiguous U.S.
    expand = FALSE) +
  ggthemes::theme_map() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "right",
    legend.text = element_text(size=12),
    plot.caption = element_text(size=10),
    legend.title = element_text(size=12)
  )

### plot count of ppl
pop_geo <- get_decennial(geography = "state", 
                         variables = "P1_001N", 
                         year = 2020,
                         geometry=T)
ggplot(pop_geo) +
  geom_sf(aes(fill=log(value)), size=0.1) +
  coord_sf(
    xlim = c(-130, -60), ylim = c(23, 50), # Limits for the contiguous U.S.
    expand = FALSE) +
  guides(fill="none") +
  ggthemes::theme_map()

### top mortgage types
### as ratio of value to hh income
mortgage %>% 
  mutate(variable = case_when(
    variable == "B25100_001" ~ "total",
    variable == "B25100_002" ~ "with_mortgage",
    variable == "B25100_003" ~ "less_than_2",
    variable == "B25100_004" ~ "2-2.9",
    variable == "B25100_005" ~ "3-3.9",
    variable == "B25100_006" ~ "4+",
    variable == "B25100_007" ~ "not_computed",
    variable == "B25100_008" ~ "not_mortgaged",
    variable == "B25100_009" ~ "not_mortgaged_2-",
    variable == "B25100_010" ~ "not_mortgaged_2-2.9",
    variable == "B25100_011" ~ "not_mortgaged_3-4.9",
    variable == "B25100_012" ~ "not_mortgaged_4+",
    TRUE ~ variable)) %>% 
  filter(variable != "total") %>% 
  group_by(variable) %>% 
  reframe(total = sum(estimate, na.rm=T)) %>% 
  arrange(desc(total))

### other county metrics are vast. mortgages of 4x income
### rural area
mortgage_clean <- mortgage %>% 
  mutate(variable = case_when(
    variable == "B25100_001" ~ "total",
    variable == "B25100_002" ~ "with_mortgage",
    variable == "B25100_003" ~ "less_than_2",
    variable == "B25100_004" ~ "2-2.9",
    variable == "B25100_005" ~ "3-3.9",
    variable == "B25100_006" ~ "4+",
    variable == "B25100_007" ~ "not_computed",
    variable == "B25100_008" ~ "not_mortgaged",
    variable == "B25100_009" ~ "not_mortgaged_2-",
    variable == "B25100_010" ~ "not_mortgaged_2-2.9",
    variable == "B25100_011" ~ "not_mortgaged_3-4.9",
    variable == "B25100_012" ~ "not_mortgaged_4+",
    TRUE ~ variable
  ))


mortgage_clean %>%  filter(variable != "total") %>% 
  mutate(state = str_extract(NAME, "\\b\\w+$")) %>% 
  group_by(state, variable) %>% 
  reframe(total = sum(estimate, na.rm=T)) %>% 
  group_by(state) %>% 
  slice_max(total, with_ties=FALSE)

### other county metrics are vast. mortgages of 4x income
### rural area
mortgage_clean <- mortgage %>% 
  mutate(variable = case_when(
    variable == "B25100_001" ~ "total",
    variable == "B25100_002" ~ "with_mortgage",
    variable == "B25100_003" ~ "less_than_2",
    variable == "B25100_004" ~ "2-2.9",
    variable == "B25100_005" ~ "3-3.9",
    variable == "B25100_006" ~ "4+",
    variable == "B25100_007" ~ "not_computed",
    variable == "B25100_008" ~ "not_mortgaged",
    variable == "B25100_009" ~ "not_mortgaged_2-",
    variable == "B25100_010" ~ "not_mortgaged_2-2.9",
    variable == "B25100_011" ~ "not_mortgaged_3-4.9",
    variable == "B25100_012" ~ "not_mortgaged_4+",
    TRUE ~ variable
  )) 

### plot mortgage as ratio of value to hh income type
mortgage_clean %>%  filter(variable != "total") %>% 
  mutate(state = str_extract(NAME, "\\b\\w+$")) %>% 
  group_by(variable) %>% 
  reframe(total = sum(estimate, na.rm=T)) %>% 
  arrange(desc(total)) %>% 
  ggplot() +
  geom_col(aes(reorder(variable, total), total, fill=variable)) +
  coord_flip() + 
  theme_minimal() +
  guides(fill="none") +
  labs(y="", x="Americans", title="Mortgage as Ratio of Value to HH Income") +
  theme(
    plot.title = element_text(size=16, face="bold", hjust=0.5)
  )


