
library(tidycensus)
library(dplyr)
library(ggplot2)
library(sf)
library(tigris)

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


