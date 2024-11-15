

library(tidycensus)
library(dplyr)
library(ggplot2)
library(sf)
library(tigris)

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