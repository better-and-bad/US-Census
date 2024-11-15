# US-Census

This repository hosts the R code and data used for analyzing and visualizing US Census data.

Overview

The project explores various aspects of US Census data using the tidycensus R package. It includes:

Demographics: Analyzing racial composition and population changes between 2010 and 2020.
Rural Areas: Identifying rural counties based on housing and population criteria.
Income Distribution: Visualizing median household income by race and geography.
Migration: Mapping migration flows and growth patterns in major cities and states.

Key Features

Fetches decennial Census and ACS data using tidycensus.
Visualizes racial demographics, rural populations, income distributions, and migration flows.
Produces interactive and static visualizations for deeper insights.

Setup
Clone the Repository:

bash
Copy code
git clone git@github.com:better-and-bad/US-Census.git
cd US-Census
Install Required Packages: Open R or RStudio and run:

R
Copy code
install.packages(c("tidycensus", "dplyr", "ggplot2", "sf", "tigris", "tidyr", "ggthemes", "mapdeck", "glue"))
Set Your Census API Key: Obtain your Census API key, then, set it in R:

R
Copy code
census_api_key("your_census_api_key", install = TRUE)



