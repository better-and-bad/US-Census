# US-Census

Big thank you to Kyle Walker for making the tidycensus package! Census data is the most detailed, publically available information we will ever have in our lives. Walker just made it more easily avialbel to us than ever before in history.

Census data is used to redistrict US counties, determine who receives trillions of dollars in financial aid and more. 

## Overview

In the todycensus.R script, I use Walker's tidycensus library to look into:

Demographics: Analyzing racial composition and population changes between 2010 and 2020.

Rural Areas: Identifying rural counties based on housing and population criteria.

Income Distribution: Visualizing median household income by race and geography.

Migration: Mapping migration flows and growth patterns in major cities and states.


## Setup
1) Download R Studio
2) Install Packages
 - install.packages(c("tidycensus", "dplyr", "ggplot2", "sf", "tigris", "tidyr", "ggthemes", "mapdeck", "glue", "stringr"))
3) Set Your Census API Key: [Obtain your Census API key](https://api.census.gov/data/key_signup.html), then, set it in R
4) Copy and Paste the tidycensus.R file above.



