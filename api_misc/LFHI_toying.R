# Get historical labor data using labour bureau Api
library(tidyverse)
library(feather)
library(blscrapeR)

# search for ids
# search_ids(keyword = c("Kansas City"))


set_bls_key("56dd40e8188e4ddba775ebc5ea28bcb1")
# First time, reload your enviornment so you can use the key without restarting R.
readRenviron("~/.Renviron")
# You can check it with:
Sys.getenv("BLS_KEY")

MONTHS <- c("January", "February", "March", "April", "May", "June", "July",
            "August", "September", "October", "November", "December")


### STATE LEVEL DATA (since 2000)
month_dates <- expand.grid(MONTHS, seq(2000, 2016))

dates <- month_dates %>%
  unite( col = "month" , Var1, Var2, sep = " ") %>%
  as.list() 

all_employment_data_2017 <- get_bls_state(date_mth = dates$month)

state_level_data <- all_employment_data %>%
  bind_rows(all_employment_data_2017) %>%
  filter(state %in% c("Missouri", "Illinois", "Minnesota")) %>%
  mutate(month = as_date(month)) %>%
  select(state_abb, month, unemployed_rate ) %>%
  mutate(level = "state")


## COUNTY LEVEL DATA (lass 10 months)
this_year <- data.frame(month = MONTHS[1:10], year = "2017") %>%
  unite( col = "month" , month, year, sep = " ") %>%
  as.list() 

clay_MO <- get_bls_county(date_mth = this_year$month,
                     stateName = c("Missouri")) %>%
  filter(area_title == c("Clay County, MO"))

cook_IL <- get_bls_county(date_mth = this_year$month,
                          stateName = c("Illinois")) %>%
  filter(area_title == c("Cook County, IL"))

ramsey_MN <- get_bls_county(date_mth = this_year$month,
                          stateName = c("Minnesota")) %>%
  filter(area_title == c("Ramsey County, MN"))

county_level_data <- bind_rows(ramsey_MN, clay_MO, cook_IL) %>%
  mutate(state_abb = if_else(fips_state == "27", "MN",
                             if_else(fips_state == "29", "MO", "IL"))) %>%
  rename(month = period) %>%
  select(state_abb, month, unemployed_rate ) %>%
  mutate(unemployed_rate = as.numeric(unemployed_rate),
        level = "county")


### MERGE COUNTY AND STATE DATA
all_historical_data <- bind_rows(state_level_data,
          county_level_data) %>%
  mutate(employement_rate = 100 - unemployed_rate)

write_feather(all_historical_data, "LFHI_employement_data.feather")








