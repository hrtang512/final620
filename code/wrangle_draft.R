library(readxl)
library(tidyverse)
library(janitor)

# https://www.census.gov/data/tables/time-series/demo/popest/2020s-national-total.html
pop_raw <- read_excel("../data/NST-EST2024-POP.xlsx", skip = 3) |>
  clean_names() 

pop_raw <- pop_raw |>
  rename(state_name = x1) |>
  mutate(state_name = str_remove(state_name, "^\\."))  


pop_clean <- pop_raw |>
  pivot_longer(cols = starts_with("x"),
               names_to = "year",
               values_to = "population") |>
  mutate(
    year = as.numeric(sub("x", "", year)),
    pop = as.numeric(population),
    state = case_when(
      state_name == "District of Columbia" ~ "DC",
      state_name == "Puerto Rico" ~ "PR",
      TRUE ~ state.abb[match(state_name, state.name)]
    )
  ) |>
  drop_na(state)  

population <- pop_clean |>
  filter(year %in% 2020:2023)

library(jsonlite)
library(purrr)
url <- "https://github.com/datasciencelabs/2024/raw/refs/heads/main/data/regions.json"
# regions <- use fromJSON to read as a data.frame
regions = fromJSON(url, simplifyDataFrame = T) |>
  mutate(
    region_name = case_when(
      region_name == "New York and New Jersey, Puerto Rico, Virgin Islands" ~ "NY, NJ, PR, VI",
      TRUE ~ region_name
    ),
    region = as.factor(unlist(region))
  ) |> 
  unnest(states) |>
  rename(state_name = states)

# where we combine
population <- population |>
  left_join(regions, by = "state_name")

save(population, file = "../data/population.rda")
