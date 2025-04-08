# the population is created
library(httr2)
request <- request("https://api.census.gov/data/2021/pep/population?get=POP_2020,POP_2021,NAME&for=state:*&key=4e1d11aa625a6852ba398de9897f9809e3a28e79")
response <- request |> req_perform()
population <- response |> resp_body_json(simplifyVector = T) 
library(tidyverse)
library(janitor)
population <- population |> 
  row_to_names(row_number = 1) |>  
  as_tibble() |>  
  select(-state) |>  
  rename(state_name = NAME) |>  
  pivot_longer(cols = starts_with("POP_"),  
               names_to = "year", 
               values_to = "population") |> 
  mutate(year = str_remove(year, "POP_"),  
         year = as.numeric(year),  
         population = as.numeric(population)) |>  
  mutate(state = case_when(
    state_name == "District of Columbia" ~ "DC",
    state_name == "Puerto Rico" ~ "PR",
    TRUE ~ state.abb[match(state_name, state.name)]  
  ))

# the regions are read in
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

save(population, file = "./data/population.rda")