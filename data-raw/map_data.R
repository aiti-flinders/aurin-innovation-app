## code to prepare `map_data` dataset goes here
library(aurininnovation)
library(sf)
library(tibble)
library(dplyr)

map_data <- tibble(bind_rows(
  aurininnovation:::sa2_2011 %>% select(sa2_name, sa3_name, sa4_name, gcc_name, state_name) %>% mutate(year = 2011),
  aurininnovation:::sa2_2016 %>% select(sa2_name, sa3_name, sa4_name, gcc_name, state_name) %>% mutate(year = 2016)
))

usethis::use_data(map_data, compress = "xz", overwrite = TRUE)
