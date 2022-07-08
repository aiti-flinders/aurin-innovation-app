## code to prepare `innovation_data` dataset goes here
library(aurininnovation)
library(dplyr)

innovation_data <- regional_innovation %>%
  left_join(map_data, by = c("year", "sa2_name"))

usethis::use_data(innovation_data, compress = "xz", overwrite = TRUE)
