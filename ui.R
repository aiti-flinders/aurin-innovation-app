library(shiny)
library(htmltools)
library(strayr)
library(leaflet)
library(mapview)
library(sf)
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(DT)
library(stringr)
library(aititheme)
library(aurininnovation)

map_data <- tibble(bind_rows(
  aurininnovation:::sa2_2011 %>% select(sa2_name, sa3_name, sa4_name, gcc_name, state_name) %>% mutate(year = 2011),
  aurininnovation:::sa2_2016 %>% select(sa2_name, sa3_name, sa4_name, gcc_name, state_name) %>% mutate(year = 2016)
))

innovation_data <- regional_innovation %>%
  full_join(map_data, by = c("year", "sa2_name")) %>%
  st_as_sf() %>%
  st_transform("+proj=longlat +datum=WGS84")

if (is.null(suppressMessages(webshot:::find_phantom()))) {webshot::install_phantomjs()}

ui <- fluidPage(
  navbarPage("Regional Innovation",
             innovationMapUI("innovation_map"),
             innovationCompareUI("innovation_compare", data = innovation_data),
             tabPanel("Documentation", icon = icon("book"),
                      fluidPage(
                        htmltools::tags$iframe(src = "regional_innovation.html",
                                               width = "100%",
                                               height = 1000,
                                               style = "border:none;")
                      ))
  )
)

