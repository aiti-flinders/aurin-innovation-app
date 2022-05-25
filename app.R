library(shiny)
library(shinyWidgets)
library(fresh)
library(bs4Dash)
library(dplyr)
library(stringr)
library(leaflet)
library(knitr)
library(sf)
library(DT)
library(strayr)
library(ggplot2)
library(aurininnovation)

map_data <- tibble(bind_rows(
    aurininnovation:::sa2_2011 %>% select(sa2_name, sa3_name, sa4_name, gcc_name, state_name) %>% mutate(year = 2011),
    aurininnovation:::sa2_2016 %>% select(sa2_name, sa3_name, sa4_name, gcc_name, state_name) %>% mutate(year = 2016)
))

innovation_data <- regional_innovation %>%
    left_join(map_data, by = c("year", "sa2_name"))



header <- dashboardHeader(
    skin = "light",
    title = dashboardBrand(
        title = "Regional Innovation",
        href = "http://www.flinders.edu.au/aiti",
        opacity = 1.0
    ),
    fixed = TRUE
)




states <- c("New South Wales",
            "Victoria",
            "Queensland",
            "South Australia",
            "Western Australia",
            "Tasmania",
            "Australian Capital Territory",
            "Northern Territory")



ui <- navbarPage("Regional Innovation",
                # tags$head(includeCSS("style.css")),
                 navbarMenu(title = "Regional Innovation",
                            innovationMapUI("innovation_map", data = innovation_data),
                            innovationCompareUI("innovation_compare", data = innovation_data)
                 ),
                 igoUI("igo", data = innovation_data),
                 navbarMenu("More",
                            tabPanel("Calculating IGO")
                 )
)

server <- function(input, output, session) {

    innovationMapServer("innovation_map", data = innovation_data)
    innovationCompareServer("innovation_compare", data = innovation_data)
    igoServer("igo", data = innovation_data)



}

shinyApp(ui = ui, server = server)
