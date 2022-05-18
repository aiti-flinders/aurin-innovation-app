library(shiny)
library(shinyWidgets)
library(fresh)
library(bs4Dash)
library(dplyr)
library(leaflet)
library(knitr)
library(sf)
library(DT)
library(aurininnovation)

map_data <- tibble(bind_rows(
    aurininnovation:::sa2_2011 %>% select(sa2_name, sa3_name, sa4_name, gcc_name, state_name) %>% mutate(year = 2011),
    aurininnovation:::sa2_2016 %>% select(sa2_name, sa3_name, sa4_name, gcc_name, state_name) %>% mutate(year = 2016)
))

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
                 navbarMenu(title = "Regional Innovation",
                            tabPanel("Maps",
                                     sidebarLayout(
                                         sidebarPanel(
                                             selectInput("state", "Select State",
                                                         states
                                             ),
                                             radioButtons("year", "Select Year",
                                                          c(2011, 2016))
                                         ),
                                         mainPanel(
                                             leafletOutput("plot")
                                         )
                                     )
                            ),
                            tabPanel("Compare Regions",
                                     sidebarLayout(
                                         sidebarPanel(
                                             selectInput("state", "Select State",
                                                         states
                                             )
                                         ),
                                         mainPanel(
                                             tableOutput("compare")
                                         )
                                     )
                            )

                 ),
                 tabPanel("Industrial Growth Opportunities",
                          sidebarLayout(
                              sidebarPanel(
                                  selectInput("stateigo", "Select State",
                                              states
                                  ),
                                  selectInput("regionigo", "Select Region",
                                              choices = map_data %>%
                                                  filter(state_name == "New South Wales",
                                                         year == 2011) %>%
                                                  distinct(sa2_name) %>%
                                                  pull()),
                                  radioButtons("yearigo", "Select Year",
                                               choiceNames = c("2011", "2016"),
                                               choiceValues = c(2011, 2016)),
                                  numericInput(inputId = "exportlimit", "Minimum State Export",
                                               value = 0,
                                               min = 0,
                                               max = 1e6,
                                               step = 10000),
                                  numericInput(inputId = "coglimit", "Minimum Benefit",
                                               value = 0,
                                               min = -3,
                                               max = 3,
                                               step = 1),
                                  numericInput(inputId = "rcalimit", "Maximum RCA",
                                               value = 1,
                                               min = 0,
                                               max = 5),
                                  numericInput("icalimit", "Minimum ICA",
                                               value = 1,
                                               min = 0,
                                               max = 5)

                              ),
                              mainPanel(
                                  DT::dataTableOutput("table")
                              )

                          )
                 ),
                 navbarMenu("More",
                            tabPanel("Calculating IGO")
                 )
)

server <- function(input, output, session) {


    create_data <- reactive({

        df <- regional_innovation %>%
            left_join(map_data, by = c("year", "sa2_name")) %>%
            filter(state_name == input$state,
                   year == input$year) %>%
            st_as_sf()


    })
    observe({
        choices <- map_data %>%
            filter(state_name == input$stateigo,
                   year == input$yearigo) %>%
            distinct(sa2_name) %>%
            pull()


        updateSelectInput(session, "regionigo", "Select Region",
                          choices = choices)
    })

    output$regionigo <- renderUI({
        selectInput("regionigo", "Select Region",
                    choices = map_data %>%
                        filter(state_name == input$stateigo,
                               year == input$yearigo) %>%
                        distinct(sa2_name) %>%
                        pull())

    })

    output$table <- DT::renderDataTable(
        igo(year = as.double(input$yearigo),
            region = input$regionigo,
            .export_value_limit = input$exportlimit) %>%
            select(c("Product Opportunity",
                     "Product Development Benefit",
                     "Region Industry Comparative Advantage",
                     "State Export Value",
                     "State Export Comparative Advantage")) %>%
            DT::datatable() %>%
            formatRound(c(2:3, 5), digits = 2) %>%
            formatCurrency(4)

    )

    create_plot <- reactive({

        domain <- create_data() %>%
            pull(innovation)



        pal <- colorBin("Blues", domain, 6, pretty = TRUE)

        leaflet(create_data()) %>%
            addTiles() %>%
            addPolygons(
                fillColor = ~pal(domain),
                weight = 1,
                opacity = 0.6,
                color = "black",
                dashArray = "",
                fillOpacity = 0.8,
                highlight = highlightOptions(
                    weight = 2,
                    color = aititheme::aiti_blue,
                    dashArray = "",
                    fillOpacity = 1,
                    bringToFront = TRUE),
                label = ~paste0(sa2_name, ": ", round(innovation, 2)))

    })

    output$plot <- renderLeaflet({

        create_plot()

    })

}

shinyApp(ui = ui, server = server)
