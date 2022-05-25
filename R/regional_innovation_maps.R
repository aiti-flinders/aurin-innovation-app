library(shinyWidgets)

innovationMapUI <- function(id, data) {

  ns <- NS(id)


  tabPanel(title = "Map",
           leafletOutput(ns("map"), height = "800px"),
           absolutePanel(id = "controls",
                         class = "panel panel-default",
                         fixed = TRUE,
                         draggable = TRUE,
                         top = 60,
                         right = 20,
                         left = "auto",
                         bottom = "auto",
                         width = 200,
                         height = "auto",
                         radioGroupButtons(
                           justified = TRUE,
                           inputId = ns("year"),
                           label = "Year: ",
                           choices = c(2011, 2016)
                         ),
                         selectInput(
                           inputId = ns("states"),
                           label = "States: ",
                           choices = c("All states" = "",
                                       clean_state(1:8, to = "state_name")),
                           multiple = TRUE),
                         radioGroupButtons(direction = "vertical",
                                           inputId = ns("colour"),
                                           label = "Color: ",
                                           choiceNames = c("Innovation",
                                                           "Human Knowledge",
                                                           "Patent Output"),
                                           choiceValues = c("innovation",
                                                            "human_knowledge",
                                                            "patent_output")
                         )))
}

innovationMapServer <- function(id, data) {

  moduleServer(
    id,
    function(input, output, session) {

      bounds <- reactive({
        map_data() %>%
          st_bbox() %>%
          as.character()
      })

      output$map <- renderLeaflet({
        leaflet() %>%
          addTiles() %>%
          fitBounds(bounds()[1], bounds()[2], bounds()[3], bounds()[4])
      })



      map_data <- reactive({
        data %>%
          filter(
            year == input$year,
            is.null(input$states) | state_name %in% input$states
          ) %>%
          st_as_sf() %>%
          st_transform("+proj=longlat +datum=WGS84")
      })


      observe({
        colour_by <- input$colour
        colour_data <- map_data()[[colour_by]]
        colour_pal <- case_when(
          input$colour == "innovation" ~ "Blues",
          input$colour == "human_knowledge" ~ "Purples",
          input$colour == "patent_output" ~ "Greens"
        )
        legend_title <- paste(str_to_title(gsub("_", " ", colour_by)), "Score")

        pal <- colorBin(colour_pal, colour_data, bins = 7, pretty = TRUE, na.color = 'black')

        leafletProxy("map",
                     data = map_data()) %>%
          clearShapes() %>%
          addPolygons(fillColor = ~pal(colour_data),
                      weight = 0.5,
                      opacity = 0.6,
                      color = "black",
                      fillOpacity = 0.5,
                      highlightOptions = highlightOptions(
                        weight = 2,
                        fillOpacity = 0.75,
                        bringToFront = TRUE
                      ),
                      label = ~sa2_name,
                      layerId = ~sa2_name
          ) %>%
          addLegend("bottomleft",
                    pal = pal,
                    values = colour_data,
                    title = legend_title,
                    layerId = "color")
      })

      popup <- function(sa2, lng, lat) {
        selected <- map_data()[map_data()$sa2_name == sa2, ]
        content <- as.character(tagList(
          tags$h4("Innovation Score: ", as.integer(selected$innovation)),
          tags$strong(HTML(sprintf("%s, %s, %s",
                                   selected$sa2_name, selected$state_name, selected$sa4_name
                                   ))), tags$p(),
          tags$h5(HTML(sprintf("Patent Output Score: %s", as.integer(selected$patent_output)))),
          sprintf("Patents per 1000 employees: %s", as.integer(selected$patents)), tags$br(),
          sprintf("Backwards citations: %s", as.integer(selected$backwards_citations)), tags$br(),
          tags$h5(HTML(sprintf("Human Knowledge Score: %s", as.integer(selected$human_knowledge)))),
          sprintf("Average skill level of occupations: %s", round(selected$skill, 1)), tags$br(),
          sprintf("Proportion KIBS employment: %s", scales::percent(selected$kibs)), tags$br(),
          sprintf("Education level: %s", round(selected$qualification, 2))

        ))

        leafletProxy("map") %>% addPopups(lng, lat, content)
      }

      #Click to show information for an SA2
      observe({
        leafletProxy("map") %>%
          clearPopups()

        event <- input$map_shape_click
        if (is.null(event))
          return()

        isolate({
          popup(event$id, event$lng, event$lat)
        })
      })

    }
  )
}
