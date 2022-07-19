
innovationMapUI <- function(id, data) {

  ns <- NS(id)
  tabPanel(title = "Innovation Map",
           leafletOutput(ns("map"), width = '100%', height = "600px"),
           fluidRow(
             dashboard_box(collapsible = TRUE,
                           title = "Terms",
                           p(tags$b("Innovation: "),
                             "Second-order factor derived from Human Knowledge and Patent Output factors."),
                           p(tags$b("Human Knowledge: "),
                             "Factor derived from knowledge intensive business services,
                             regional skill level, and regional qualification level"),
                           p(tags$b("Patent Output: "),
                             "Factor derived from the number of patents per 1000 workers, and
                             the number of backward citations each patent recieved.")
             ),
             dashboard_box(collapsible = TRUE,
                           title = "Customise Map",
                           selectInput(
                             inputId = ns("states"),
                             label = "Select States: ",
                             choices = c("All states" = "",
                                         clean_state(1:8, to = "state_name")),
                             multiple = TRUE
                           ),
                           radioGroupButtons(
                             justified = TRUE,
                             inputId = ns("year"),
                             label = "Select Year: ",
                             choices = c(2011, 2016)
                           ),
                           radioGroupButtons(
                             direction = "vertical",
                             justified = TRUE,
                             inputId = ns("colour"),
                             label = "Select Indicator: ",
                             choiceNames = c("Innovation", "Human Knowledge", "Patent Output"),
                             choiceValues = c("innovation", "human_knowledge", "patent_output")
                           )
             ),
             dashboard_box(collapsible = TRUE,
                           title = "Downloads",
                           width = 4,
                           download_graph_ui(id)
             )
           )
  )
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

      map_reactive <- reactive({
        leaflet(data = map_data()) %>%
          addTiles() %>%
          fitBounds(bounds()[1], bounds()[2], bounds()[3], bounds()[4]) %>%
          map_draw()
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

      map_draw <- function(map) {
        colour_by <- input$colour
        colour_data <- map_data()[[colour_by]]
        colour_pal <- case_when(
          input$colour == "innovation" ~ "Blues",
          input$colour == "human_knowledge" ~ "Purples",
          input$colour == "patent_output" ~ "Greens"
        )
        legend_title <- paste(str_to_title(gsub("_", " ", colour_by)), "Score")

        pal <- colorBin(colour_pal, colour_data, bins = 7, pretty = TRUE, na.color = 'black')

        map %>%
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
      }


      observe({
        leafletProxy("map",
                     data = map_data()) %>%
          map_draw()

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

      user_map <- reactive({
        map_reactive() %>%
          map_draw() %>%
          setView(lng =  input$map_center$lng,
                  lat = input$map_center$lat,
                  zoom = input$map_zoom)

      })

      output$download_plot <- downloadHandler(
        filename = function(){
          paste0(input$filename, "-map.", input$filetype)
        },
        content = function(file) {
          mapview::mapshot(user_map(), file = file, cliprect = "viewport", selfcontained = FALSE)
        }
      )

    }
  )
}
