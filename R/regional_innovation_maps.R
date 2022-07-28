
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
                             choiceValues = c("innovation_score", "human_knowledge_score", "patent_output_score")
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
          pivot_longer(cols = ends_with("_score"),
                       names_to = "indicator",
                       values_to = "value") %>%
          mutate(value_label = scales::label_number(accuracy = 0.1)(value)) %>%
          st_as_sf() %>%
          st_transform("+proj=longlat +datum=WGS84")
      })

      map_draw <- function(map) {
        colour_by <- input$colour
        colour_data <- map_data() %>%
          filter(indicator == input$colour) %>%
          pull(value)
        colour_pal <- case_when(
          input$colour == "innovation_score" ~ "Blues",
          input$colour == "human_knowledge_score" ~ "Purples",
          input$colour == "patent_output_score" ~ "Greens"
        )
        legend_title <- paste(str_to_title(gsub("_", " ", colour_by)))

        pal <- colorBin(colour_pal, colour_data, bins = 5, pretty = TRUE, na.color = 'black')

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
                      label = ~paste(sa2_name, ": ", value_label),
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
                     data = map_data() %>%
                       filter(indicator == input$colour)) %>%
          map_draw()

      })

      popup <- function(sa2, lng, lat) {
        selected <- map_data() %>%
          filter(sa2_name == sa2) %>%
          pivot_wider(id_cols = -value_label,
                      names_from = indicator,
                      values_from = value)

        content <- as.character(tagList(
          tags$h4(sprintf("Innovation Score: %.1f", selected$innovation_score)),
          tags$strong(HTML(sprintf("%s, %s, %s",
                                   selected$sa2_name, selected$state_name, selected$sa4_name
          ))), tags$p(),
          tags$h5(HTML(sprintf("Patent Output Score: %.1f", selected$patent_output_score))),
          sprintf("Patents per 1000 employees: %.0f", selected$patents), tags$br(),
          sprintf("Backwards citations: %.0f", selected$backwards_citations), tags$br(),
          tags$h5(HTML(sprintf("Human Knowledge Score: %.1f", selected$human_knowledge_score))),
          sprintf("Average skill level of occupations: %.0f", selected$skill), tags$br(),
          sprintf("Proportion KIBS employment: %s", scales::percent(selected$kibs)), tags$br(),
          sprintf("Education level: %.0f", as.numeric(selected$qualification))

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
          setView(lng = input$map_center$lng,
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
