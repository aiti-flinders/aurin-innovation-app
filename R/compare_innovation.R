innovationCompareUI <- function(id, data) {

  ns <- NS(id)

  tabPanel("Compare Regions", icon = icon("search"),
           fluidRow(
             column(3,
                    selectInput(ns("state"),
                                label = "State: ",
                                choices = c("All states"="", unique(data$state_name)),
                                multiple = TRUE),
             ),
             column(3,
                    conditionalPanel("input.state", ns = ns,
                                     selectInput(ns("sa4"),
                                                 label = "Statistical Area (Level 4): ",
                                                 choices = c("All SA4"="", unique(data$sa4_name)),
                                                 multiple = TRUE)
                    )
             ),
             column(3,
                    conditionalPanel("input.state", ns = ns,
                                     selectInput(ns("sa2"),
                                                 label = "Statistical Area (Level 2): ",
                                                 choices = c("All SA2"="",unique(data$sa2_name)),
                                                 multiple = TRUE)
                    )
             ),
             column(3,
                    radioButtons(ns("year"), label = "Year: ", choices = c(2011, 2016))
             )
           ),
             downloadButton(ns("download_data"), label = "Download Table Data", class = "download-button"),

           DTOutput(ns("table"))
  )

}

innovationCompareServer <- function(id, data) {

  moduleServer(
    id,
    function(input, output, session) {

      observe({
        sa4 <- if (is.null(input$state)) character(0) else {
          data %>%
            filter(state_name %in% input$state,
                   year == input$year) %>%
            pull(sa4_name) %>%
            unique() %>%
            sort()
        }
        still_selected <- isolate(input$sa4[input$sa4 %in% sa4])
        updateSelectizeInput(session, "sa4", choices = sa4,
                             selected = still_selected, server = TRUE)
      })

      observe({
        sa2 <- if (is.null(input$sa4)) character(0) else {
          data %>%
            filter(state_name %in% input$state,
                   year == input$year,
                   is.null(input$sa4) | sa4_name %in% input$sa4) %>%
            pull(sa2_name) %>%
            unique() %>%
            sort()
        }
        still_selected <- isolate(input$sa2[input$sa2 %in% sa2])
        updateSelectizeInput(session, "sa2", choices = sa2,
                             selected = still_selected, server = TRUE)
      })


      create_data <- reactive({
        data %>%
          select(year, sa2_name, sa4_name, state_name, patents, backwards_citations, kibs, skill, qualification, human_knowledge_score, patent_output_score, innovation_score) %>%
          filter(is.null(input$state) | state_name %in% input$state,
                 is.null(input$sa4) | sa4_name %in% input$sa4,
                 is.null(input$sa2) | sa2_name %in% input$sa2,
                 year == as.numeric(input$year))
      })

      output$table <- renderDT({
        df <- create_data() %>%
          select(sa2_name, patents, backwards_citations, kibs, skill, qualification, human_knowledge_score, patent_output_score, innovation_score)

        datatable(df,
                  options = list(
                    order = list(list(1, "desc"))
                  ),
                  colnames = c("SA2 Name" = 'sa2_name',
                               "Patents" = 'patents',
                               "Backward Citations" = 'backwards_citations',
                               "KIBS Employment" = 'kibs',
                               "Occupation Skill Level" = 'skill',
                               "Education Level" = 'qualification',
                               "Human Knowledge Score" = 'human_knowledge_score',
                               "Patent Output Score" = 'patent_output_score',
                               "Innovation Score" = 'innovation_score'),
                  rownames = FALSE) %>%
          formatRound(c("Occupation Skill Level", "Education Level"), digits = 1) %>%
          formatRound(c("Human Knowledge Score", "Patent Output Score", "Innovation Score"), digits = 1) %>%
          formatPercentage(c("KIBS Employment"), digits = 1)
      })

      output$download_data <- downloadHandler(
        filename = function() {
          paste("innovation_data.csv")
        },
        content = function(file) {
          write.csv(create_data() %>% as_tibble() , file, row.names = FALSE)
        }
      )


    }
  )
}
