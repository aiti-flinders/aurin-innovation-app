innovationCompareUI <- function(id, data) {

  ns <- NS(id)

  tabPanel("Compare Regions",
           fluidRow(
             column(3,
                    selectInput(ns("state"), label = "State: ", choices = c("All states"="", unique(data$state_name)), multiple = TRUE),
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

      # output$hist <- renderPlot({
      #   ggplot(data = data[data$year == input$year, ], aes_string(x = input$var)) +
      #     geom_histogram(bins = 25)
      # })
      #
      # output$plot <- renderPlot({
      #   df <- data[data$sa2_name %in% input$sa2,] %>%
      #     select(sa2_name, year, innovation, patent_output, human_knowledge) %>%
      #     tidyr::pivot_longer(names_to = "indicator",
      #                         values_to = "value",
      #                         cols = -c(sa2_name, year))
      #
      #     ggplot(df) +
      #       geom_line(aes(x = year,
      #                     y = value,
      #                     col = indicator)) +
      #       facet_grid(~sa2_name)
      #
      # })

      create_data <- reactive({
        data %>%
          filter(is.null(input$state) | state_name %in% input$state,
                 is.null(input$sa4) | sa4_name %in% input$sa4,
                 is.null(input$sa2) | sa2_name %in% input$sa2,
                 year == as.numeric(input$year))
      })

      output$table <- renderDT({
        df <- create_data() %>%
          select(sa2_name, patents, backwards_citations, kibs, skill, qualification, human_knowledge, patent_output, innovation)
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
                               "Human Knowledge Score" = 'human_knowledge',
                               "Patent Output Score" = 'patent_output',
                               "Innovation Score" = 'innovation'),
                  rownames = FALSE) %>%
          formatRound(c("Occupation Skill Level", "Education Level"), digits = 1) %>%
          formatRound(c("Human Knowledge Score", "Patent Output Score", "Innovation Score"), digits = 1) %>%
          formatPercentage(c("KIBS Employment"), digits = 1)
      })


    }
  )
}
