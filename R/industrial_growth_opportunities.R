library(shinyWidgets)

igoUI <- function(id, data) {

  ns <- NS(id)

  tabPanel("Industrial Growth Opportunities",
           sidebarLayout(
             sidebarPanel(
               radioGroupButtons(
                 inputId = ns("switch"),
                 label = "Focus: ",
                 choiceValues = c("Product", "Region"),
                 choiceNames = c("Product Opportunities",
                                 "Regional Opportunities"),
                 direction = "vertical",
                 justified = TRUE
               ),
               selectInput(ns("state"),
                           label = "State: ",
                           choices = unique(data$state_name)),
               uiOutput(ns("select1")),
               uiOutput(ns("select2")),
               radioGroupButtons(
                 inputId = ns("year"),
                 label = "Year: ",
                 choices = c(2011, 2016),
                 justified = TRUE
               ),
               p("Can I put Text Here?"),
               numericInput(
                 inputId = ns("exportlimit"),
                 label = "State Export Minimum Value:",
                 value = 0,
                 min = 0,
                 max = 1e6,
                 step = 10000),
               numericInput(
                 inputId = ns("coglimit"),
                 label = "Minimum Benefit to Region:",
                 value = 0,
                 min = -3,
                 max = 3,
                 step = 1),
               numericInput(
                 inputId = ns("rcalimit"),
                 label = "RCA Threshold",
                 value = 1,
                 min = 0,
                 max = 5),
               numericInput(
                 inputId = ns("icalimit"),
                 label = "ICA Threshold",
                 value = 1,
                 min = 0,
                 max = 5)

             ),
             mainPanel(
               DT::dataTableOutput(ns("table"))
             )

           )
  )

}

igoServer <- function(id, data) {
  moduleServer(
    id,
    function(input, output, session) {

      load("data/product_heirarchy.rda")

      # observe({
      #   sa2_choices <- data %>%
      #     filter(state_name == input$state,
      #            year == input$year) %>%
      #     distinct(sa2_name) %>%
      #     pull() %>%
      #     sort()
      #
      #   updateSelectInput(session, "sa2", choices = sa2_choices)
      # })


      output$table <- DT::renderDataTable(
        igo(year = as.double(input$year),
            product = input$product,
            .export_value_limit = input$exportlimit) %>%
          filter(location_code == input$state) %>%
          DT::datatable(data = .,
                        rownames = FALSE)
      )



      #Industrial growth opportunities focusing on products shows the regions
      #best suited for the development of a specified product.

      output$select1 <- renderUI({


        if (input$switch == "Product") {


          selectizeInput(session$ns("product_section"),
                         label = "Product Group: ",
                         choices = unique(product_heirarchy$section))



        }
      })

      output$select2 <- renderUI({

        if (input$switch == "Product") {
          selectizeInput(session$ns("product"),
                         label = "Product: ",
                         choices = product_heirarchy %>%
                           filter(section == input$product_section) %>%
                           distinct(d4) %>%
                           pull() %>%
                           sort())
        }
      })


    }
  )
}
