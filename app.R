library(shiny)
library(fresh)
library(bs4Dash)
library(aurininnovation)

header <- dashboardHeader(
    skin = "light",
    title = dashboardBrand(
        title = "Regional Innovation",
        href = "http://www.flinders.edu.au/aiti",
        opacity = 1.0
    ),
    fixed = TRUE
)


sidebar <- dashboardSidebar(
    skin = "light",
    sidebarMenu(
        id = "sidebarmenu",
        menuItem(
            text = "Introduction",
            tabName = "intro",
            icon = icon("book"),
            selected = TRUE
        ),
        menuItem(
            text = "Regional Innovation",
            tabName = "regional_innov",
            icon = icon("chart-bar")
        ),
        menuItem(
            text = "Industrial Growth Opportunities",
            tabName = "igo",
            icon = icon("chart-bar")
        )
    )
)

regional_innovation <- tabItem(
    tabName = "regional_innov",
    fluidRow(
        tabBox(
            id = "regional_innovation_tab_id",
            width = 12,
            tabPanel(title = "Compare Regions"),
            tabPanel(title = "Maps")
            )
    )
)

igo <- tabItem(
    tabName = "igo",
    fluidRow(
        tabBox(
            id = "igo_tab_id",
            width = 12,
            tabPanel(title = "Opportunities")
        )
    )
)

body <- dashboardBody(
    tabItems(
        regional_innovation,
        igo
    )
)

ui <- dashboardPage(
    dark = FALSE,
    title = "Regional Innovation",
    header = header,
    sidebar = sidebar,
    body = body,
    footer = dashboardFooter(
        left = "Australian Industrial Transformation Institute",
        fixed = TRUE,
        right = "Flinders University"
    )
)

server <- function(input, output) {


}

shinyApp(ui = ui, server = server)
