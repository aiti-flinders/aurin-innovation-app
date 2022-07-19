header <- function() {
  dashboardHeader(
    skin = "light",
    title = dashboardBrand(
      title = "Regional Innovation Browser",
      href = "https://aurin.org.au/about-aurin/projects/aurin-high-impact-projects/",
      opacity = 1.0
    ),
    fixed = TRUE
  )
}
sidebar <- function() {
  dashboardSidebar(
    skin = "light",
    width = "250px",
    sidebarMenu(
      menuItem(
        text = "Map",
        tabName = "map",
        icon = icon("map"),
        selected = TRUE
      ),
      menuItem(
        text = "Comparison",
        tabName = "data",
        icon = icon("chart-bar")
      )
    )
  )
}

body <- function() {
  dashboardBody(
    use_theme(app_theme()),
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom-assets/custom.css")),
    tabItems(
      tabItem(
        tabName = "map",
        innovationMapUI("innovation_map", data = innovation_data)
      ),
      tabItem(
        tabName = "data",
        innovationCompareUI("innovation_compare", data = innovation_data)
      ),
      tabItem(
        tabName = "docs",

      )
    )
  )
}

innovation_ui <- function() {
  dashboardPage(title = "Regional Innovation",
                header = header(),
                sidebar = sidebar(),
                body = body(),
                footer = dashboardFooter(left = "Australian Industrial Transformation Institute", right = "AURIN")
  )
}
