#' @rawNamespace import(shiny, except = c(column, tabsetPanel, insertTab, actionButton))
#' @rawNamespace import(shinyWidgets, except = c(progressBar))
#' @import bs4Dash
#' @import fresh
#' @import strayr
#' @import leaflet
#' @import mapview
#' @import sf
#' @import ggplot2
#' @import dplyr
#' @import DT
#' @import stringr
#' @import aititheme
#' @import pkgload
#'
#' @export innovation_app
# Preamble ----------------------------------------------------------------

#Install PhantomJS - for downloading maps
if (is.null(suppressMessages(webshot:::find_phantom()))) { webshot::install_phantomjs() }


innovation_server <- function(input, output, session) {

  innovationMapServer("innovation_map", data = innovation_data)
  innovationCompareServer("innovation_compare", data = innovation_data)

}


innovation_app <- function() {
  shinyApp(ui = innovation_ui(), server = innovation_server)
}
