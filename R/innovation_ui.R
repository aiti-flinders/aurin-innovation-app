innovation_ui <- function() {
  navbarPage("Regional Innovation",
             navbarMenu(title = "Regional Innovation",
                        innovationMapUI("innovation_map", data = innovation_data),
                        innovationCompareUI("innovation_compare", data = innovation_data)
             ),
             navbarMenu("More",
                        tabPanel("Something MORE",
                                 includeHTML("https://aiti-flinders.github.io/aurin-innovation/articles/industrial_growth_opportunities.html"))
             )
  )
}
