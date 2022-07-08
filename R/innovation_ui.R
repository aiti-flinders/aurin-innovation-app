innovation_ui <- function() {
  navbarPage("Regional Innovation",
             tabPanel(title = "Regional Innovation",
                        innovationMapUI("innovation_map", data = innovation_data)
             ),
             tabPanel(title = "Compare Regions",
                        innovationCompareUI("innovation_compare", data = innovation_data)
             )
  )
}
