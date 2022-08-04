innovation_server <- function(input, output, session) {


  map_data <- tibble(bind_rows(
    aurininnovation:::sa2_2011 %>% select(sa2_name, sa3_name, sa4_name, gcc_name, state_name) %>% mutate(year = 2011),
    aurininnovation:::sa2_2016 %>% select(sa2_name, sa3_name, sa4_name, gcc_name, state_name) %>% mutate(year = 2016)
  ))

  innovation_data <- regional_innovation %>%
    full_join(map_data, by = c("year", "sa2_name"))

  innovationMapServer("innovation_map", data = innovation_data)
  innovationCompareServer("innovation_compare", data = innovation_data)


}
