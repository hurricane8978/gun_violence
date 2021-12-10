library(shiny)
library(tidyverse)
library(shinydashboard)
library(viridis)
library(usmap)
library(maps)

unemploy = read_csv("Unemployment_renamed.csv") 
clean_unemploy =  
  dplyr::rename(unemploy, state = series_id) %>% 
  mutate(label = fct_inorder(label))
state = clean_unemploy %>% distinct(state) %>% pull()

gun = read_csv("mass shootings(all years).csv") %>% 
  janitor::clean_names() %>% 
  separate(incident_date, c("date", "month", "year"), sep = "-" ) %>% 
  mutate(year = as.numeric(year) + 2000) %>% 
  filter(year != 2017)

new = gun %>% 
  group_by(state,year) %>% 
  summarize(number = n()) 


gun_state = gun %>%
  group_by(year, state) %>% 
  summarise(number_of_incident = n(), 
            sum_killed = sum(number_killed), 
            sum_injured = sum(number_injured)) %>% 
  replace(is.na(.), "District of Columbia") 

url <- 'https://eric.clst.org/assets/wiki/uploads/Stuff/gz_2010_us_040_00_20m.json'
states <- fromJSON(file=url)




ui = navbarPage("My Application",
    tabPanel("US map for mass shooting",
                         sidebarPanel(
                           sliderInput(
                             "year_select", 
                             "Select year",
                             min = 2018, max = 2021, value = 2020)
                         ),
                         mainPanel(plotlyOutput("usplot"))), 
    
    tabPanel("unemployment rate",
               sidebarPanel(
                 selectInput(
                   "state_select", 
                   "Select state",
                   choices = state, selected = "Alabama")
               ),
             mainPanel(plotOutput("lineplot")))
)


server = function(input,output){
    
    output$lineplot = renderPlot({
    clean_unemploy %>% 
    filter(state == input$state_select) %>% 
    ggplot(aes(x = label, y = value, group = state)) +
    geom_line()+
    theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1))+
    labs(
      title = "Unemployment Rate by month from 2017-2021",
      x = "Time",
      y = "Unemployment rate"
    )
  })
    
    output$usplot = renderPlotly({
    gun_state %>% 
      filter(year == input$year_select) %>% 
      mutate(text_label = str_c("Number of killed: ", sum_killed, '\nNumber of injured: ', sum_injured)) %>%
      plot_ly() %>% 
      add_trace(
        type = "choroplethmapbox",
        featureidkey = "properties.NAME", 
        geojson = states,
        locations= ~state,
        z=~number_of_incident,
        colorscale="Viridis", 
        # add text
        text = ~text_label,
        marker = list(line = list(
          width = 0),
          opacity = 0.75
        )
      ) %>% 
      layout(
        mapbox = list(
          style="carto-positron",
          zoom = 2,
          center = list(lon = -95.71, lat = 37.09)), 
        title = "US Map of Mass Shooting",
        legend = list(x = 100, y = 0.5)) %>%   
      colorbar(title = "Mass Shooting")
  })
}

shinyApp(ui, server)