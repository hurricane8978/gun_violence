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

ui = navbarPage("My Application",
    tabPanel("unemployment rate",
               sidebarPanel(
                 selectInput(
                   "state_select", 
                   "Select state",
                   choices = state, selected = "Alabama")
               ),
             mainPanel(plotOutput("lineplot"))
                                   ),
    tabPanel("US map for mass shooting",
                sidebarPanel(
                  sliderInput(
                    "year_select", 
                    "Select year",
                    min = 2018, max = 2021, value = 2020)
                ),
             mainPanel(plotOutput("usplot"))),
                          tabPanel("Component 3")
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
    
    output$usplot = renderPlot({
      new %>% 
        filter(year == input$year_select) %>% 
        plot_usmap(data = ., values = "number") +
        labs(fill = 'number of mass shooting') + 
        scale_fill_gradientn(colours=rev(heat.colors(20)), na.value = 0,guide = guide_colourbar(barwidth = 0.5, barheight = 10)) +
        theme(legend.position = "right")
      
                                                     
    })
}

shinyApp(ui, server)