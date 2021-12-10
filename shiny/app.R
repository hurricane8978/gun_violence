library(shiny)
library(tidyverse)
library(flexdashboard)
library(viridis)
library(plotly)
library(dplyr)
library(rjson)
library(shinydashboard)

unemploy = read_csv("Unemployment_renamed.csv") 
clean_unemploy =  
  dplyr::rename(unemploy, state = series_id) %>% 
  mutate(label = fct_inorder(label))
state = clean_unemploy %>% distinct(state) %>% pull()

ui = fluidPage(
  titlePanel(title = "dashboard"),
  sidebarPanel(
    selectInput(
      "state_select", 
      "Select state",
      choices = state, selected = "Alabama")
  ),
  mainPanel(
   plotOutput("lineplot")
  )
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
}

shinyApp(ui, server)