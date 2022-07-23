#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(dplyr)
library(ggplot2)
library(tidyr)
library(dplyr)
library(shiny)
library(DT)
library(stringr)
pokemon <- read_csv("pokemon.csv")

function(input, output, session) {
    
    output$title <- renderUI({
        text <- paste("Relationship between ", input$attr1, " and ", input$attr2, " from Generation ", input$gen, " Pokemon", sep = "")
        str_to_title(text)
        tools::toTitleCase(text)
    })
    
    
    output$plot <- renderPlot({
        pokemon %>%
            filter(generation == input$gen) %>%
            select(attr1 = input$attr1, attr2 = input$attr2, len = input$attr1, name = name) %>%
            ggplot(aes(attr1, attr2))+
            geom_point(aes(color=len, text=name), size = 2, alpha = 0.7)+
            geom_smooth(method = "lm", size=1.5)+
            theme_light()+
            labs(x=paste(input$attr1), y=paste(input$attr2))
    })
    
    output$table <- renderDataTable({pokemon})
    
}
