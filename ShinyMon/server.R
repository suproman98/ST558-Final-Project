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
library(tidyverse)
pokemon <- read_csv("pokemon.csv")
pokemon <- pokemon %>% mutate(catch_rate_pct = round((catch_rate/255)*100)) %>% select(-catch_rate)

function(input, output, session) {
    
    output$title <- renderUI({
        text <- paste("Relationship between ", input$attr1, " and ", input$attr2, " from Generation ", input$gen, " Pokemon", sep = "")
        h3(text)
        tools::toTitleCase(text)
    })
    
    
    output$corrplot <- renderPlot({
        pokemon %>%
            filter(generation == input$gen) %>%
            select(attr1 = input$attr1, attr2 = input$attr2, name = name, type = type_1) %>%
            ggplot(aes(attr1, attr2))+
            geom_point(aes(text=name, color=type), size = 2, alpha = 0.7)+
            geom_smooth(method = "lm", size=1.5)+
            labs(x=paste(input$attr1), y=paste(input$attr2))
    })
    
    output$barplot <- renderPlot({
        pokemon %>% 
            filter(generation == input$gen) %>%
            select(Type = type_1, Catch_Rate_Pct = catch_rate_pct) %>%
            ggplot(aes(Type, Catch_Rate_Pct, fill=Type))+
            geom_boxplot()+
            coord_flip()
    })
    
    
    pokesum <- reactive({
        req(input$column)
        pokemon %>%
            select(input$column) %>%
            summary(input$column) %>%
            as.data.frame() %>%
            separate(Freq, c("Stat", "Value"), sep=":") %>%
            pivot_wider(names_from=Stat, values_from=Value)
    })
    output$table <- renderDataTable(pokesum())
    

    
    
    
    
        ### Subset data
        pokesub <- reactive({ 
            subpoke <- subset(pokemon, type_1 %in% input$types & generation %in% input$generations & growth_rate %in% input$growth & status %in% input&status)
        return(subpoke)
        })
    
        output$subset <- renderDataTable({pokesub()})
        output$download <- downloadHandler(
            filename = function(){"PokemonData.csv"}, 
            content = function(fname){
                write.csv(dataset(), fname)
            }
        )
}
