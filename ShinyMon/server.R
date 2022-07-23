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
    
    output$catsum <- renderDataTable({
        pokemon %>%
            filter(type_1 == input$type1) %>%
            filter(generation == input$gen) %>%
            select(name = name, total = total_points, catchpct = catch_rate)
    })
    
    
    output$numsum <- renderDataTable({
        if(input$type1){
            if(input$type2){
                pokemon %>%
                    select(name, generation, type_1, type_2, hp, attack, defense, sp_attack, sp_defense, speed, total_points)
                    filter(type_1 == input$type1, type_2 == input$type2) %>%
                    summarise(mean = avg(total_points), median = median(total_points), sd = sd(total_points))
            } else {
                pokemon %>%
                    filter(type_1 == input$type1) %>%
                    summarise(mean = avg(total_points), median = median(total_points), sd = sd(total_points))
            }
        } else {
            print("Select a valid typing")
        }
    })
    
    
    
    
        ### Subset data
        pokesub <- reactive({ 
            a <- subset(pokemon, type_1 %in% input$type)
        return(a)
        })
    
        output$subset <- renderDataTable({pokesub()})
        output$download <- downloadHandler(
            filename = function(){"PokemonData.csv"}, 
            content = function(fname){
                write.csv(dataset(), fname)
            }
        )
}
